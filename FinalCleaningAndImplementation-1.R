library(ggplot2) # for data visualization
library(stringr) #extracting string patterns
library(Metrics) # rmse
library(dplyr) # load this in last so plyr doens't overlap it
library(caret) # one hot encoding
library(scales) # plotting $$
library(e1071) # skewness
library(corrplot) # correlation plot
library(RMySQL)




mydb = dbConnect(MySQL(), user='root', password='root', dbname='new_schema', host='localhost')

# Selecting data from my table in database
r = dbSendQuery(mydb, "select * from train")
mydata = fetch(r, n=-1)
train = mydata
# train <- read.csv('train.csv', stringsAsFactors = FALSE)

r = dbSendQuery(mydb, "select * from test")
mydata = fetch(r, n=-1)
test = mydata


# test <- read.csv('test.csv', stringsAsFactors = FALSE)

# combining both the training  and testing datasets so that we can remove missing values from both the datasets together.
housing <- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(housing)


#Removing all the NA's so that we can better train our model. 


#Finding total number of columns with missing values in our data set.
na.cols <- which(colSums(is.na(housing)) > 0)
sort(colSums(sapply(housing[na.cols], is.na)), decreasing = TRUE)

paste('There are', length(na.cols), 'columns with missing values')

#function for plotting categoric data for easier data visualization
plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(housing[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}





#Checking for all the columns g=having missing values.
# KitchenQual: Kitchen quality
# With only 1 missing value for KitchenQual and Electrical each we can fill in the missing value with the most frequent value from each column.

plot.categoric('KitchenQual', housing)


housing$KitchenQual[is.na(housing$KitchenQual)] = 'TA'

plot.categoric('Electrical', housing)


housing$Electrical[is.na(housing$Electrical)] = 'SBrkr'



#Pool quality

plot.categoric('PoolQC', housing)
housing[(housing$PoolArea > 0) & is.na(housing$PoolQC),c('PoolQC','PoolArea')]


housing[,c('PoolQC','PoolArea')] %>%
  group_by(PoolQC) %>%
  summarise(mean = mean(PoolArea), counts = n()) 


housing[2421,'PoolQC'] = 'Ex'
housing[2504,'PoolQC'] = 'Ex'
housing[2600,'PoolQC'] = 'Fa'
housing$PoolQC[is.na(housing$PoolQC)] = 'None'

#GarageYrBlt: Year garage was built
#Giving the same year as house built year for missing values in Garage Built year.
length(which(housing$GarageYrBlt == housing$YearBuilt))
idx <- which(is.na(housing$GarageYrBlt))
housing[idx, 'GarageYrBlt'] <- housing[idx, 'YearBuilt']

garage.cols <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
housing[is.na(housing$GarageCond),garage.cols]

#Only one house who had NA's in their garage columns had an area greateer than 0. We can fill this house in manually and set the rest of the houses NA's to 0.
 
# For the house with GarageArea = 360 and GarageCars = 1, but NA's in the other columns, we can use the most frequent values for each columns from houses with a similar area and car count.

idx <- which(((housing$GarageArea < 370) & (housing$GarageArea > 350)) & (housing$GarageCars == 1))

names(sapply(housing[idx, garage.cols], function(x) sort(table(x), decreasing=TRUE)[1]))


housing[2127,'GarageQual'] = 'TA'
housing[2127, 'GarageFinish'] = 'Unf'
housing[2127, 'GarageCond'] = 'TA'

 
# fill in any missing numeric values with 0 and categoric with 'None' since these houses recorded having 0 area and 0 cars in their garage.
for (col in garage.cols){
  if (sapply(housing[col], is.numeric) == TRUE){
    housing[sapply(housing[col], is.na), col] = 0
  }
  else{
    housing[sapply(housing[col], is.na), col] = 'None'
  }
}





#Basement related attributes.
bsmt.cols <- names(housing)[sapply(names(housing), function(x) str_detect(x, 'Bsmt'))]

b = housing[is.na(housing$BsmtExposure),bsmt.cols]


plot.categoric('BsmtExposure', housing)

housing[c(949, 1488, 2349), 'BsmtExposure'] = 'No'

for (col in bsmt.cols){
  if (sapply(housing[col], is.numeric) == TRUE){
    housing[sapply(housing[col], is.na),col] = 0
  }
  else{
    housing[sapply(housing[col],is.na),col] = 'None'
  }
}


 
# Exterior1st: Exterior covering on house
# Exterior2nd: Exterior covering on house (if more than one material)


#plot.categoric(c('Exterior1st', 'Exterior2nd'), housing)
idx <- which(is.na(housing$Exterior1st) | is.na(housing$Exterior2nd))
housing[idx,c('Exterior1st', 'Exterior2nd')]


housing$Exterior1st[is.na(housing$Exterior1st)] = 'Other'
housing$Exterior2nd[is.na(housing$Exterior2nd)] = 'Other'


# SaleType, functional and utilities


# SaleType, Functional and Utilities have less than 3 missing values. 
#For SaleType we can compare with  the SaleCondition of the house  use a contingency table to see which SaleType and SaleCondition overlap together the most.
 

plot.categoric('SaleType', housing)
housing[is.na(housing$SaleType),c('SaleCondition')]
table(housing$SaleCondition, housing$SaleType)

# Most houses with a SaleCondition of 'Normal' almost all have a SaleType of 'WD'. 
#We'll replace the missing value accordingly.


housing$SaleType[is.na(housing$SaleType)] = 'WD'

plot.categoric('Functional', housing)


housing$Functional[is.na(housing$Functional)] = 'Typ'

 
# Utilities only has 1 value for NoSeWa and the rest AllPub. 
#We can drop this feature from our dataset as the house with 'NoSeWa' is from our training set and will have won't help with any predictive modelling

plot.categoric('Utilities', housing)


which(housing$Utilities == 'NoSeWa') # in the training data set


col.drops <- c('Utilities')
housing <- housing[,!names(housing) %in% c('Utilities')]               


 
# MSZoning & MSSubClass: 

# There are only 4 missing values for MSZoning. 
#We can see what the subclass is for the houses with missing values for Zoning.


housing[is.na(housing$MSZoning),c('MSZoning','MSSubClass')]


plot.categoric('MSZoning', housing)


table(housing$MSZoning, housing$MSSubClass)

#For Subclasses with 20 'RL' has the largest frequency, however, for Subclasses with 30 and 70 'RM' has the most frequency. 
#We will fill the missing values accordingly.


housing$MSZoning[c(2217, 2905)] = 'RL'
housing$MSZoning[c(1916, 2251)] = 'RM'

 
# MasVnrType & MasVnrArea

 
# There are 23 missing values for MasVnrArea and 24 for MasVnrType. 
#We can see if both missing values come from the same houses



housing[(is.na(housing$MasVnrType)) | (is.na(housing$MasVnrArea)), c('MasVnrType', 'MasVnrArea')]

 
# All but one house has missing values for both columns. 
#For houses with NA's on both columns we can fill 0 for the area and None for the type since they likely do not have a masonry veneer. For the house with a MasVnrArea of 198 but NA for MasVnrType we can record the median areas for each type and see which type is closest to 198


na.omit(housing[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)


#plot.categoric('MasVnrType', housing)

housing[2611, 'MasVnrType'] = 'BrkFace'


# The areas we can replace with 0 and types can be replaced with 'None'


housing$MasVnrType[is.na(housing$MasVnrType)] = 'None'
housing$MasVnrArea[is.na(housing$MasVnrArea)] = 0




 
# LotFrontage
# There are 486 missing values for LotFrontage, 
#which is quite a lot of values to fill and we can't just replace these with 0. We're given that "LotFrontage: Linear feet of street connected to property." The area of each street connected to the house property is most likely going to have a similar area to other houses in its neighborhood. We can group by each neighborhood and take the median of each LotFrontage and fill the missing values of each LotFrontage based on what neighborhood the house comes from.


housing['Nbrh.factor'] <- factor(housing$Neighborhood, levels = unique(housing$Neighborhood))

lot.by.nbrh <- housing[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh


idx = which(is.na(housing$LotFrontage))

for (i in idx){
  lot.median <- lot.by.nbrh[lot.by.nbrh == housing$Neighborhood[i],'median']
  housing[i,'LotFrontage'] <- lot.median[[1]]
}

 
# Fence

# We can replace any missing vlues for Fence and MiscFeature with 'None' as they probably don't have this feature with their property.


plot.categoric('Fence', housing)


housing$Fence[is.na(housing$Fence)] = 'None'


table(housing$MiscFeature)

housing$MiscFeature[is.na(housing$MiscFeature)] = 'None'

# FirePlace

plot.categoric('FireplaceQu', housing)


which((housing$Fireplaces > 0) & (is.na(housing$FireplaceQu)))

 
# All the houses that have missing values did not record having any fireplaces. We can replace the NA's with 'None' since these houses don't have any fireplaces at all.


housing$FireplaceQu[is.na(housing$FireplaceQu)] = 'None'


 
# Alley
# There are 2721 missing values for Alley and only 2 potential options - Grvl and Pave. We can fill 'None' for any of the houses with NA's as these houses must not have any type of alley access.



plot.categoric('Alley', housing)




housing$Alley[is.na(housing$Alley)] = 'None'



paste('There are', sum(sapply(housing, is.na)), 'missing values left')






#Converting all the attributes to factor



#Checking for numerical and integer attributes and binning all of them for better learning of our model. 

housing1 = housing

for (i in 1:79) {
  if (class(housing1[,i]) == "integer"){
    housing1[,i] = cut(housing1[,i], 5, include.lowest=TRUE)
  } 
}


for (i in 1:79) {
  if (class(housing1[,i]) == "numeric"){
    housing1[,i] = cut(housing1[,i], 5, include.lowest=TRUE)
  } 
}


#Converting all character attributes to factor, as there is only one attribute with maximum unique values as 25.
for (i in 1:79) {
  if (class(housing1[,i]) == "character"){
    housing1[,i] = as.factor(housing1[,i])
  } 
}




#Separating all the train and test data.
train_1<-housing1[1:1460,]
test_1<-housing1[1461:2919,]

#Binning SalesPrice data
SalesPriceBinned<-cut(log(train$SalePrice), 5, include.lowest=TRUE)
#Adding class attribute to our training set
train_1$SalePrice <- SalesPriceBinned













#Writing functions for ID3 training and prediction and using it for prediction of our data.


#Creating a tree structure containing root and edges using list for storing edges of the tree.
tree <- function(root, edges) {
  structure(list(root=root, edges=edges), class='tree')
}



# Creating a leaf structure for storing the terminal class lables for our decision tree.
leaf <- function(root) {
  structure(list(root=as.character(root)), class='leaf')
}



#Calculating Entropy function.
entropy <- function(S) {
  if (!is.factor(S)) S <- as.factor(S)
  
  p <- prop.table(table(S))
  
  -sum(sapply(levels(S),
              function(name) p[name] * log2(p[name]))
  )
}





# Defining our ID3 function.
#	Recursively builds a tree and finally gives our decision tree.
ID3 <- function(data, target_attr,
                attributes=setdiff(names(data), target_attr)) {
  
  #BAse cases:  
  # If there are no attributes left to classify with return the most common class.
  if (length(attributes) <= 0) {
    
    return(leaf(most.frq(data[, target_attr])))
  }
  
  # If there is only one class left in our data -> return a leaf with that classification.
  
  if (length(unique(data[, target_attr])) == 1) {
    
    return(leaf(unique(data[, target_attr])[1]))
  }
  
  # Select the best attribute based on the minimum entropy
  best_attr <- attributes[which.min(sapply(attributes, entropy))]
  
  
  # Create the set of remaining attributes
  rem_attrs <- setdiff(attributes, best_attr)
  
  # Split the data into groups based on levels of the best_attr
  split_data <- split(data, data[, best_attr])
  # Recursively edge to create the tree.
  edges <- lapply(seq_along(split_data), function(i) {
    # The name of the edges
    name <- names(split_data)[i]
    # Get edge data
    edge <- split_data[[i]]
    
    # If there is no data, return the most frequent class in the parent, otherwise start over with new edge data.
    if (nrow(edge) == 0) leaf(most.frq(data[, target_attr]))
    else ID3(edge[, union(target_attr, rem_attrs), drop=FALSE],
             target_attr,
             rem_attrs)
  })
  names(edges) <- names(split_data)
  
  id3_tree <- tree(root=best_attr, edges=edges)
  id3_tree
}

# PREDICTION FUNCTION

predict <- function(test_obs, id3_tree) {
  traverse <- function(obs, work_tree) {
    if (class(work_tree) == 'leaf') work_tree$root
    else {
      var <- work_tree$root
      new_tree <- work_tree$edges[[as.character(obs[var])]]
      traverse(obs, new_tree)
    }
  }
  apply(test_obs, 1, traverse, work_tree=id3_tree)
}


most.frq <- function(nbr.class, nbr.distance) {
  uniq <- unique(nbr.class)
  uniq[which.max(tabulate(match(nbr.class, uniq)))]
}





#Creating training set and testing set from training data
set.seed(3033)
intrain <- createDataPartition(y =train_1$SalePrice, p= 0.7, list = FALSE)
training <- train_1[intrain,]
testing <- train_1[-intrain,]

#Using ID3 function for training on our training data.
decision_tree <- ID3(training, 'SalePrice')

#Predicting values for our testing data using the decision tree.
pi = predict(testing, decision_tree)

c = 0
# NotEqual = c()


#Calculating the accuracy and error of our prediction model on housing data set.
for (i in 1:nrow(testing)) {
  if (testing[i,80]==pi[i]) {
    c = c+1
    
  }
  
}
print(c)

#Calculating Accuracy
Accuracy = c/nrow(testing)
print(Accuracy)


#Calculating error rate.
Error_rate = (nrow(testing)-c)/nrow(testing)
print(Error_rate)







