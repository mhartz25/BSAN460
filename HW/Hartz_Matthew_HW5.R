### BSAN 460 -- Business Analytics Senior Project
### Spring 2020
### Week 7 -- Decision Tree, Random Forest, Gradient Boosting for Binary Classification & Model Tuning


# Install packagaes if they are not already installed on the PC (uncomment the rows below)
#install.packages("dplyr")
#install.packages("pROC")
#install.packages("MLmetrics")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("gbm")

# Load packages in R
library(dplyr)
library(pROC) # this package is used to calculate AUC (performance measure for fitness of probabilities)
library(MLmetrics)
library(rpart)  # use for decision tree
library(randomForest) # use for random forest
library(gbm) # use for gradient boosting
library(caret)

# set the working directory; ! if you copy paste the path, then change the \ to /
setwd("D:/Teaching Courses/BSAN 460 -- Capstone -- Spring 2020")

# let's read the file again from the folder; now, there is no need to write the entire path
cust_churn <- read.csv("Customer Churn.csv")


# check the structure of the data frame
# str() function tells that the data has 5298 observations and 18 variables
# variables are stored as factors, integers and numeric
# notice that ToalCharges is stored as factor, but it should in fact be stored as numeric
str(cust_churn)

# check the summary of the data frame
# notice that there are some columns that have NAs (ContractType)
# some columns (Education, ServiceDetails) have multiple levels, some levels only having few instances 
summary(cust_churn)

# ! Note that there are more customers that don't churn than customers that do churn
# 3,974 don't churn; 1,324 churn --> 75% of the customers don't churn --> imbalanced dataset

# let's look at the ServiceDetails colum
# determine how many times each level in this column shows up
# c("0", "No phone service", "DSL", "Fiber optic", "DSL") --> these categories only show up a few times
cust_churn %>% count(ServiceDetails)
# ServiceDetails column is stored at factor
# to do operations on this column, we should change it to character format
cust_churn <- cust_churn %>% mutate(ServiceDetails = as.character(ServiceDetails))
# change the 4 categories that appear few times to "Other"; this operation won't work on data that is stored as factor
cust_churn <- cust_churn %>% mutate(ServiceDetails = ifelse(ServiceDetails %in% c("0", "No phone service", "DSL", "Fiber optic", "DSL"), 
                                                            "Other", ServiceDetails))
# check again how many times each category shows up
cust_churn %>% count(ServiceDetails)


# check how many times each category within TypeOfService shows up
# no need to create groupings here; 
# in some real world applications, making groupings out of a column such as this one could make sense
cust_churn %>% count(TypeOfService)


# check how many times each category within Education shows up
# do the same operations as we did for the ServiceDetails column
cust_churn %>% count(Education)
cust_churn <- cust_churn %>% mutate(Education = as.character(Education),
                                    Education = ifelse(Education %in% c("0", "Other"), "Other", Education))
cust_churn %>% count(Education)

# TotalCharges column is stored as factor, but it in fact contains numbers, so let's change it to numeric
# ! Note that R doesn't automatically transform from factor to numeric
# you should first transform the column to character and then to numeric: as.numeric(as.character(myColumn))
str(cust_churn)
cust_churn <- cust_churn %>% mutate(TotalCharges = as.numeric(as.character(TotalCharges)))

# replace NAs
# we got a warning the NAs were introduced in the data, so let's check the summary of the data
summary(cust_churn)
# TotalCharges column has 2 instances of NA, so let's display the rows that have NAs
# is.na(cust_churn$TotalCharges) check each instance of the TotalCharges colum to see if it is NA.
# If it is, then it will say TRUE, otherwise it will say FALSE; 
# this chunk of code creates a boolean vector with TRUE and FALSE
# take this boolean vector and filter out the rows of the date: cust_churn %>% filter(boolean vector)
# the boolean_vector sits before the comma because it is used to filter the rows
cust_churn %>% filter(is.na(TotalCharges))
# in the cust_churn data, replace the NAs in the TotalCharge column with the mean of the column
# mean(TotalCharges) would calculate the mean of the TotalCharges column, but since it contains NAs 
# in it, it will return NA
# mean(TotalCharges, na.rm = TRUE) calculated the mean of all the instances that don's have NAs
cust_churn <- cust_churn %>% mutate(TotalCharges = ifelse(is.na(TotalCharges) == TRUE, mean(TotalCharges, na.rm = TRUE), TotalCharges))


# remove columns
# Country and State columns only have 2 levels and most of the instances come from only one of the levels
cust_churn <- cust_churn %>% select(-Country, -State)


# replace NAs in the ContractType column
cust_churn %>% count(ContractType) # check how many times each category shows up
cust_churn <- cust_churn %>% mutate(ContractType = as.character(ContractType),  # this column is initially a factor, so change to character format first
                                    ContractType = ifelse(is.na(ContractType), "Month-to-month", ContractType)) # change the NA to "Other"
cust_churn %>% count(ContractType) # check how many times each category shows up

# the DOC column has 41030 in each instance, so this column is useless; let's remove it
summary(cust_churn$DOC)
cust_churn <- cust_churn %>% select(-DOC)

# transform the character columns into factors
# models don't take in characted column, so we should transform all character columns to factor
str(cust_churn) # Education, ContractType, ServiceDetails are stored as characters
cust_churn <- cust_churn %>% mutate_if(is.character, as.factor) # this changes all the character variables to factor format
str(cust_churn)




# we now have 5298 observations and 15 variables
# out of these 15 variables, the first one, CustomerID, is a unique key of the data
# CustomerID will therefore not be used in the model
# Churn column is the dependent variable
# all the other columns are independent variables
# notice that all categorical columns (including the dependent variable) are stored as factors
# all continuous variables are stored as integer or numeric
str(cust_churn)



# when building a model, it is important avoid overfitting, which means that the model should 
# generalize well on new data
# to check that the model doesn't overfit, we should create a training and a testing data set
# usually, the training set contains 70% of the instances, while the testing set contains 30% of the instances
# steps:
# build the model on the training set
# predict the values on the training set
# predict the values on the testing set
# compare the performance measures on the training and testing set
# if the model is not overfitting and generalizing well, the performance measures should be simmilar
# on both the training and testing sets
# if the model is overfitting, then you could do one of the following:
# collect more data
# use less independent variables
# modify the model hyper-parameters

# split the data into train and test data
set.seed(123) # set a seed so that every time we generate a random sample we get the same one (reproduce results)
# train_ind is a vector that contains indeces (row numbers) of 70% of the instances in the data set
# nrow(cust_churn) gives the number of rows in the cust_churn data set
# size = round(.7 * nrow(cust_churn)) gives the number of sample rows we need (70% of the total number of rows)
# replace = F indicates that the sampling is not with replacement
train_ind <- sample(nrow(cust_churn), size = round(.7 * nrow(cust_churn)), replace = F)
head(train_ind)
train <- cust_churn[train_ind, ]
head(train)
test <- cust_churn[-train_ind, ]
head(test)


#################################################
################# Decision Tree #################
#################################################
# https://cran.r-project.org/web/packages/rpart/rpart.pdf
# when working with decision trees, there are a few hyperparameters that we could tune:
  # Minimum samples for a node split 
  # Minimum samples for a terminal node (leaf)
  # Maximum depth of tree
# create the random commbination of hyper-parameters (same as random search)

# build the tree model
set.seed(23)
tree_model <- rpart(Churn ~ .,  data = train %>% select(-CustomerID),
                    control = rpart.control(minsplit = 20, # Minimum samples for a node split 
                                            minbucket = 10, # Minimum samples for a terminal node (leaf)
                                            maxdepth = 20)) # Maximum depth of tree


# displays how the splits are created at each node
# variables with high importance show at the top of the tree
# the model also provides a Variable Importance value 
summary(tree_model)

# plot the decision tree
# use.n = TRUE displays the number of correctly predicted at the nodes
# all = TRUE labels all nodes
plot(tree_model)
text(tree_model, use.n = TRUE, all = TRUE) 

##### predict on train
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_train_tree <- as.data.frame(predict(tree_model, train, type='prob'))
head(pred_train_tree)

# AUC on train
roc(train$Churn, pred_train_tree$Yes, percent = TRUE, plot = TRUE) 

##### predict on test
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_test_tree <- as.data.frame(predict(tree_model, test, type='prob'))
head(pred_test_tree)

# AUC on test
roc(test$Churn, pred_test_tree$Yes, percent = TRUE, plot = TRUE) 


#############################################################################
################# Decision Tree with Hyper-Parameter Tuning #################
#############################################################################
# Instead of manually checking different combinations of hyper-parameters, 
  # let's create a grid of the hyper-parameters
# Some package hvae Grid Search and Random Search pre-build, so you wouldn't have to create your own
# However, if you use a package that doesn't have the grids built in, then you can create your own 
  # by using a loop.
# Having your own grid would also allow you to have more flexibility: 
  #  create your own performance measure and sort results

# preparing a table with random parameters values in order to  find the right combination that 
  # maximized the desired performance measure
set.seed(123)
table_parameters <- data.frame(minsplit_para = floor(runif(6, 1, 500)), # floor(runif(a, b, c)) gives a ineteger numbers between b and c
                               minbucket_para = floor(runif(6, 5, 50)),
                               maxdepth_para = floor(runif(6, 10, 30)))
table_parameters # this table contains 6 rows, each row representing a combination of hyper-parameters
AUC_values <- data.frame(AUC_train = numeric(), AUC_test = numeric()) # empty data frame
AUC_values # this will be used to keep track of the AUC on the train and test data at each iteration

# this loop will go through each hyper-parameter combination (each row of the table_parameters)
for(para_comb in 1:nrow(table_parameters)){
  # set.seed(1) # if needed
  # build the tree model
  tree_model <- rpart(Churn ~ .,  data = train %>% select(-CustomerID),
                      control = rpart.control(minsplit = table_parameters[para_comb, "minsplit_para"], # Minimum samples for a node split 
                                              minbucket = table_parameters[para_comb, "minbucket_para"], # Minimum samples for a terminal node (leaf)
                                              maxdepth = table_parameters[para_comb, "maxdepth_para"])) # Maximum depth of tree
  
  # predict on the train data
  pred_train_tree <- as.data.frame(predict(tree_model, train, type='prob'))
  # AUC on train
  auc_train <- roc(train$Churn, pred_train_tree$Yes, percent = TRUE, plot = TRUE)
  
  # predict on test data
  pred_test_tree <- as.data.frame(predict(tree_model, test, type='prob'))
  # AUC on test
  auc_test <- roc(test$Churn, pred_test_tree$Yes, percent = TRUE, plot = TRUE) 
  
  # store the AUC of the train and test data in a table
  AUC_values[para_comb, ] <- c(auc_train$auc, auc_test$auc)
}

table_parameters
AUC_values

tree_model

#################################################
################# Random Forest #################
#################################################
# https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
# when working with random forest, there are a few hyperparameters that we could tune:
  # Minimum samples for a node split 
  # Minimum samples for a terminal node (leaf)
  # Maximum depth of tree
  # Sampling a % of rows
  # Sampling a % of columns 
  # Number of trees


set.seed(123) # random forest uses random numbers, so we should use a seed to recreate the results
random_forest_model <- randomForest(Churn ~ ., data = train %>% select(-CustomerID),
                                    # choose some of the hyper-parameters included in the randomForest package
                                    nodesize = 100, # minimum number of elements at the terminal nodes
                                    sampsize = 1000, # number of element drawn from the data for each tree
                                    mtry = 4, # number of predictors used at each split
                                    ntree = 100) # number of trees built
                                    
# check details about the random forest model
random_forest_model


varImp(random_forest_model)
varImpPlot(random_forest_model)


##### predict on train
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_train_random_forest <- as.data.frame(predict(random_forest_model, train, type='prob'))
head(pred_train_random_forest)

# AUC on train
roc(train$Churn, pred_train_random_forest$Yes, percent = TRUE, plot = TRUE) 

##### predict on test
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_test_random_forest <- as.data.frame(predict(random_forest_model, test, type='prob'))
head(pred_test_random_forest)

# AUC on test
roc(test$Churn, pred_test_random_forest$Yes, percent = TRUE, plot = TRUE) 



#################################################
############### Gradient Boosting ###############
#################################################
# https://cran.r-project.org/web/packages/gbm/gbm.pdf
# when working with gradient boosting, there are a few hyperparameters that we could tune:
  # Minimum samples for a node split 
  # Minimum samples for a terminal node (leaf)
  # Maximum depth of tree
  # Sampling a % of rows
  # Sampling a % of columns 
  # Number of trees
  # Shrinkage / learning rate

# change the Yes and No in the Churn column to 0 and 1 (gbm requirement beacause of package)
train_gbm <- train %>% mutate(Churn = ifelse(Churn == "Yes", 1, 0))
test_gbm <- test %>% mutate(Churn = ifelse(Churn == "Yes", 1, 0))

set.seed(123) # gbm uses random numbers, so we should use a seed to recreate the results
gbm_model <- gbm(Churn ~ ., data = train_gbm %>% select(-CustomerID), 
                 distribution = "bernoulli",
                 n.trees = 100, # number of trees being fit on the data
                 shrinkage = 0.05, # shrinkage / learning rate
                 interaction.depth = 3, # maximum depth of the tree
                 bag.fraction = 0.5, # % of the rows that will be used to train a decision tree
                 n.minobsinnode = 10, # minimum number of observations at each terminal node
                 # cv.folds = 10, here you could choose to do k-fold cross validation
                 verbose = TRUE)

# check the important variables of the model
summary(gbm_model)

##### predict on train_gbm
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_train_gbm <- predict(gbm_model, train_gbm, type = "response", n.trees = 100) # it sometimes gives issues if you don't specify the number of trees you wish to use in the prediction
head(pred_train_gbm)

# AUC on train_gbm
roc(train_gbm$Churn, pred_train_gbm, percent = TRUE, plot = TRUE) 

##### predict on test_gbm
# this provides probabilities for each category. The values add up to 1 at the row level
# saving as a data frame so we can easily subset it
pred_test_gbm <- predict(gbm_model, test_gbm, type='response', n.trees = 100)
head(pred_test_gbm)

# AUC on test_gbm
roc(test_gbm$Churn, pred_test_gbm, percent = TRUE, plot = TRUE) 

rm(list=ls())
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")


############################
### 1. Homework Exercise ### # What is the difference between decision tree, random forest and gradient boosting?
############################

##All 3 of those methods have to do with decision trees in different ways. Random Forest and Gradient Boosts are both
#ensemble methods that approach decision trees from a different angle. Random Forest looks at which trees have the best performance 
#and build future trees off of that. Gradient Boost looks at what trees have the most error and avoids those trees in future models. 
#Both of those methods use decision trees to build out the model.



############################
### 2. Homework Exercise ###
############################
# Find the best hyperparameter combination for the Random Forest model. 
# You can manually tune the model, use a loop or another package.
# Maximize the metric of your choice.


#Looking to maximize mtry

random_forest_model <- randomForest(Churn ~ ., data = train %>% select(-CustomerID),
                                    # choose some of the hyper-parameters included in the randomForest package
                                    nodesize = 100, # minimum number of elements at the terminal nodes
                                    sampsize = 1000, # number of element drawn from the data for each tree
                                    mtry = 4, # number of predictors used at each split
                                    ntree = 100) # number of trees built

random_forest_model
varImp(random_forest_model)
varImpPlot(random_forest_model)


##### predict on train
pred_train_random_forest <- as.data.frame(predict(random_forest_model, train, type='prob'))
head(pred_train_random_forest)
# AUC on train
roc(train$Churn, pred_train_random_forest$Yes, percent = TRUE, plot = TRUE) ## 86.08
##### predict on test
pred_test_random_forest <- as.data.frame(predict(random_forest_model, test, type='prob'))
head(pred_test_random_forest)
# AUC on test
roc(test$Churn, pred_test_random_forest$Yes, percent = TRUE, plot = TRUE) #81.16%

#------------------------------------------------------------------------

random_forest_model <- randomForest(Churn ~ ., data = train %>% select(-CustomerID),
                                    # choose some of the hyper-parameters included in the randomForest package
                                    nodesize = 100, # minimum number of elements at the terminal nodes
                                    sampsize = 1000, # number of element drawn from the data for each tree
                                    mtry = 2, # number of predictors used at each split
                                    ntree = 100) # number of trees built

random_forest_model
varImp(random_forest_model)
varImpPlot(random_forest_model)


##### predict on train
pred_train_random_forest <- as.data.frame(predict(random_forest_model, train, type='prob'))
head(pred_train_random_forest)
# AUC on train
roc(train$Churn, pred_train_random_forest$Yes, percent = TRUE, plot = TRUE)  ##85.43 
##### predict on test
pred_test_random_forest <- as.data.frame(predict(random_forest_model, test, type='prob'))
head(pred_test_random_forest)
# AUC on test
roc(test$Churn, pred_test_random_forest$Yes, percent = TRUE, plot = TRUE) ## 80.75%

#------------------------------------------------------------------------

train <- train[, colSums(is.na(train)) == 0]

random_forest_model <- randomForest(Churn ~ ., data = train %>% select(-CustomerID),
                                    # choose some of the hyper-parameters included in the randomForest package
                                    nodesize = 100, # minimum number of elements at the terminal nodes
                                    sampsize = 1000, # number of element drawn from the data for each tree
                                    mtry = 6, # number of predictors used at each split
                                    ntree = 100) # number of trees built

random_forest_model
varImp(random_forest_model)
varImpPlot(random_forest_model)


##### predict on train
pred_train_random_forest <- as.data.frame(predict(random_forest_model, train, type='prob'))
head(pred_train_random_forest)
# AUC on train
roc(train$Churn, pred_train_random_forest$Yes, percent = TRUE, plot = TRUE) ## 85.7%
##### predict on test
pred_test_random_forest <- as.data.frame(predict(random_forest_model, test, type='prob'))
head(pred_test_random_forest)
# AUC on test
roc(test$Churn, pred_test_random_forest$Yes, percent = TRUE, plot = TRUE)  ## 80.75

#------------------------------------------------------------------------

#The ideal mtry number appears to be 4

############################
### 3. Homework Exercise ###
############################
# Find the best hyperparameter combination for the Gradient Boosting model. 
# You can manually tune the model, use a loop or another package.
# Maximize the metric of your choice.
# use train_gbm and test_gbm defined above. We had to change the dependent variable to 0 and 1 due to how the package is developed.

#Looking to maximize interaction.depth

# change the Yes and No in the Churn column to 0 and 1 (gbm requirement beacause of package)
train_gbm <- train %>% mutate(Churn = ifelse(Churn == "Yes", 1, 0))
test_gbm <- test %>% mutate(Churn = ifelse(Churn == "Yes", 1, 0))

set.seed(234) # gbm uses random numbers, so we should use a seed to recreate the results
gbm_model <- gbm(Churn ~ ., data = train_gbm %>% select(-CustomerID), 
                 distribution = "bernoulli",
                 n.trees = 100, # number of trees being fit on the data
                 shrinkage = 0.05, # shrinkage / learning rate
                 interaction.depth = 3, # maximum depth of the tree
                 bag.fraction = 0.5, # % of the rows that will be used to train a decision tree
                 n.minobsinnode = 10, # minimum number of observations at each terminal node
                 # cv.folds = 10, here you could choose to do k-fold cross validation
                 verbose = TRUE)

# check the important variables of the model
summary(gbm_model)

##### predict on train_gbm
pred_train_gbm <- predict(gbm_model, train_gbm, type = "response", n.trees = 100) # it sometimes gives issues if you don't specify the number of trees you wish to use in the prediction
head(pred_train_gbm)    ## [1] 0.59185086 0.51190874 0.23465098 0.41704086 0.09129631 0.05488484

# AUC on train_gbm
roc(train_gbm$Churn, pred_train_gbm, percent = TRUE, plot = TRUE) ## 87.17%

##### predict on test_gbm
pred_test_gbm <- predict(gbm_model, test_gbm, type='response', n.trees = 100)
head(pred_test_gbm) ## [1] 0.32097470 0.34728357 0.07483743 0.16753457 0.07025559 0.09351836

# AUC on test_gbm
roc(test_gbm$Churn, pred_test_gbm, percent = TRUE, plot = TRUE) ## 83.19%

#----------------------------------------------------------------------

gbm_model <- gbm(Churn ~ ., data = train_gbm %>% select(-CustomerID), 
                 distribution = "bernoulli",
                 n.trees = 100, # number of trees being fit on the data
                 shrinkage = 0.05, # shrinkage / learning rate
                 interaction.depth = 7, # maximum depth of the tree
                 bag.fraction = 0.5, # % of the rows that will be used to train a decision tree
                 n.minobsinnode = 10, # minimum number of observations at each terminal node
                 # cv.folds = 10, here you could choose to do k-fold cross validation
                 verbose = TRUE)

# check the important variables of the model
summary(gbm_model)

##### predict on train_gbm
pred_train_gbm <- predict(gbm_model, train_gbm, type = "response", n.trees = 100) # it sometimes gives issues if you don't specify the number of trees you wish to use in the prediction
head(pred_train_gbm)    ## [1] 0.65215391 0.45454120 0.18135048 0.40966270 0.07428041 0.05623862

# AUC on train_gbm
roc(train_gbm$Churn, pred_train_gbm, percent = TRUE, plot = TRUE) ##89.15%

##### predict on test_gbm
pred_test_gbm <- predict(gbm_model, test_gbm, type='response', n.trees = 100)
head(pred_test_gbm)   ## [1] 0.26141865 0.31710026 0.05879384 0.14695616 0.04414133 0.06695641

# AUC on test_gbm
roc(test_gbm$Churn, pred_test_gbm, percent = TRUE, plot = TRUE) ##83.2%

#----------------------------------------------------------------------


gbm_model <- gbm(Churn ~ ., data = train_gbm %>% select(-CustomerID), 
                 distribution = "bernoulli",
                 n.trees = 100, # number of trees being fit on the data
                 shrinkage = 0.05, # shrinkage / learning rate
                 interaction.depth = 5, # maximum depth of the tree
                 bag.fraction = 0.5, # % of the rows that will be used to train a decision tree
                 n.minobsinnode = 10, # minimum number of observations at each terminal node
                 # cv.folds = 10, here you could choose to do k-fold cross validation
                 verbose = TRUE)

# check the important variables of the model
summary(gbm_model)

##### predict on train_gbm
pred_train_gbm <- predict(gbm_model, train_gbm, type = "response", n.trees = 100) # it sometimes gives issues if you don't specify the number of trees you wish to use in the prediction
head(pred_train_gbm)   ## [1] 0.65439649 0.50490141 0.23532034 0.45264765 0.06310349 0.05311792

# AUC on train_gbm
roc(train_gbm$Churn, pred_train_gbm, percent = TRUE, plot = TRUE) ## 88.21%

##### predict on test_gbm
pred_test_gbm <- predict(gbm_model, test_gbm, type='response', n.trees = 100)
head(pred_test_gbm)    ## [1] 0.31076032 0.32710869 0.06917469 0.15241071 0.05040369 0.08616275

# AUC on test_gbm
roc(test_gbm$Churn, pred_test_gbm, percent = TRUE, plot = TRUE) ##83.34

#----------------------------------------------------------------------

##The ideal Interaction Depth number appears to be 5

############################
### 4. Homework Exercise ###
############################
# Get the important variables of the Decision Tree. What are the top few ones?

##The top variables from the Decision Tree were Total Charges, DOE, and Education

############################
### 5. Homework Exercise ### 
############################
# Get the important variables of the Random Forest. What are the top few ones? hint: use varImp()


##The top variables from the Random Forest were DOE, Contract type, and Base Charges

############################
### 6. Homework Exercise ### 
############################
# Get the important variables of the GBM models. What are the top few ones?

##The top variables from the GBM models were Contract Type, Base Charges, and DOE

############################
### 7. Homework Exercise ###
############################
# Do the same variables appear as important across all 3 models? Do they have the same order?

##The same variables appear in both the Random Forest and GBM models, however they appear in a different order. The only
##variable that appears in all 3 model types is DOE.

############################
### 8. Homework Exercise ###
############################
# Is it something to be concerned about if they show different order? Why do you think they show differences?

#This does not have to be concerning, no. In every model there are trade-offs for what may be the best tool for the job
#Those trade-offs relate to mathematical differences, and those differences can explain why the model might weight a 
#certain variable might be different across models.