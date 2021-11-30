### BSAN 460 -- Business Analytics Senior Project
### Fall 2021
### Intro to Neural Networks

rm(list=ls())
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")

# Install packagaes if they are not already installed on the PC (uncomment the rows below)
#install.packages("dplyr")
#install.packages("pROC")
#install.packages("MLmetrics")
install.packages("h2o", dependencies = TRUE)

# Load packages in R
library(dplyr)
library(pROC) # this package is used to calculate AUC (performance measure for fitness of probabilities)
library(MLmetrics)
library(h2o)





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
############ Feed Forward Neural Net ############
#################################################
# regular neural network packages in R are not too good, but h2o opensourced some models, including feed-forward neural net (multi-layer perceptron)
# h2o neural net is good because it is fast (allows to use more memory and cores to run the models, while other packages don't allow for it)
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/deep-learning.html

# start an h2o instance
h2o.init() # inside here you could specify how much max memory it should use or how many cores
# h2o.init(max_mem_size = "10g", nthreads = 7) # for example, this uses 10gb memory and 7 cores

# to apply an h2o model, we should first transform the data frames into h2o frames
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

# save the independent variables in x and the dependent variable in y
x <- names(train[,!names(train) %in% c("CustomerID", "Churn")]) # keep all the column names except for CustomerID and Churn
x
y <- "Churn"
y

# specify the type of grid
my_search_criteria <- list(strategy = "RandomDiscrete", # random grid search
                           max_models = 20, #  number of models to be included in the random grid search
                           seed = 1) 

# determine the hyperparameters for the grid search; more hyper-parameters available in the documentation
hyper_parameters <- list(
  activation = c("RectifierWithDropout"), # activation function (check documentation to see what other activation functions are available)
  hidden = list(c(20,2), c(6,4)),  # this has 2 options: 2 layer of 20 and 2 nodes; 2 layers of 6 and 4 nodes
  input_dropout_ratio = c(0), # % of input variables to remove
  hidden_dropout_ratios = list(c(0.3, 0.6), c(0.2, 0.4)), # percent of input nodes to remove at each layer
  rate = seq(0.01, 0.3, 0.01), # learning rate of gradient descend
  epochs = seq(5, 25, 2) # number of times the model should be trained
)

# build the neural net model
nn_model <- h2o.grid(algorithm = "deeplearning",
                    x = x, # these are the independent variables
                    y = y, # the dependent variable (Churn)
                    training_frame = train.h2o,
                    validation_frame = test.h2o,
                    hyper_params = hyper_parameters, # uses the grid defined above
                    search_criteria = my_search_criteria, # uses random grid search, as defined above
                    standardize = TRUE, # in neural nets, data should always be standardized! by default, the h2o package standardizes data, but other packages don't do this by default, so you would need to scale all the variables before putting them in a model
                    seed = 1)

# look at the summary of the model
# it uses logloss as the cost function and you can see how well the model performs for each set of hyper-parameters
nn_model


# predict on train
# when doing the prediction with the h2o model, we get back an h2o frame
# to use functions from MLmetrics package to calculate perfomance measure, we have to change the h2o frame to data frame
# another option is to use the functionsof the h2o package to calculate performance
pred_train_nn <- as.data.frame(predict(h2o.getModel(nn_model@model_ids[[1]]), train.h2o)) # use as.data.frame() to change the h2o frame to a data frame; the number 1 inside [[1]] means that we get the first model out of 20
head(pred_train_nn)

# AUC on train
roc(train$Churn, pred_train_nn$Yes, percent = TRUE, plot = TRUE) 

# predict on test
pred_test_nn <- as.data.frame(predict(h2o.getModel(nn_model@model_ids[[1]]), test.h2o))
head(pred_test_nn)

# AUC on test
roc(test$Churn, pred_test_nn$Yes, percent = TRUE, plot = TRUE)


############################
### 1. Homework Exercise ### - 1 point
############################ # you can use the functions that we used in the previous weeks (MLmetrics package) or the h2o.performance() function: https://www.rdocumentation.org/packages/h2o/versions/3.28.0.2/topics/h2o.performance
# Calculate Accuracy, AUC, Confusion Matrix, f-measure and Recall on both train and test data
# write the code and then write in comments the results that you get

model <- h2o.getModel(nn_model@model_ids[[1]])
metrics <- h2o.performance(model)

metrics

#Accuracy .81
#F1 .62
#Precision 1
#Recall 1

#Confusion Matrix
      #No   #Yes
#No   2250  534
#Yes  271   654

############################
### 2. Homework Exercise ### - 1 point
############################
# When you develop a predictive model, why is it recommended to use train and test datasets?

##The reasons this is done is mainly to avoid overfitting and to have ways to validate the model. If you train the model on 
##all the data you have then the model will do its best to fit that specific dataset, which might not have the same charatersitics 
##of future data you would run the model on. 

############################
### 3. Homework Exercise ### - 1 point
############################
# Why should you use a seed when you run some models?

##The seed serves as a starting point for the mathematical models random numbers. You do this so that every time you run the model
## you get the same results on the same data. Otherwise, if you were given different results everytime it would be tough to feel 
##confident in the model.

############################
### 4. Homework Exercise ### - 1 point
############################
# Do you expect a neural net to take longer time to run than a logistic regression?

##Yes, the calculations going on inside a neural net are much more complicated than the are in logistic regression. But also a neural network 
#will run many more models with slight changes.

############################
### 5. Homework Exercise ### - 1 point
############################
# Should you standardize data before applying a neural net model? why?

##It is not completely necessary to standardize the data for a neural network. However, because of performance
##concerns it can be very helpful to standardize the data. In this case performance can be in regard to both
##time to train the model, and also overall validation of the model. When you standardize the data you take care of outliers
##which can help the model to converge on an opitmal cost function faster.

############################
### 6. Homework Exercise ### - 1 point
############################
# Why do you have to try different combinations of hyper-parameters for models such as random forest and neural net?
##Part of this is just trying to iterate to find the best possible combination. Every model, whether neural net or Random Forest, is 
##different, so it is necessary to try out different parameters to find what may work best for that specific model.



