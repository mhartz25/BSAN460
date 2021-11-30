### BSAN 460 -- Business Analytics Senior Project
### Winter 2021
### Week 6 -- Logistic Regression for Binary Classification
### Prof. Irina Nedelcu

# Install packagaes if they are not already installed on the PC (uncomment the rows below)
#install.packages("dplyr")
#install.packages("pROC")
#install.packages("MLmetrics")

# Load packages in R
library(dplyr)
library(pROC) # this package is used to calculate AUC (performance measure for fitness of probabilities)
library(MLmetrics)

# set the working directory; ! if you copy paste the path, then change the \ to /
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")


# let's read the file again from the folder; now, there is no need to write the entire path
cust_churn <- read.csv("Customer Churn.csv")


# check the structure of the data frame
# str() function tells that the data has 5298 observations and 18 variables
# variables are stored as characters, integers and numeric
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
cust_churn <- cust_churn %>% mutate(Education = ifelse(Education %in% c("0", "Other"), "Other", Education))
cust_churn %>% count(Education)

# TotalCharges column is stored as character, but it in fact contains numbers, so let's change it to numeric
str(cust_churn)
cust_churn <- cust_churn %>% mutate(TotalCharges = as.numeric(TotalCharges))

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
cust_churn <- cust_churn %>% mutate(ContractType = ifelse(is.na(ContractType), "Month-to-month", ContractType)) # change the NA to "Other"
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


# now we have the train and test sets created, so let's apply logistic regression on them

# glm comes from Generalized Linear Model
# glm() function can therefore be used for both linear and logistic regression
# family = "binomial" is used because in this case, we apply logistic regression
# usually, you would use these 3 options in the glm() function:
  # formula --> dependentVariable ~ independentVar1 + indepVar2
              # dependentVariable ~ .         ---> this includes all the other variables in the data as independent
  # data --> dataset that has the dependent and independent variables
  # family --> use binomial for logistic regression

################### train model 1
# let's first fit a logistic regression using just 2 independent variables
mylogit1 <- glm(Churn ~ Retired + HasPartner, data = train, family = "binomial")
mylogit1
# notice that the intercept is negative, so by default, a customer is less likely to churn
# the coefficients of the model show if they have a positive or negative impact on the dependent variable
# an increase in Retired or HasPartner leads to an increase of the probability to churn
# the coefficients can be added in the log regr equation to calculate the increase in odds by each increase in unit

##### predict on train
# predict values on the train data
# predict() function takes in the model, the data to do the predictions on and the type of output
# type = 'response' provides predicted values on the same scale as the dependent variable
pred_train <- predict(mylogit1, train, type='response')
head(pred_train)

# check the histogram of the probabilities to see over what range between 0 and 1 they are
hist(pred_train, breaks = 100) 
# notice that there are no probabilities greater than 0.5, so all the instances get categorized as No Churn if we keep the 0.5 threshold

# calculate and display the Area Under the Curve (AUC) on the train data
roc(train$Churn, pred_train, percent = TRUE, plot = TRUE) 
# we get an AUC of 62.59%, not that good since AUC of random chance is 50%

# calculate performance measures using the Metrics package
# first, transform the probabilities and then calculate the metrics
pred_train_cat <- ifelse(pred_train >= 0.5, "Yes", "No")
Accuracy(y_true = train$Churn, y_pred = pred_train_cat) # 0.74 # ! the dataset has imbalanced dependent variable, so Accuracy is not a good metric
F1_Score(y_true = train$Churn, y_pred = pred_train_cat)   
ConfusionMatrix(y_true = train$Churn, y_pred = pred_train_cat)

##### predict on test
# predict values on the test data
pred_test <- predict(mylogit1, test, type='response') 
head(pred_test)

# calculate and display the Area Under the Curve (AUC) on the train data
roc(test$Churn, pred_test, percent = TRUE, plot = TRUE) 
# we get an AUC of 60.13%, not that good since AUC of random chance is 50%, but similar to train set

# calculate performance measures on test data
pred_test_cat <- ifelse(pred_test >= 0.5, "Yes", "No")
Accuracy(y_true = test$Churn, y_pred = pred_test_cat) # 0.765
F1_Score(y_true = test$Churn, y_pred = pred_test_cat)   
ConfusionMatrix(y_true = test$Churn, y_pred = pred_test_cat)


################### train model 2
# let's first fit a logistic regression using all variables
mylogit2 <- glm(Churn ~ ., data = train %>% select(-CustomerID), family = "binomial")
mylogit2 
# notice that the intercept is negative, so by default, a customer is less likely to churn
# the coefficients of the model show if they have a positive or negative impact on the dependent variable
summary(mylogit2)

##### predict on train
pred_train2 <- predict(mylogit2, train, type='response') 
head(pred_train2)

# AUC on train
roc(train$Churn, pred_train2, percent = TRUE, plot = TRUE) 
# we get an AUC of 85.13%, which is much better than before!

# performance metrics on train
pred_train_cat2 <- ifelse(pred_train2 >= 0.5, "Yes", "No")
Accuracy(y_true = train$Churn, y_pred = pred_train_cat2) # 0.81
F1_Score(y_true = train$Churn, y_pred = pred_train_cat2) # 0.88
ConfusionMatrix(y_true = train$Churn, y_pred = pred_train_cat2)


##### predict on test
pred_test2 <- predict(mylogit2, test, type='response') 
head(pred_test2)

# AUC on test
roc(test$Churn, pred_test2, percent = TRUE, plot = TRUE) 
# we get an AUC of 85.09%; 

# calculate performance measures on test data
pred_test_cat2 <- ifelse(pred_test2 >= 0.5, "Yes", "No")
Accuracy(y_true = test$Churn, y_pred = pred_test_cat2) # 0.82 
F1_Score(y_true = test$Churn, y_pred = pred_test_cat2) # 0.89
ConfusionMatrix(y_true = test$Churn, y_pred = pred_test_cat2)

rm(list=ls())


############################
### 1. Homework Exercise ###
############################
# For what type of applications would you use Logistic Regression and what are some advantages of Logistic Regression?

#Logistic Regression can be especially useful when you have a population in which you are trying to classify something that is binary.
#For instance, if you were trying to predict if an email is spam you could use logistic regression to analyze the characteristics
#of the email and classify it as spam or not spam. Advantages of logistic regression include its simplicity to use and also the ability to
#explain it to people who are not familiar with statistics. 

############################
### 2. Homework Exercise ###
############################
# What is the difference between linear and logistic regression?

#Logistic regression deals with a dependent variable that is categorical or binary. Essentially this variable has only two outcomes.
#Linear regression deals with a dependent variable that is continuous. 


############################
### 3. Homework Exercise ###
############################
# How would you evaluate classification models? In class, we talked about several performance metrics. 
# In what cases would you prefer to use certain performance measures over others?

#There are different type of statistics you can use to evaluate a classification model. These include forms of internal and external 
#validation. When the model makes predictions there are 4 different results that these predicitions can be: True Positive, True Negative,
#False Positive, and False Negative. From these 4 results we can compose different ways of evaluating a model. Some of the most popular 
#metrics are Accuracy and Precision. In an example of when the class balance is especially imbalanced we would probably prefer to use
#Precision over Accuracy. For example, if we are dealing with credit card fraud, fraud is a rare event and if our model just consistently
#predicts no fraud it will ultimately be very accurate. However, this will not be useful to our means for creating the model.



############################
### 4. Homework Exercise ###
############################
# In mylogit2, we included all the independent variables in the logistic regression.
# check summary(mylogit2). This will provide the coefficients of each variable as well as significance levels
# of these coefficients (last column: Pr(>|z|)). If a star is displayed for a coefficient, then it means that
# it is significantly having an impact on the equation line.
# select important variables using the output of the mylogit2 model and create a new model (mylogit3) just
# with these variables. Check the summary of the new logistic model afterwards.
# When you put the variables in the equation line, you should put just the variable name, not 
  # the variable name and the level name for it to work.
# There might be variables for which some levels are significant, while others aren't. 
  # For these, you can include all the levels in the regression. 


summary(mylogit2)

mylogit3 <- glm(Churn ~ ., data = train %>% select(Churn,Retired, Education, BaseCharges, TotalCharges, DOE, ElectronicBilling,
                                                   ContractType), family = "binomial")

summary(mylogit3)



############################
### 5. Homework Exercise ###
############################
# For the new model that you ran, check to see how well it is performing on both the train and test set.
# Use measures such as Accuracy, F-measure and AUC. You can check other metric from the MLmetrics package 
# as well. Check in the Help tab what other metrics it includes: Sensitivity, PRAUC, Precision etc.
# not all metrics are for classification models, some are for regression models.
# some of these metrics are used to evaluate the probabilities predicted, while other metrics 
# are used to evaluate the model when choosing a threshold. You can try different thresholds.

# evaluate mylogit3 on both train and test data

##### predict on train
pred_train3 <- predict(mylogit3, train, type='response') 
head(pred_train3)

# AUC on train
roc(train$Churn, pred_train3, percent = TRUE, plot = TRUE) 
# AUC of 85.57%

# performance metrics on train
pred_train_cat3 <- ifelse(pred_train3 >= 0.5, "Yes", "No")
Accuracy(y_true = train$Churn, y_pred = pred_train_cat3) # 0.8155
F1_Score(y_true = train$Churn, y_pred = pred_train_cat3) # 0.8817
ConfusionMatrix(y_true = train$Churn, y_pred = pred_train_cat3)


##### predict on test
pred_test3 <- predict(mylogit3, test, type='response') 
head(pred_test3)

# AUC on test
roc(test$Churn, pred_test3, percent = TRUE, plot = TRUE) 
#AUC of 82.9%

# calculate performance measures on test data
pred_test_cat3 <- ifelse(pred_test3 >= 0.5, "Yes", "No")
Accuracy(y_true = test$Churn, y_pred = pred_test_cat3) # 0.7954 
F1_Score(y_true = test$Churn, y_pred = pred_test_cat3) # 0.8685
ConfusionMatrix(y_true = test$Churn, y_pred = pred_test_cat3)




############################
### 6. Homework Exercise ###
############################
# How is the new model performing when using fewer but significant variables comparing to the way it 
# was performing when all the variables were included in the model?

#The performance of the model on the testing set ended up decreasing in many metrics. In AUC, Accuracy, and Precision there was 
#all a decrease in performance


############################
### 7. Homework Exercise ### 
############################
# When we calculate measures such as Accuracy and F-measure, we apply a threshold on the probability
  # and binarize it. Different thresholds will change the value of Accuracy, F-measure and other measures 
  # that are threshold dependent. 
# Try different thresholds when binarizing the output of logistic regression (in class we used 0.5). 
# You can use mylogit2 model for this.
# Find a threshold that maximizes the f-measure.
# What threshold would you use for the model and why? 
  # You can create a loop to find out the best threshold (not mandatory).
  # If you create a loop, here are some tips:
    # use an indicator, let's call it i, going from 0.1 to 0.9 in increments of 0.01: for(i in seq(0.1, 0.9, by = 0.01)){}
    # F1_score gives some issues when everything is categorized as either Yes or No, so that's why I chose 0.1 and 0.9 above
    # inside the loop, you will need an if statement. Ex: if(new_f1_score > max_f1_score){...put code here...}



# Changing the threshold for F-measure on model 3

##### predict on train
pred_train3 <- predict(mylogit3, train, type='response') 
head(pred_train3)

# AUC on train
roc(train$Churn, pred_train3, percent = TRUE, plot = TRUE) 
# AUC of 85.57%

# performance metrics on train
pred_train_cat3 <- ifelse(pred_train3 >= 0.6, "Yes", "No") #changed threshold from .5 to .6
Accuracy(y_true = train$Churn, y_pred = pred_train_cat3) # Was 0.8155 now .8056
F1_Score(y_true = train$Churn, y_pred = pred_train_cat3) # Was 0.8817 now .8803
ConfusionMatrix(y_true = train$Churn, y_pred = pred_train_cat3)

##### predict on test
pred_test3 <- predict(mylogit3, test, type='response') 
head(pred_test3)

# AUC on test
roc(test$Churn, pred_test3, percent = TRUE, plot = TRUE) 
#AUC of 82.9%

# calculate performance measures on test data
pred_test_cat3 <- ifelse(pred_test3 >= 0.6, "Yes", "No")
Accuracy(y_true = test$Churn, y_pred = pred_test_cat3) # Was 0.7954 now .7967
F1_Score(y_true = test$Churn, y_pred = pred_test_cat3) # Was 0.8685 now .8752
ConfusionMatrix(y_true = test$Churn, y_pred = pred_test_cat3)




############################
### 8. Homework Exercise ### 
############################
# Look at the input data. Is there any other data cleanup that you would do? Why? (no need to write code)

#One thing that could be done to the input data is normalizing the numerical columns. By doing this form of standardizing the data, the 
#model might do a better job of prediciting churn



############################
### 9. Homework Exercise ### 
############################
# In text mining, you would use the Tf-Idf to do logistic regression.
# The Tf-Idf would have a column for each term. These columns become your independent variables.
# Your dependent variable would be the sentiment column.
# In the last class, we did Sentiment Analysis using lexicon dictionaries.
# Would you expect to get better results from lexicon based sentiment analysis or by using 
  # logistic regression (or another machine learning model)? (No need to calculate anything, just type an answer)

#I think I would probably expect to get better results from lexicon based sentiment because the model is specifically
#desgined for senitment analysis. The logistic regression model might suffer if the corpus is extremely big, and it will not know how to
#weight certain words. 




############################# Once going through the explanations, answer: 
### 10. Homework Exercise ###  How does standardization affect logistic regression?
#############################
# Below, I take the continuous variables and I bring them on the same scale; 
cust_churn3 <- cust_churn %>% mutate_each_(funs(scale),vars=c("DOE","TotalCharges", "BaseCharges"))
summary(cust_churn3) # notice in the summary funciton that these 3 columns have a mean of 0 (mean 0 and stdev of 1)

# mylogit2 is the same model we ran previously
mylogit2 <- glm(Churn ~ ., data = train %>% select(-CustomerID), family = "binomial")

# create another train dataset that has the same instances as cust_churn, 
  # but we are using cust_churn3 to create train (has 3 numeric columns are scaled)
train3 <- cust_churn3[train_ind, ]
mylogit3 <- glm(Churn ~ ., data = train3 %>% select(-CustomerID), family = "binomial")


# these are the first few instances of predicted probabilities using mylogit2 and mylogit3 models
# Does standardization affect predicted probabilities?
head(predict(mylogit2, train, type='response'))
head(predict(mylogit3, train3, type='response'))


# Check the summary of these 2 models. Do you notice any difference in the output?
# Look at AIC, which is a measure of model fitness; 
# Also look at the beta coefficients of the variables (Estimate column).
summary(mylogit2)
summary(mylogit3)


#Standardization is way to limit the affect of outliers on the model itself. Because the error is squared in a regression model
#such as this it means that as you get further away from the mean on certain inputs the impact becomes larger and larger on the cost function



