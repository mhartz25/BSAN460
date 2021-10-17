### BSAN 460 -- Business Analytics Senior Project
### Fall 2021
### Lecture 1 -- Intro to R


# R is a free software environment for statistical computing and graphics.
# It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS.
# 
# R provides a wide variety of statistical (linear and nonlinear modelling, classical statistical tests,
# time-series analysis, classification, clustering.) and graphical techniques, and is highly extensible.
# 
# Many users think of R as a statistics system. We prefer to think of it as an environment within
# which statistical techniques are implemented. R can be extended (easily) via packages.
# There are many packages available through the CRAN family of Internet sites covering a very wide range
# of modern statistics.
# 
# Want to learn more about R? Check out this link:
# https://www.r-project.org/about.html


#########################################
# Data wrangling using R base functions #
#########################################

##### Create a vector
# A vector is a basic data structure in R. 
# It contains element of the same type. 
# The data types can be logical, integer, double, character, factor, complex or raw.

# vector containing just numeric values
# if wishing to create a vector that has more than 1 element, then put the element in c(), separated by commas
my_vector <- c(1, 2, 3, 4, 5) 
my_vector

# vector containing words is stored in character format, shown by the "" when being displayed;
# when specifying the elements to be added in the vector, it doesn't matter if "" or '' is used;
# however, an element has to be surrounded by quotes of the same type (no "')
my_vector2 <- c("here", 'us') 
my_vector2

# notice that when mixing numbers and characters in the same vector, 
# all the instances are transformed into character format;
# a vector must contain elements of the same type!
my_vector3 <- c(1, 2, "here") 
my_vector3


##### Access an element of a vector
# get the element that is at the first position in the vector
my_vector3[1]


##################
# class exercise # Get the 2nd element of my_vector 2 and assign it to a variable named my_variable
##################



##### Create a data frame
#  A data frame is used for storing data tables. It is a list of vectors of equal length

# data.frame() is a function used to create a data frame
# the data frame below has 2 columns (Column1 and Column2), the 1st one having 4 numeric elements, 
# the 2nd one having character elements
my_data_frame <- data.frame(Column1 = c(1,2,4,5), Column2 = c("school", "class", "phone", "here"))
my_data_frame


##### Create a list
# A list is a generic vector containing other objects.

# list() is a function to create a list
# the list below has 2 objects (Object1 and Object2), the 1st one having 3 numeric elements,
# the 2nd one having 4 characted elements
my_list <- list(object1 = c(1,2,4), object2 = c("school", "class", "phone", "here"))
my_list


##### Create a matrix
# A matrix is a data frame in which all the columns are of the same type

# matrix() is a function to create a matrix
# it takes in a vector; nrow signifies the number of rows, while ncol signifies the number of columns
my_matrix <-  matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
my_matrix


##### Check the structure of a vector, data frame and list.
# The str() function shows details about the dimensions of the data frame/ vector/ list/ matrix and 
# the types of variables that it contains.
# To find out just the number of rows and columns in a data frame, use the function dim().

# str() of a vector shows the number of elements in that vector and the first few elements
str(my_vector)

# str() of a data frame shows that you have a data frame structure, the number of rows and columns and 
# the type of each column
str(my_data_frame)
# the 1st number in the output is the number of observation and the 2nd number is the number of variables
dim(my_data_frame) 

# str() of a list shows the number of objects in the list along with the type of elements in each object
str(my_list)

# str() of a matrix displays the number of columns and rows along with the first few elements
str(my_matrix)
# dim() of a matrix displays the number of columns and rows
dim(my_matrix)



# To see the top rows of a data frame, use the function head(). By default, it is displaying the first 6 rows of a data frame. 
# If you wish to display the top n rows, then write head(my_data, n).
# For display purposes, let's use one of the buit in data frames, iris, in R.
head(iris)

##################
# class exercise # Display the top 3 rows of the iris dataset
##################


# To display the last few rows of a dataset, use the function tail() instead of head().

##################
# class exercise # Display the bottom 4 rows of the iris dataset
##################


##################
# class exercise # What types of columns are in the data frame? How many rows and columns are there?
##################



##### Check details about the data.
# For numeric values, the summary() function provides the minimum, 1st quartile, median, mean, 3rd quartile and maximum statistics. 
# For categorical variables, the summary() function displays the number of times each factor appears.
summary(iris)


##### Change the type of column that we have in a data frame.
# When working with strings, there would be times when we need the column be stored in factor format, while other times in character format. 
# Machine Learning models take factor variables as input, while using regex functions on string data requires character format.
# the Species column is currently stored as a factor. Let's change it to character format
iris$Species <- as.character(iris$Species)
str(iris)
summary(iris)

##### Subset rows in a data frame.
subset_rows1 <- iris[1, ] # select row 1
subset_rows1

subset_rows2 <- iris[2:5, ] # select rows 2 to 5
subset_rows2

##### Subset columns in a data frame.
subset_columns1 <- iris[, 1] # select column 1
head(subset_columns1)

subset_columns2 <- iris[, 2:4] # select columns 2 to 4
head(subset_columns2)

subset_columns3 <- iris[, c(2, 4)] # select columns 2 and 4
head(subset_columns3)

subset_columns4 <- iris[, c("Sepal.Width", "Species")] # select multiple columns by their names
head(subset_columns4)

subset_columns5 <- iris$Sepal.Length # select one column by its name
head(subset_columns5)
# Notice that the output of subset_columns5 and subset_columns4 is displayed in a different format.



##################
# class exercise # Check the structure of subset_columns4 and subset_columns5. Why are they displayed differently? Use str().
##################



##### Create a new column.
iris$new_column <- iris$Sepal.Length + 1   # add 1 to all the values within the Sepal.Length column
head(iris)

##### Using ifelse statement to create a column based on condition.
iris$new_column2 <- ifelse(iris$Sepal.Length < 5, "low", "high")  # if Sepal.Length is lower than 5, then put "low", otherwise, populate with "high"
head(iris)


# !All of the above data manipulations are performed using base R functions.

##############################
# Data wrangling using dplyr #
##############################
# dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges.
# If using RStudio, then dplyr package would need to be installed on the machine first, unless it has been previously installed. 
# To install a package, write the following: install.packages("dplyr").
# Also, everytime we have a new R session and we need to use a package, we have to load it: library(dplyr)
# To learn more about dplyr, use the following link: https://www.rdocumentation.org/packages/dplyr/versions/0.7.8
# 
# The dplyr cheat sheet is also very helpful: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# install the dplyr package. Uncomment if you need to install.
# install.packages("dplyr")

# load the dplyr library
library(dplyr)

##### Select columns from a data frame using select().
# in dplyr, there is no need to put the column names in quotes or use the $
my_data <- iris %>% select(Species, Sepal.Length, Sepal.Width)
head(my_data)


##### Create a new column using mutate().
my_data <- my_data %>% mutate(Difference_Length_Width = Sepal.Length - Sepal.Width,
                              Sum_Length_Width = Sepal.Length + Sepal.Width)
head(my_data)


##### Filter data frame by a certain condition using filter().
my_data <- my_data %>% filter(Sum_Length_Width > 8) # keeps only the rows that have Sum_Length_Width greater than 8
head(my_data)


##### Calculate summary statistics of columns using summarise().
head(iris)
iris %>% summarise(mean_Sepal_Length = mean(Sepal.Length), median_Sepal_Length = median(Sepal.Length),
                   mean_Sepal_Width = mean(Sepal.Width), median_Sepal_Width = median(Sepal.Width), row_counts = n()) # n() gives the number of rows


# Dplyr uses pipes, %>%, which allow you perform multiple data manipulation functions in the same chunk of code. 
# So, if you wish to both filter and add a new column to the data frame within the same code chunk, then you could write: 
# data %>% filter() %>% mutate()


##################
# class exercise # How many records of each species are in the data frame? Hint: use count() or group_by() %>%  summarise().
##################




##################
# class exercise # Calculate the number of Setosa that have a Sepal Length greater than 5.
##################



##################
# class exercise # Rename the column Sepal.Width to Sepal_Width in the iris data frame and save as my_data. 
################## Hint: use rename(newColumnName = oldColumnName)






########################################################
# setting working directory, reading, writing CSV file #
########################################################
# reading csv files is done by using the read.csv() function
# read the customer churn data and keep the data in a dataset names cust_churn
# inside read.csv(), put the entire csv file path; if you come the path, then make sure to change all \ to / !!
cust_churn <- read.csv("D:/Teaching Courses/BSAN 460 - Capstone - Fall 2021/Customer Churn.csv")

# if wanting to read/ write multiple files from the same folder, then it is recommended to set a working directory
setwd("D:/Teaching Courses/BSAN 460 - Capstone - Fall 2021")
getwd() # this tells you the path of the current working directory, which in this case it is the one that we just set up

# let's read the file again from the folder; now, there is no need to write the entire path
cust_churn2 <- read.csv("Customer Churn.csv")

# writing files is done using write.csv()
# inside write.csv(), writhe the name of the dataset you wish to write and the name of the file to be saved
write.csv(cust_churn, "Cust Churn1.csv")

# if you open the csv file, notice that it has an extra column at the beginning, which represents the row number
# to not write the row numbers, then we should use row.names = FALSE
write.csv(cust_churn, "Cust Churn2.csv", row.names = FALSE)


# check the structure of the file
# if you use an older version of R, then the variables that are stored as character might be stored as factor instead
# recommendation is to work with variables that are stored as character instead of factor
# if variables are read as factor, then use the following to read the csv file: read.csv("file name and path", stringsAsFactors = FALSE)
str(cust_churn)


############ 
# booleans #
############
# a boolean is a binary variable that has 2 possible value: True or False
# when checking for equality, == is used

2 == 3 # FALSE
2 == 2 # TRUE

# boolean values take numeric value: TRUE is a 1 and FALSE is a 0
# therefore, booleans can be used as numbers
TRUE + TRUE # this equals 2
TRUE + FALSE # this equals 1
FALSE + FALSE # this equals 0

vector1 <- c(1,2,4,5,6)
vector2 <- c(2,5,7,8)
vector3 <- c(1,2,3,NA) # the last element is a missing value (NA)

# check if any of the elements of vector1 is in vector2
vector1 %in% vector2  # FALSE  TRUE FALSE  TRUE FALSE  --> elements 2 and 5 are in vector 2
vector1 == 4 # FALSE FALSE  TRUE FALSE FALSE --> this compares each element of vector1 with 4

# check if there are any NAs in vector3
summary(vector3)
is.na(vector3)
!is.na(vector3)  # putting ! in front of is.na() reverses the boolean value


# check the summary of the cust_churn dataset 
cust_churn %>% count(ContractType) # Notice that the ContractType column has 5 NAs in it

# this checks if any of the cells in the dataset has  any missing values
is.na(cust_churn)
head(is.na(cust_churn), 3)  # if a value is missing then it populates with TRUE, otherwise, it populates with FALSE
# CustomerID Churn Country State Retired HasPartner HasDependents Education Gender BaseCharges   DOC
# [1,]      FALSE FALSE   FALSE FALSE   FALSE      FALSE         FALSE     FALSE  FALSE       FALSE FALSE
# [2,]      FALSE FALSE   FALSE FALSE   FALSE      FALSE         FALSE     FALSE  FALSE       FALSE FALSE
# [3,]      FALSE FALSE   FALSE FALSE   FALSE      FALSE         FALSE     FALSE  FALSE       FALSE FALSE
# TotalCharges   DOE ElectronicBilling ContractType PaymentMethod TypeOfService ServiceDetails
# [1,]        FALSE FALSE             FALSE        FALSE         FALSE         FALSE          FALSE
# [2,]        FALSE FALSE             FALSE        FALSE         FALSE         FALSE          FALSE
# [3,]        FALSE FALSE             FALSE        FALSE         FALSE         FALSE          FALSE

# colSums() is used to calculate the sum of all the values in each column
# since inside colSumn() we put is.na(), then we are calculating how many NAs are in each column
colSums(is.na(cust_churn))

# colnames() returns back a vector that has the names of the columns in a dataset
colnames(cust_churn)

# using colnames() along with colSums, you can get the names of the columns that have at least 1 NA value in them
colnames(cust_churn)[colSums(is.na(cust_churn)) > 0] # ContractType column has missing values


# usually, when preparing data for modeling, we have to ensure that there are no missing values
# in the data. If there are any, then these missing values should be inputed.

my_data <- data.frame(Col1 = c(1, 2, NA, 5), Col2 = c(1, NA, 5, 6), Col3 = c("here", "there", "here", "yes"))
my_data
#   Col1 Col2  Col3
# 1    1    1  here
# 2    2   NA there
# 3   NA    5  here
# 4    5    6   yes
is.na(my_data$Col1) # output: FALSE FALSE  TRUE FALSE --> this shows that at the 3rd index, there is an NA
my_data <- my_data %>% mutate(Col1 = ifelse(is.na(Col1), 0, Col1)) # in the my_data dataset, replace Col1 --> if Col1 has NA, then put 0, otherwise, keep the value in the column
my_data
#   Col1 Col2  Col3
# 1    1    1  here
# 2    2   NA there
# 3    0    5  here
# 4    5    6   yes

mean(my_data$Col2) # NA --> calculating the mean (or anything else) of a column that has NA results in an NA
mean(my_data$Col2, na.rm = TRUE) # 4 --> if you add na.rm = TRUE, then it will calculate the mean of a column after excluding the NA

##################
# Class Exercise #  # in my_data, replace the NA in Col2 with the mean of the column
##################


str(my_data) #  The third column of my_data is stored as a character
# 'data.frame':	4 obs. of  3 variables:
# $ Col1: num  1 2 0 5
# $ Col2: num  1 4 5 6
# $ Col3: chr  "here" "there" "here" "yes"

# let's create 2 new columns in which we replace any of values of "here" and "there" to "no"
my_data$Col3 %in% c("here", "there") # TRUE  TRUE  TRUE FALSE --> if the value is any of these (here, there), then return TRUE
my_data <- my_data %>% mutate(Col4 = ifelse(Col3 %in% c("here", "there"), "no", Col3), # replacing values when the column is a character
                              Col3 = as.factor(Col3),  # changing Col3 from factor to character format
                              Col5 = ifelse(Col3 %in% c("here", "there"), "no", Col3)) # replacing values when the column is a character
my_data
#   Col1 Col2  Col3 Col4 Col5
# 1    1    1  here   no   no
# 2    2    4 there   no   no
# 3    0    5  here   no   no
# 4    5    6   yes  yes    3  --> Col 4 has the correct value in it. 
# make sure that when you do this kind of changes to columns that have strings that they are stored as character, not factor!



# change from character to numeric
cust_churn <- cust_churn %>% mutate(TotalCharges = as.numeric(TotalCharges))



############# 
# functions #
#############
# a function takes in arguments as inputs; then it applies a set of statements to the arguments and returns a value
# R has several built-in functions, though these can be created by the user as well


# this is the structure of functions:
# myfunction <- function(arg1, arg2, arg3){
#   statements
#   return(object)
# }

# myFuntion = function name
# arg1, arg2, arg3 = arguments/ inputs of the function; you can denote as many as you wish
# statements = function body that tells what operations to perform with the arguments
# return() = return value; this is the output of the function

myFunction1 <- function(x){
  a <- x + 2
  return(a)
}

myFunction1(2) # 4
myFunction1(6) # 8
myFunction1(c(2,3,4)) # 4 5 6

myFunction2 <- function(x, y){
  a <- x + 3
  b <- y + 2
  c <- a * b
  return(c)
}

myFunction2(4, 3) # 35 = (4 + 3) * (3 + 2)
myFunction2(c(1,2,3,4), c(2,3,4,5)) # 16 25 36 49


##################
# class exercise # Create a function that takes in 2 elements and returns the element that has the highest value
##################






######### 
# loops #
#########
# Loops are used in order to automate a process by applying certain statements multiple times.
# There are 2 types of loops: for loops and while loops.
# For loops are executed a certain number of times
# While loops are executed as long as a condition is met

##### for loops --> repeats a certain number of times
# 1:10 provides a sequence of all numbers from 1 to 10: 1  2  3  4  5  6  7  8  9 10
# this loop has an index called i that will take in any of the values in the sequence
# at first iteration, i = 1, then enter loop, which says print(i), which means print(1)
# since print(i) is the only code inside the loop, the first iteration is over after 1 is printed
# second iteration starts and i takes the value of 2; then, enter the loop and print(2)
# start 3rd iteration etc
for(i in 1:10){
  print(i)
}


# in this loop, i takes values from 1 to 5; at the first iteration, i = 1 and x = 1 * 2 = 2
# at the second iteration, i = 2 and x = 4 etc
for(i in 1:5){
  x = i * 2
  print(x)
}
x # after the loop is ended, x has the last value it had in the loop, which is 10
i # i has the last number in the sequence, which is 5


##################
# class exercise # create a loop in which the index takes values from 1 to the number of rows of the data frame x
################## at each iteration, multiply the first and the second column and print the results
# suppose you have the following data frame, x
x <- data.frame(column1 = c(1,2,3,4), column2 = c(3,6,3,2))
x



# the output of the loop should look like this:
# your results would look like this:
# [1] 3
# [1] 12
# [1] 9
# [1] 8
# # iteration 1: 1 * 3 = 4
# iteration 2: 2 * 6 = 12
# iteration 3: 3 * 3 = 9
# iteration 4: 4 * 2 = 8



##### while loops --> repeats as long as a condition is met
# while loops contain a logical condition that they evaluate
# if the condition is met, then the loop is executed, otherwise it stops from execution

# start with an index i equal to 1
# print "BSAN 460" as long as i <= 5
# at each iteration, the value of i increases by 1
i = 1
while (i <= 5){
  print("BSAN 460")
  i = i + 1
}



####### 
# sql #
#######
# read more about joining capabilities of dplyr package here: https://dplyr.tidyverse.org/reference/join.html
# when working with multiple datasets for the same project, there's often the need to merge the datasets
# to learn more about sql, learn about the following statements: select, from, where, group by, having, order by
# below, we will cover different types of joins; filtering and summarizing data can be done using dplyr package in R

# there are 4 different types of join:
# left join:
# return all rows from x, and all columns from x and y. 
# Rows in x with no match in y will have NA values in the new columns. 
# If there are multiple matches between x and y, all combinations of the matches are returned.

# right join:
# return all rows from y, and all columns from x and y. 
# Rows in y with no match in x will have NA values in the new columns. 
# If there are multiple matches between x and y, all combinations of the matches are returned.

# inner join:
# return all rows from x where there are matching values in y, and all columns from x and y. 
# If there are multiple matches between x and y, all combination of the matches are returned.

# full/ outer join:
# return all rows and all columns from both x and y. 
# Where there are not matching values, returns NA for the one missing.


# let's create 2 datasets and then join them
data1 <- data.frame(class = c("BSAN 460", "MGMT 200", "MGMT 200", "ACCT 115", "MATH 101"), studentID = c(123, 423, 236, 432, 432))
data2 <- data.frame(lecture = c("BSAN 460", "MGMT 200", "ACCT 115", "ECON 200"), professor = c("Ann", "Chris", "Matt", "Sybel"))
data1
data2


# left join
data1 %>% left_join(data2, by = c("class" = "lecture"))
# same as:
left_join(data1, data2, by = c("class" = "lecture"))

# right join
data1 %>% right_join(data2, by = c("class" = "lecture"))


##################
# class exercise # Above, we did a right join; Transform the right join in a left join while getting the same information back
##################




# inner join
data1 %>% inner_join(data2, by = c("class" = "lecture"))



# full join
data1 %>% full_join(data2, by = c("class" = "lecture"))



##################
# class exercise # Merge data1 and data2 to create a data frame that contains 3 columns: class, studentID and professor
################## Keep only the records that are for the MGMT 200 and BSAN 460 class






rm(list=ls())

#######################
# Homework exercise 1 # 
#######################
# Take the iris dataset and add the Sepal.Length and the Sepal.Width columns.
# The new column should be names Sepal.Length.plus.Width.
# The new data should be named as my_data.
# The first 3 rows of the output look like this:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species new_column new_column2 Sepal.Length.plus.Width
# 1          5.1         3.5          1.4         0.2  setosa        6.1        high                     8.6
# 2          4.9         3.0          1.4         0.2  setosa        5.9         low                     7.9
# 3          4.7         3.2          1.3         0.2  setosa        5.7         low                     7.9

my_data <- iris

my_data$Sepal.Length.plus.Width <- my_data$Sepal.Length + my_data$Sepal.Width

head(my_data, 3)



#######################
# Homework exercise 2 # 
#######################
# Sort my_data by the Sepal.Width column and save the output in my_data. Hint: use arrange().
# Output:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species new_column new_column2 Sepal.Length.plus.Width
# 1            5         2.0          3.5         1.0 versicolor          6        high                     7.0
# 2            6         2.2          4.0         1.0 versicolor          7        high                     8.2
# 3            6         2.2          5.0         1.5  virginica          7        high                     8.2
library(plyr)

my_data <-arrange(my_data, Sepal.Width)


head(my_data, 3)



#######################
# Homework exercise 3 # 
#######################
# Sort my_data descending by the Sepal.Width column and save the output in my_data. Hint: use arrange().
# Output:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species new_column new_column2 Sepal.Length.plus.Width
# 1          5.7         4.4          1.5         0.4  setosa        6.7        high                    10.1
# 2          5.5         4.2          1.4         0.2  setosa        6.5        high                     9.7
# 3          5.2         4.1          1.5         0.1  setosa        6.2        high                     9.3

my_data <- arrange(my_data, desc(Sepal.Width))

head(my_data, 3)


#######################
# Homework exercise 4 # 
#######################
# Rename the column Sepal.Width to Sepal_Width in the my_data data frame and save as my_data.
# Output:
#   Sepal.Length Sepal_Width Petal.Length Petal.Width Species new_column new_column2 Sepal.Length.plus.Width
# 1          5.7         4.4          1.5         0.4  setosa        6.7        high                    10.1
# 2          5.5         4.2          1.4         0.2  setosa        6.5        high                     9.7
# 3          5.2         4.1          1.5         0.1  setosa        6.2        high                     9.3

library(tidyverse)

names(my_data)[2] <- "Sepal_Width"
head(my_data, 3)


#######################
# Homework exercise 5 # 
#######################
# Calculate the average Sepal_Width and Sepal.Length.plus.Width for each Species.
# Output:
# Species    mean_width mean_length_width
# 1 setosa           3.43              8.43
# 2 versicolor       2.77              8.71
# 3 virginica        2.97              9.56


my_data %>%
  group_by(Species) %>%
  summarise_at(vars(Sepal_Width, Sepal.Length.plus.Width), list(name = mean))

#######################
# Homework exercise 6 # 
#######################
# Read  the Customer Churn file again
# run str(cust_churn)
# How many observations does that data have?  - 0.25 points
# How many columns does the data have?  - 0.25 points
# What is the format of each variable (int, factor, character etc)?  - 0.5 points

setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")

cust_churn <- read.csv("Customer Churn.csv")

str(cust_churn)

#There are a total of 5298 observations in the data.
#There are 18 columns
#CustomerID = character
#Churn = Factor
#Country = character
#State = character
#Retired = Factor
#Has Partner = Factor
#Has Dependents = Factor
#Education = character
#Gender = Factor
#BaseChargers = integer
#DOC - integer
#ElectronicBilling = Factor
#ContractType = character
#PaymentMethod = character
#TypeOfService = character
#ServiceDetails = character



####################### 
# Homework exercise 7 # 
#######################
# Check what columns have NA
# Notice that the ContractType column has 5 NA's
# Replace the NA's in the ContractType column with the word "Unknown", otherwise keep the original value - 0.5 points
# Hint: you can use the following: mutate, ifelse, is.na; make sure the column is in the right format before making changes
# Get the counts of each category within the ContractType column - 0.5 points
# You should now have 5 records with the value "Unknown" 
# While the counts of the other categories should remain the same

cust_churn %>%
  summarise_all(funs(sum(is.na(.))))

cust_churn$ContractType <- cust_churn$ContractType %>% replace_na('Unknown')

table(cust_churn$ContractType)

#######################
# Homework exercise 8 # 
#######################
# Using the cust_churn dataset, remove the rows that have NA in the TotalCharges column. Save the output in cust_churn
# Run the summary() and str() function. Your dataset should now have 2 rows less and no NA in the TotalCharges column.
# Hint: you can use the following: filter, is.na

cust_churn <- cust_churn[- grep("MISSINGVAL", cust_churn$TotalCharges),]

summary(cust_churn)
str(cust_churn)


#######################
# Homework exercise 9 #  
#######################
# Check for the Education column how many times each category shows up
# Hint: you can use count() from dplyr or group_by() with summarise()

cust_churn %>%
  count(Education)


########################
# Homework exercise 10 #  
########################
# Change those categories of Education that show up less than 50 times to "Unknown" - 0.5 points
# Hint: check the type of the column; check how many times each instance shows up using count() and then you can use mutate, ifelse, %in%
# After you make the change, check how many times "Unknown" shows up in the Education column. - 0.5 points
# (it should be 51 times)

cust_churn$Education <- mapvalues(cust_churn$Education, from = c(0, "Other"), to = c("Unknown", "Unknown"))

cust_churn %>%
  count(Education)

########################
# Homework exercise 11 # 
########################
# In the previous lecture, you lerned about group_by() used along with summarise(). However, you can use group_by() along with other functions as well, for example mutate()
# Assume you want to create 2 columns, in one of them you calculate the average expected mean BaseCharge per education level, while in the other column the median for the entire column
# See below 3 ways of writing code, what effect does ungroup() have on the output data? Which version do you think would be best practice?

# version 1
churning <- read.csv("Customer Churn.csv")
churning <- churning %>% group_by(Education) %>% mutate(mean_charge = mean(BaseCharges))
churning <- churning %>% mutate(median_charge = median(BaseCharges))

# version 2
churning2 <- read.csv("Customer Churn.csv")
churning2 <- churning2 %>% group_by(Education) %>% mutate(mean_charge2 = mean(BaseCharges)) %>% ungroup()
churning2 <- churning2 %>% mutate(median_charge2 = median(BaseCharges))

# version 3
churning3 <- read.csv("Customer Churn.csv")
churning3 <- churning3 %>% group_by(Education) %>% mutate(mean_charge = mean(BaseCharges), median_charge = median(BaseCharges)) %>% ungroup()

#Ungroup can be used to reverse the effects of group by statement. If theres any sort of aggregation going on,
#ungroup will exclude that field from it. In practice the first version would probably be the best version because
# it is the most readable version of the code. So if someone else was reading your code they would know what is happening


########################
# Homework exercise 12 # 
########################
# Research online what the main sql statements are used for: select, from, where, group by, having, order by
# Write 1-2 sentences about what each of these statements is doing. - 0.5 points
# What is the difference between where and having statements? - 0.5 points

#Select is used to decide which columns you want to include in your data
#From is used to tell the query where to look for the data
#Where is used to oass a condition to limit the scope of your search or results
#Group by is used to aggregate the data by the values of a certain column(s)
#Having is used to limit the result when you use aggregation
#Order by is used to return the data in the order of a specific column(s)

#The differrence between the two is that you have to use the having clause when there
#is any sort of aggregation statement in your query. For exxample if you use
#sum you can use the Having to say only show me rows that have a sum of over 50