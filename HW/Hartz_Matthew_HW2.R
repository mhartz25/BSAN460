### BSAN 460 -- Business Analytics Senior Project
### Winter 2021
### Week 2 -- Text Pre-Processing
### Prof. Irina Nedelcu

# Before doing any text modeling, it is important to look over the data to get familiarized with it as well as clean it.
# Save the text_emotion.csv file posted on BbLearn to your computer.

# We will be using Regular Expressions (Regex) to make modifications to text. Check this R cheatsheet to see how to apply different regex rules: 
# https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# install and load the needed libraries
#install.packages("tm")
#install.packages("textstem")
library(dplyr)
library(tm) # provides list of stopwords in the English dictionary; https://cran.r-project.org/web/packages/tm/tm.pdf
library(textstem) # helps with stemming and lemmatization; https://cran.r-project.org/web/packages/textstem/textstem.pdf


# set the working directory and read files
setwd("D:/Teaching Courses/BSAN 460 - Capstone - Fall 2021") # change the path as needed; if you copy paste the path from your folder, change \ to /
text_emotion <- read.csv("text_emotion.csv", stringsAsFactors = FALSE)

# check the structure of the data
str(text_emotion)


# We will do the following pre-processing to the tweets step by step and look at how each pre-processing affects the output:
# bring to lower case
# remove numbers
# remove stopwords
# remove tags
# remove URLs
# remove punctuation
# remove certain words
# remove white space
# apply stemming/ lemmatization


############################
# bring text to lower case #
############################
# save the changes to a new column, called new_Content to be able to compare the before and after
text_emotion <- text_emotion %>% mutate(new_Content_lower = tolower(Content))


##################
# remove numbers #
##################
# this function looks for any number in the text and then removes it
# if desired to replace the numbers with a space, add a space inside the quotes: ' ' instead of ''
text_emotion <- text_emotion %>% mutate(new_Content_noNumbers = gsub('[[:digit:]]','',new_Content_lower)) # gsub(): http://www.endmemo.com/program/R/gsub.php


####################
# remove stopwords #
####################
# let's see what stopwords are in the default stopwords of R.
stopwords('en')


# You can subset the stopwords vector as any other vector. The stopwords are stored in a vector with character format. 
# You can also add new words to the stopwords list.
str(stopwords('en'))

# subset the stopwords:
stopwords('en')[1:10]

# this returns a boolean vector; if it finds a given string at any position, it returns TRUE, otherwise FALSE
!stopwords('en') %in% c('i') # ! means negations, so makes the TRUE become a FALSE and the FALSE to become TRUE

# remove certain stopwords:
stopwords('en')[!stopwords('en') %in% c('i')]  # notice that the first stopword, i, was removed
# stopwords('en') %in% c('i') ---> this gives back a vector with TRUE and FALSE
# ! ---> this gives negation


# add new stopwords:
c(stopwords('en'), "under")  # notice that the stopwords have a new element: under

# Let's now remove the stopwords:
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
text_emotion <- text_emotion %>% mutate(new_Content_noStopWords = gsub(stopwords_regex,'',new_Content_noNumbers)) # explanation on how gsub() works: http://www.endmemo.com/program/R/gsub.php


################## What happens if we don't collapse stopwords? 
# class exercise # What if inside the gsub() function we put stopwords('en') instead of stopwords_regex?
################## Apply gsub() and create a new column, new_Content_noStopWords_noCollapse, which contains the output.




# Let's remove the new_Content_noStopWords_noCollapse column:
text_emotion <- text_emotion %>% select(-new_Content_noStopWords_noCollapse)


####################
# remove @username #
####################
# Before removing punctuation, remove the tags from the text.
text_emotion <- text_emotion %>% mutate(new_Content_noTags = gsub('@\\w*','',new_Content_noStopWords))


#########################
# remove URLs from text #
#########################
text_emotion <-text_emotion %>% mutate(new_Content_noURL = gsub('http.*\\s','',new_Content_noTags))


######################
# remove punctuation #
######################
text_emotion <-text_emotion %>% mutate(new_Content_noPunctuation = gsub('[[:punct:]]','',new_Content_noURL))


#############################
# Replace words within text #
#############################
# If there are words that have typos, let's change them to the correct words.
text_emotion <-text_emotion %>% mutate(new_Content_noTypos = gsub('fb','facebook',new_Content_noPunctuation))



############################
# Remove extra white space #
############################
# (this would include space, tab, vertical tab, newline, form feed, carriage return):
text_emotion <-text_emotion %>% mutate(new_Content_noSpaces = gsub('\\s+',' ',new_Content_noTypos))



################## In the code above, we remove white space and then add a space.
# class exercise # What happens if we replace whitespace with nothing? Save the output in a collumn named new_Content_noSpaces2.
##################





################## 
# class exercise # Remove column new_Content_noSpaces2.
##################



######################
# Apply lematization #
######################
text_emotion <-text_emotion %>% mutate(new_Content_Lemma = lemmatize_strings(new_Content_noSpaces))


##################
# Apply stemming #
##################
text_emotion <-text_emotion %>% mutate(new_Content_Stem = stem_strings(new_Content_noSpaces))


# Keep just the Sentiment and new_Content_Lemma columns for further modeling:
text_emotion <- text_emotion %>% select(Sentiment, new_Content_Lemma)



# transform the text_emotion data frame to corpus. 
# We need to transform it to corpus because the DocumentTermMatrix() function takes a corpus as an input.
corp <- Corpus(VectorSource(text_emotion$new_Content_Lemma))



##########
# Tf-Idf #  Once you create Tf-Idf, you can use it as input to predictive models
##########
# create the Tf-Idf
my_tfidf <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf))
my_tfidf # documents: 10374, terms: 11614


# If data is bigger, Tf-Idf can get quite large, since it has a column for each term
# Tf-Idf has many terms, so lets, remove those that are sparse, those that appear only a few times
# 0.99 gives you 110 terms. change the number to lower or higher to get less or more terms 
# # 0.99 tells that a column must have at most 99% of the columns with 0 in them
my_tfidf_small <-  removeSparseTerms(my_tfidf, 0.99) 
my_tfidf_small  # documents: 10374, terms: 110


# transform the my_tfidf_small into a data frame
my_data_frame <- as.data.frame(as.matrix(my_tfidf_small))
my_data_frame <- cbind(sentiment = text_emotion$Sentiment, my_data_frame)
str(my_data_frame) # there are 111 columns: 1 for the sentiment and 110 for terms

# Now, that the data is stored as a tf-idf, we can use it in predictive modeling.
# The Sentiment column would be the dependent variable, while the other columns are the independent variables.


rm(list=ls())
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")
text_emotion <-read.csv('text_emotion.csv')

####################### 
# Homework exercise 1 # 
#######################
# Do you believe that stemming or lemmatization is better? Why? 
# Give some examples of differences between stemming and lemmatization from the text_emotion data frame.

#I think lemmatization is better because it account for the type of speech that the word is being used as.
#Context is super important in the English language, so I think that retaining as much context as possible
#is the best strategy. Which is why I would favor lemmatization over stemming

head(text_emotion)
#An example of where the two methods might differ can be found in the 6th row of the dataset. The text includes the
#phrase 'gave me chills'. If part of speech was disregarded it is possible that we could end up interpreting 
#the word chills to be a verb, which could completely alter our interpretation of the sentence. By retaining
#the part of speech it is less likely this happens.

####################### 
# Homework exercise 2 # 
#######################
# When we first transformed the text into Tf-Idf, the dataset had 11,614 terms/ columns (a column was created for each term)
# Once we applied this, removeSparseTerms(my_tfidf, 0.99), we ended up with a dataset that has 110 terms
# This shows that most of columns in the dataset are sparse; more than 11k column are sparse.
# Why is this happening?

#When we use this function it removes all off the terms that have a sparsity above that threshold. In this case using .99 
# essentially means that the word only needs to appear a few times to not be removed. The main reason
#most of the terms were still removed is due to the presence of @ names in the dataset. These 2 names are unique
#and are present in most of the text, so this could lead to much more of the terms to be removed

####################### 
# Homework exercise 3 # 
#######################
# Suppose you have the data below that has 3 documents. 
# Below, there is also code to remove stopwords; 
# modify the stopwords dictionary so that you add the word "want" to the dictionary
# then, remove the stopwords from the column Text. Save the results to column Text3
# (check at the beginning of the workbook on how to add words to the dictionary)

install.packages("tm")
library(tm)

new_data <- data.frame(Text = c("i am at the cafeteria", 
                                "today i am doing the assignment", 
                                "where do you want to eat"))
new_data
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')

c(stopwords('en'), "want")
new_data <- new_data %>% mutate(Text2 = gsub(stopwords_regex,'',Text))

new_data <- new_data %>% mutate(Text3 = sub(stopwords_regex,'',Text))

new_data

####################### 
# Homework exercise 4 # 
#######################
# Give an example in which you would want to replace a word with another in text.

#There are many examples where you might want to replace a word with another in text
#One specific example could be in the case of text slang. If someone uses the letters 'tn'
#to mean the word 'tonight' that would be something that is worth replacing

####################### 
# Homework exercise 5 # 
#######################
# What is Tf-Idf? (write a few sentences)

#Tf-idf is a statistical measure that is used in text mining and NLP to figure out 
#how often a word appear in a body of text or documents, and in turn understand how important it is.
#It is calculated by using the frequency of a word and comparing it to its relative frequency
#The reason for doing this is because if a word appears many times in a certain document, but not often in other documents
#this could be a sign that the word is very important in that particular document.
