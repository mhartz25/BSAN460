### BSAN 460 -- Business Analytics Senior Project
### Winter 2021
### Week 4 -- Sentiment Analysis
### Prof. Irina Nedelcu

# There are two ways of doing sentiments analysis: lexicon-based and learning-based. 
# These allow to determine the sentiment/ opinion of text. 
# Lexicon-based approach is an unsupervised method, while learning-based approaches are supervised models.
# 
# In this notebook, we will implement lexicon-based method to evaluate whether text has 
          # positive or negative meaning.
# 
# Load/ install packages that we will need in the analysis:
#install.packages("tm")
install.packages("textstem")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("textdata")
library(dplyr)
library(tm)
library(textstem)
library(tidytext) 
library(textdata)
library(wordcloud)


# set the working directory and read files
setwd("D:/Teaching Courses/BSAN 460 - Capstone - Fall 2021") # change the path as needed; if you copy paste the path from your folder, change \ to /
text_emotion <- read.csv("text_emotion.csv")

# check the structure of the data
str(text_emotion)


# Create a function that performs all the pre-processing techniques to an entire column
pre_processing_data_frame_fct <- function(text_column){
  text_column <- tolower(text_column) # bring all the words to lower case
  text_column <- gsub('[[:digit:]]','',text_column) # remove numbers
  text_column <- gsub(paste(stopwords('en'), collapse = '\\b|\\b'),'',text_column) # remove stopwords
  text_column <- gsub('[[:punct:]]','',text_column) # remove punctuation
  text_column <- gsub('\\s+',' ',text_column) # remove white space
  text_column <- lemmatize_strings(text_column) # lemmatize text
  corp <- Corpus(VectorSource(text_column)) # transform to corpus
  return(corp)
}


### Apply the pre-processing function to my_data and save the output to my_data_clean
my_data_clean <- pre_processing_data_frame_fct(df$review)
my_data_clean

# transform the clean data into a term document matrix
my_tdm <- TermDocumentMatrix(my_data_clean)

# a tdm has a row for each term and a column for each document...since the text_emotion data has 
  # 10k rows, then the new tdm has 10k columns


#  this gives a row for each term-document combination and the number of times each term appears in each document
tidy_frame <- tidy(my_tdm)
head(tidy_frame)
str(tidy_frame) # this frame has roughly 75k rows (term-document combination)



##### tidytext
# tidytext is one of the R packages that is widely used to detect sentiment in text. 
# You can read more about how to use the tidytext package here: 
# https://cran.r-project.org/web/packages/tidytext/tidytext.pdf
# tidytext package has a few sentiment lexicons, some of them providing categorical sentiment assignment 
      # (bing, loughran), while others provide numerical sentiment scores (afinn).
# negative values show a negative sentiment, while positive values show a positive sentiments


# Categorical sentiment assignment:
# bing
sentiment_bing <- get_sentiments("bing")
sentiment_bing

# loughran
sentiment_loughran <- get_sentiments("loughran") 
sentiment_loughran

# Numerical value sentiment assignment:
# afinn
sentiment_afinn <- get_sentiments("afinn") 
sentiment_afinn

### change the format of the data frames that store the values of the sentiments to later merge them
# bing
sentiment_bing <- sentiment_bing %>% rename(score_bing = sentiment) %>% 
                                     mutate(score_bing = ifelse(score_bing == "negative", -1, 1))
head(sentiment_bing)
tail(sentiment_bing)
#loughran
unique(sentiment_loughran$sentiment)
sentiment_loughran <- sentiment_loughran %>% rename(score_loughran = sentiment) %>% 
                                             filter(score_loughran %in% c("negative", "positive")) %>% 
                                             mutate(score_loughran = ifelse(score_loughran == "negative", -1, 1))
head(sentiment_loughran)
tail(sentiment_loughran)
# afinn
sentiment_afinn <- sentiment_afinn %>% rename(score_afinn = value)
head(sentiment_afinn)
tail(sentiment_afinn)

### put the scores from all 3 dictionaries in 1 data frame
sentiments <- full_join(sentiment_bing, sentiment_loughran, by = c("word" = "word"))
sentiments <- full_join(sentiments, sentiment_afinn, by = c("word" = "word"))
head(sentiments)
tail(sentiments)



# you can use the tidy frame to bring the sentiment to it
# the tidy_frame and sentiment_bing data sets have the term and word columns in common
head(tidy_frame)
head(sentiments)
tidy_frame$document <- as.numeric(tidy_frame$document)

#merge the tidy_frame and sentiment_bing data sets; use by.x = "term" and by.y = "word to indicate the column 
# that is the same in both data sets. Keep all the rows from tidy_frame and bring the matching results from 
# the sentiment_bing data set
my_sentiments <- left_join(tidy_frame, sentiments, by = c("term" = "word"))
my_sentiments <- my_sentiments %>% arrange(document, term) # sort the data by the document and term columns
head(my_sentiments)
str(my_sentiments)


# bring the sentiment column to my_sentiments
# in the initial dataset, text_emotion, create a column that represents the row number; 
  # this equals the number of the documents and we will use this column to be able to bring the sentiment
  # column to the data frame that  has the sentiment scores
text_emotion <- text_emotion %>% mutate(document = row_number()) 
my_sentiments <- my_sentiments %>% mutate(document = as.numeric(document))
my_sentiments <- full_join(my_sentiments, text_emotion %>% select(document, Sentiment),
                           by = c("document" = "document"))



### Replace all NAs in the dictionary columns with 0
my_sentiments <- my_sentiments %>% mutate_at(vars(score_bing, score_loughran, score_afinn),  ~ if_else(is.na(.), 0, .))
head(my_sentiments)

### Create a wordcloud
# to create a wordcloud, we need to know the list of words and how many times each word shows up
# to do so, we can use the term and count columns from my_sentiments data 
# currently, my_sentiments data, shows the counts of words for each document
# we nee the total counts of each word, so let's sum count for each term
cloud_data <- my_sentiments %>% group_by(term) %>% summarise(counts = sum(count)) %>% ungroup()
cloud_data <- cloud_data %>% filter(!is.na(term))  # there is an NA in the term column; let's remove this row, otherwise you will have issues plotting the wordcloud
head(cloud_data %>% arrange(-counts))  # these are the most common words
wordcloud(words=cloud_data$term, freq=cloud_data$counts, random.order=FALSE, colors=brewer.pal(7, "Greens"), max.words = 70, min.freq = 20)

# there are 4 terms that have NA, let's remove these rows; these NAs came from data cleaning
colSums(is.na(my_sentiments))  
my_sentiments <- my_sentiments %>% filter(!is.na(term))

### Calculate the lexicon scores per document per word; 
# multiply the number of times each word shows up in a document by the score of each dictionary
my_sentiments <- my_sentiments %>% mutate(score_bing = count * score_bing,
                                          score_loughran = count * score_loughran,
                                          score_afinn = count * score_afinn)
head(my_sentiments)



# as of now, we have a score for each of the 3 dictionaries per word-document combination
# what we need is a score for each dictionary for  each document
# to find this out, we need to sum up the scores per document
my_sentiments <- my_sentiments %>% group_by(document, Sentiment) %>% 
                                   summarise(sum_score_bing = sum(score_bing),
                                             sum_score_loughran = sum(score_loughran),
                                             sum_score_afinn = sum(score_afinn)) %>% 
                                   ungroup()
head(my_sentiments)


# now, my_sentiments data frame has, for each document, a score coming from each of the 3 dictionaries
# if the score is higher than 0, then the dictionary predicts that the sentiment of the document in positive
# if the score is lower than 0, then the dictionary predicts that the sentiment of the document is negative
# if the score is 0, then the dictionary predicts neutral; if you have to predict positive/ negative, then 
    # you can put the neutral in any of these 2 categories



###########################
### Homework Exercise 1 ###
###########################
# Change the content of the sum_score_bing, sum_score_loughran, sum_score_afinn columns.
# if they contain a value that is equal or greater than 0, then change the number to "happiness"
# if they contain a value that is lower than 0, then change the number to "sadness"

rm(list=ls())
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")
text_emotion <- read.csv("text_emotion.csv")



my_sentiments$sum_score_bing <-ifelse(my_sentiments$sum_score_bing >= 0, "happiness" , "sadness")

my_sentiments$sum_score_loughran <-ifelse(my_sentiments$sum_score_loughran >= 0, "happiness" , "sadness")

my_sentiments$sum_score_afinn <-ifelse(my_sentiments$sum_score_afinn >= 0, "happiness" , "sadness")

head(my_sentiments)

###########################
### Homework Exercise 2 ###
###########################
# Calculate how accurate each of the dictionaries is
# Accuracy is calculated by dividing the number of correctly predicted over the total number of instances
    # to find out the number of correctly predicted values you can use the following for bing for example: 
            # sum(my_sentiments$Sentiment == my_sentiments$sum_score_bing)
    # to find out the number of instances, you calculate the number of rows: nrow(my_sentiments)
# As a result, you should get 3 accuracy measures, they are between 0.6 and 0.7


bing_accuracy <- sum(my_sentiments$Sentiment == my_sentiments$sum_score_bing)/nrow(my_sentiments)
loughran_accuracy <- sum(my_sentiments$Sentiment == my_sentiments$sum_score_loughran)/nrow(my_sentiments)
afinn_accuracy <- sum(my_sentiments$Sentiment == my_sentiments$sum_score_afinn)/nrow(my_sentiments)


bing_accuracy #.6815
loughran_accuracy #.6075
afinn_accuracy # .6847

###########################
### Homework Exercise 3 ###
###########################
# Notice that when we analyze the data like this, we are not account for negations
# after cleaning the data, a sentence such as "I don't like pasta" would become "like pasta"
# so, a negative sentence is predicted as a positive sentence
# If you wish to account for negations in the text to improve prediction, how would you do it?

#You would need to ensure that your preprocessing steps do not remove those negative words such as "don't"
#Failure to account for those words can completely change the way your sentence is understood, and can 
#result in a bias towards positive interpretation


###########################
### Homework Exercise 4 ###
###########################
# What are some limitations of doing sentiment analysis using a lexicon based dictionary?

#There are a few limitations in using a lexicon dictionary for sentiment analysis. Mainly the loss of context
#in the analysis. When each word is looked at individually it can make certainly sentences lose their intended
#effect. Also, another downside is that the weight applied to certain words in the dictionary might not make 
#sense for your specific analysis. If you are doing some social media analysis the weights you wanted for specific
#words might not be the same if you were doing an analysis of restuarant reviews. The dictionary based approach
#will not account for this

###########################
### Homework Exercise 5 ###
###########################
# Voting works this way:
# If 2 or more of the lexicons predict positive, then the final prediction will be positive. 
# If 2 or more lexicons predict negative, then the final category will be negative.
# Calculate a column called voting based on the rules above.

my_sentiments$encode_bing <- ifelse(my_sentiments$sum_score_bing == "happiness", 1 , 0)
my_sentiments$encode_loughran <- ifelse(my_sentiments$sum_score_loughran == "happiness", 1 , 0)
my_sentiments$encode_afinn <- ifelse(my_sentiments$sum_score_afinn == "happiness", 1 , 0)

my_sentiments$prediction <- my_sentiments$encode_bing +my_sentiments$encode_loughran +my_sentiments$encode_afinn

my_sentiments$voting <- ifelse(my_sentiments$prediction >= 2, "positive", "negative")

table(my_sentiments$voting) #2281 Negative Predictions, 8089 Positive predictions
###########################
### Homework Exercise 6 ###
###########################
# Voting is often used in modeling in order to improve the accuracy of the results.
# If you are to use a voting method to determine the sentiment of a text, would this improve the results in our data?
# Why do you think this is happening?


#yes I do think this will improve the accuracy of the results. You are ensuring that the majority of the 
#models agree on the final output. You essentially end up with a weighted average, which you use as your final 
#prediction.


###########################
### Homework Exercise 7 ###
###########################
# In class, we talked about the context of the words and how BOW is not accounting for it.
# Read about context of wors in the article(and others): https://www.topcoder.com/thrive/articles/Word%20Embeddings:%20Context%20Matters%20in%20Natural%20Language%20Processing
# Write 3-4 sentences about context of words.

#Context of words is a relatively new method of NLP that can be superior to the BOW method. This method can be used
#to predict words that come before and after certain terms. This can be extremely helpful because it is able to
#retain the intended meaning. Before the method just used a vector of words with their frequency in the corpus.


###########################
### Homework Exercise 8 ###
###########################
# compare the word 'bank' in the following: "bank of the river" and "money in the bank"
# using BOW (Bag of Words), would 'bank' be represented that same in the DTM? - 0.5 points
# using context of words, would 'bank' be represented the same in the vector space? - 0.5 points

#For BOW, bank would be represented in the same way since they are both nouns. There would be no way to retain the original
#divergence in what each word was supposed to represent. 

#For COW, the two words would not be represented in the same way. The presence of the other noun in the phrase will cause
#the model to realize there is a different context being used in each sentence.



###########################
### Homework Exercise 9 ###
###########################
# compare the word 'qeen' and 'king' in the following: "the queen is governing" and "the king is governing"
# using BOW (Bag of Words), would 'queen' and 'king' be represented that same in the DTM? (aka: are there 2 separate columns created for these 2 words?) - 0.5 points
# using context of words, would 'queen' and 'king' be represented the same in the vector space? - 0.5 points


#For BOW the two words would be represented separately in the DTM. The model will not realize that they are effectively
#the same thing.

#For COW it would depend on what sort of model you were using and what sort of customizations you created for it.
#If it was important for your model to treat these phrases as the same thing you could add some code to make sure
#it treated these two words as equals
