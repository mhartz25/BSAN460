#BSAN 460 Final Project

#Clear Environment
rm(list=ls())

#Set working Directory
setwd("/Users/matthewhartz/Documents/r_practice/BSAN460/data")


#Load Packages
#install.packages("tm")
#install.packages("textstem")
#install.packages("tidytext")
#install.packages("wordcloud")
#install.packages("textdata")
library(dplyr)
library(tm)
library(textstem)
library(tidytext) 
library(textdata)
library(wordcloud)

#Load in IMDB Dataset
df <- read.csv("IMDB_Dataset.csv")


#Check df's structure
str(df)

#Are the tags of the dataset balanced
table(df$sentiment)


# Create a function that performs all the pre-processing techniques to an entire column
pre_processing_data_frame_fct <- function(text_column){
  text_column <- tolower(text_column) # bring all the words to lower case
  text_column <- gsub('[[:digit:]]','',text_column) # remove numbers
  text_column <- gsub(paste(stopwords('en'), collapse = '\\b|\\b'),'',text_column) # remove stopwords
  text_column <- gsub('[[:punct:]]','',text_column) # remove punctuation
  text_column <- gsub('\\s+',' ',text_column) # remove white space
  
  text_column <- gsub('<.+>',' ',text_column) # remove HTML tags
  
  text_column <- lemmatize_strings(text_column) # lemmatize text
  corp <- Corpus(VectorSource(text_column)) # transform to corpus
  return(corp)
}


### Apply the pre-processing function to my_data and save the output to my_data_clean
df_clean <- pre_processing_data_frame_fct(df$review)
df_clean



#-----------------------------------------------------------------------

# transform the clean data into a term document matrix
my_tdm <- TermDocumentMatrix(df_clean)




#  this gives a row for each term-document combination and the number of times each term appears in each document
tidy_frame <- tidy(my_tdm)
head(tidy_frame)
str(tidy_frame) 
#-----------------------------------------------------------------------


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
df <- df %>% mutate(document = row_number()) 
my_sentiments <- my_sentiments %>% mutate(document = as.numeric(document))
my_sentiments <- full_join(my_sentiments, df %>% select(document, sentiment),
                           by = c("document" = "document"))



### Replace all NAs in the dictionary columns with 0
my_sentiments <- my_sentiments %>% mutate_at(vars(score_bing, score_loughran, score_afinn),  ~ if_else(is.na(.), 0, .))
head(my_sentiments)


#-----------------------------------------------------------------------
### Create a wordcloud
# to create a wordcloud, we need to know the list of words and how many times each word shows up
# to do so, we can use the term and count columns from my_sentiments data 
# currently, my_sentiments data, shows the counts of words for each document
# we nee the total counts of each word, so let's sum count for each term
cloud_data <- my_sentiments %>% group_by(term) %>% summarise(counts = sum(count)) %>% ungroup()
cloud_data <- cloud_data %>% filter(!is.na(term))  # there is an NA in the term column; let's remove this row, otherwise you will have issues plotting the wordcloud
head(cloud_data %>% arrange(-counts))  # these are the most common words
wordcloud(words=cloud_data$term, freq=cloud_data$counts, random.order=FALSE, colors=brewer.pal(7, "Greens"), max.words = 70, min.freq = 20)

#-----------------------------------------------------------------------
