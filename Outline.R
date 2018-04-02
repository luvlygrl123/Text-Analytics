#-------------------------------------------------------------------------
# Install Packages
#-------------------------------------------------------------------------

library(twitteR)
library(rtweet)
library(data.table)
library(stringr)
library(tidytext)

#-------------------------------------------------------------------------
# 2,000 Tweets
#-------------------------------------------------------------------------

# keys
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# search tweets
tweets <- searchTwitter("",n=2000)

# convert to dataframe
tweets.df <- twListToDF(tweets)

# write to CSV
write.csv(tweets.df, file = "location.csv",row.names=FALSE)

# columns will be:
#    [text, favorited, favoriteCount, replyToSN, created, truncated, 
#    replyToSID, id, replyToUID, statusSource, screenName, retweetCount, 
#    isRetweet, retweeted, longitude, latitude]

#--------------------------------------------------------------------------------------------------------------------------------------------------
# THE REST SHOULD BE ON A NEW DOCUMENT ############################################################################################################
#--------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Install Packages
#-------------------------------------------------------------------------

library(wordcloud)
library(tm)
library(ggplot2)
library(rtweet)
library(SnowballC)
library(data.table)
library(stringr)
library(qdap)
library(tibble)
library(RWeka)
library(lubridate)
library(lexicon)
library(tidytext)
library(gutenbergr)
library(dplyr)
library(radarchart)

setwd("point/to/folder/")


tweet <- read.csv(file="papajtweets.csv", header=TRUE, sep=",")

#-------------------------------------------------------------------------
# Cleaning
#-------------------------------------------------------------------------

# remove non alpha numeric characters 
tweet$text <- iconv(tweet$text, from = "UTF-8", to = "ASCII", sub = "")

# making a corpus of a vector source 
tweet_corp <- VCorpus(VectorSource(tweet$text))
# Cleaning corpus - pre_processing
clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_number))
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  # available stopwords
  # stopwords::stopwords()
  
  # Add any custom stop words here
  custom_stop_words <- c("")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  # cleaned_corpus <- tm_map(cleaned_corpus, stemDocument,language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}


cleaned_tweet_corp <- clean_corpus(tweet_corp)

# TDM/ DTM ---------------------------------------------------
TDM_Tweets <- TermDocumentMatrix(cleaned_tweet_corp)
TDM_Tweets_m <- as.matrix(TDM_Tweets)

# Term Frequency ---------------------------------------------
term_frequency <- rowSums(TDM_Tweets_m)

#-------------------------------------------------------------------------
# Unigram
#-------------------------------------------------------------------------

word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=2000,colors=brewer.pal(10,"BrBG"))

#-------------------------------------------------------------------------
# Bigram
#-------------------------------------------------------------------------

tokenizer2 <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2)) # this is number of words shown together

bigram_tdm <- TermDocumentMatrix(cleaned_tweet_corp,control = list(tokenize=tokenizer2))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency
term_frequency2 <- rowSums(bigram_tdm_m)
# Sort term_frequency in descending order
term_frequency2 <- sort(term_frequency2,dec=TRUE)

# Create word_freqs
word_freqs2 <- data.frame(term = names(term_frequency), num = term_frequency2)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs2$term, word_freqs2$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "BrBG"))

#-------------------------------------------------------------------------
# Trigram
#-------------------------------------------------------------------------

tokenizer3 <- function(x)
  NGramTokenizer(x,Weka_control(min=3,max=3)) # this is number of words shown together

trigram_tdm <- TermDocumentMatrix(cleaned_tweet_corp,control = list(tokenize=tokenizer3))
trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency
term_frequency3 <- rowSums(trigram_tdm_m)
# Sort term_frequency in descending order
term_frequency3 <- sort(term_frequency3,dec=TRUE)

# Create word_freqs
word_freqs3 <- data.frame(term = names(term_frequency3), num = term_frequency3)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs3$term, word_freqs3$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "BrBG"))

#-------------------------------------------------------------------------
# TF - IDF
# Term Frequencies and Inverse Document Frequencies
#-------------------------------------------------------------------------

# Unigram --------------------------------------------------------------
print("------------\nunigram:\n------------\n")

tfidf_tdm <- TermDocumentMatrix(cleaned_tweet_corp,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))


# Bigram --------------------------------------------------------------
print("------------\nbigram:\n------------\n")

tfidf_tdm2 <- TermDocumentMatrix(cleaned_tweet_corp,control = list(tokenize=tokenizer2, weighting=weightTfIdf))
tfidf_tdm2_m <- as.matrix(tfidf_tdm2)

# Term Frequency
term_frequency2 <- rowSums(tfidf_tdm2_m)
# Sort term_frequency in descending order
term_frequency2 <- sort(term_frequency2,dec=TRUE)

# Create word_freqs
word_freqs2 <- data.frame(term = names(term_frequency2), num = term_frequency2)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs2$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))


# Trigram -------------------------------------------------------------
print("------------\ntrigram:\n------------\n")

tfidf_tdm3 <- TermDocumentMatrix(cleaned_tweet_corp,control = list(tokenize=tokenizer3, weighting=weightTfIdf))
tfidf_tdm3_m <- as.matrix(tfidf_tdm3)

# Term Frequency
term_frequency3 <- rowSums(tfidf_tdm3_m)
# Sort term_frequency in descending order
term_frequency3 <- sort(term_frequency3,dec=TRUE)

# Create word_freqs
word_freqs3 <- data.frame(term = names(term_frequency3), num = term_frequency3)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs3$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))

#-------------------------------------------------------------------------
# Positive and Negatives
#-------------------------------------------------------------------------



#-------------------------------------------------------------------------
# Split into 2 sets (positive and negative)
#-------------------------------------------------------------------------



#-------------------------------------------------------------------------
# Comparison Cloud (positive and negative)
#-------------------------------------------------------------------------



#-------------------------------------------------------------------------
# Commonality Cloud (shared words)
#-------------------------------------------------------------------------



#-------------------------------------------------------------------------
# Radar Chart
#-------------------------------------------------------------------------



