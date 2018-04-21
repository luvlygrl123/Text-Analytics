library(wordcloud)
library(tm)
library(ggplot2)
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

setwd("C:/Users/Serina Brenner/Documents/GitHub/Text-Analytics")


mysearch <- read.csv(file="tableau_text.csv", header=TRUE, sep=",")

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Cleaning
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# remove non alpha numeric characters 
mysearch$text <- iconv(mysearch$text, from = "UTF-8", to = "ASCII", sub = "")

# making a corpus of a vector source 
mysearch_corp <- VCorpus(VectorSource(mysearch$text))
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


cleaned_mysearch_corp <- clean_corpus(mysearch_corp)

# TDM/ DTM 
TDM_mysearch <- TermDocumentMatrix(cleaned_mysearch_corp)
TDM_mysearch_m <- as.matrix(TDM_mysearch)

# Term Frequency 
term_frequency <- rowSums(TDM_mysearch_m)

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Unigram
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=2000,colors=brewer.pal(10,"BrBG"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Bigram
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

tokenizer2 <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2)) # this is number of words shown together

bigram_tdm <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer2))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency
term_frequency2 <- rowSums(bigram_tdm_m)
# Sort term_frequency in descending order
term_frequency2 <- sort(term_frequency2,dec=TRUE)

# Create word_freqs
word_freqs2 <- data.frame(term = names(term_frequency2), num = term_frequency2)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs2$term, word_freqs2$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "BrBG"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Trigram
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

tokenizer3 <- function(x)
  NGramTokenizer(x,Weka_control(min=3,max=3)) # this is number of words shown together

trigram_tdm <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer3))
trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency
term_frequency3 <- rowSums(trigram_tdm_m)
# Sort term_frequency in descending order
term_frequency3 <- sort(term_frequency3,dec=TRUE)

# Create word_freqs
word_freqs3 <- data.frame(term = names(term_frequency3), num = term_frequency3)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs3$term, word_freqs3$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "BrBG"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# TF - IDF
# Term Frequencies and Inverse Document Frequencies
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# Unigram 
tfidf_tdm <- TermDocumentMatrix(cleaned_mysearch_corp,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))


# Bigram
tfidf_tdm2 <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer2, weighting=weightTfIdf))
tfidf_tdm2_m <- as.matrix(tfidf_tdm2)

# Term Frequency
term_frequency2 <- rowSums(tfidf_tdm2_m)
# Sort term_frequency in descending order
term_frequency2 <- sort(term_frequency2,dec=TRUE)

# Create word_freqs
word_freqs2 <- data.frame(term = names(term_frequency2), num = term_frequency2)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs2$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))


# Trigram 
tfidf_tdm3 <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer3, weighting=weightTfIdf))
tfidf_tdm3_m <- as.matrix(tfidf_tdm3)

# Term Frequency
term_frequency3 <- rowSums(tfidf_tdm3_m)
# Sort term_frequency in descending order
term_frequency3 <- sort(term_frequency3,dec=TRUE)

# Create word_freqs
word_freqs3 <- data.frame(term = names(term_frequency3), num = term_frequency3)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs3$num,min.freq=5,max.words=2000,colors=brewer.pal(8, "Paired"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Positive and Negatives
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Emoji Sentiment Polarity Lookup Tables
emojis <- data(hash_sentiment_emojis)

nrc_lex <- get_sentiments("nrc")
table(nrc_lex$sentiment)

loughran_lex <- get_sentiments("loughran")
table(loughran_lex$sentiment)

psc <- c()
# Get polarity score the dirty way
for(i in 1:nrow(mysearch))
{
  # Text as Variable
  stg <- i$text
  
  # Polarity Table
  tbl <- polarity(stg)
  
  # Counts
  cnt <- count(tbl)
  
  # Number of Positive Words
  pos <- length(cnt$pos.words[[1]])
  
  # Total Number of Words
  wds <- cnt$wc
  
  # Verify Polarity Score
  pol <- pos / sqrt(wds)
  
  # Add Polarity Score to Dataframe
  psc <- rbind(psc, pol)
}

# Try Something Different
mysearch$text <- iconv(mysearch$text, from = "UTF-8", to = "ASCII", sub = "")

clean_corpus <- function(cleaned_corpus){
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

review_corpus <- VCorpus(VectorSource(mysearch$text))
cleaned_review_corpus <- clean_corpus(review_corpus)

tidy_mytext <- tidy(TermDocumentMatrix(cleaned_review_corpus))
bing_lex <- get_sentiments("bing")
mytext_bing <- inner_join(tidy_mytext, bing_lex, by = c("term" = "word"))
mytext_bing$sentiment_n <- ifelse(mytext_bing$sentiment=="negative", -1, 1)
mytext_bing$sentiment_score <- mytext_bing$count*mytext_bing$sentiment_n
aggdata <- aggregate(mytext_bing$sentiment_score, list(index = mytext_bing$document), sum)
sapply(aggdata,typeof)
aggdata$index <- as.numeric(aggdata$index)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Split into 2 sets (positive and negative)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Comparison Cloud (positive and negative)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Commonality Cloud (shared words)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Radar Chart
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



