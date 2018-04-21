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
library(reshape2)
library(rlang)
library(plyr)
setwd("C:/Users/Serina Brenner/Documents/GitHub/Text-Analytics")
mysearch <- read.csv(file="search.csv", header=TRUE, sep=",")

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Cleaning MySearch
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# remove any non funky characters in "text" column
mysearch$text <- iconv(mysearch$text, from = "UTF-8", to = "ASCII", sub = "")

# vector source corpus - cleaning and preprocessing
mysearch_corp <- VCorpus(VectorSource(mysearch$text))
clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T)) # create function
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)                                      # use function to remove urls
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))      # replace abbreviations
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))                   # replace capital letters with lowercase
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)                              # remove punctuation
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))              # remove common stopwords
  
  # custom stopwords
  custom_stop_words <- c("pizza","pinneapple", "hawaiian", "rt", "wsj")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
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
# Unigram (one word) wordcloud
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=200,colors=brewer.pal(9,"Blues"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Bigram (two words) wordcloud
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

tokenizer2 <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2)) # two words shown
bigramtdm <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer2))
bigramtdmmat <- as.matrix(bigramtdm)
# Term Frequency desc
tf2 <- rowSums(bigramtdmmat)
tf2 <- sort(tf2,dec=TRUE)
# word frequency
word_freqs2 <- data.frame(term = names(tf2), num = tf2)
wordcloud(word_freqs2$term, word_freqs2$num,min.freq=5,max.words=200,colors=brewer.pal(9,"Blues"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Trigram (three words) wordcloud
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

tokenizer3 <- function(x)
  NGramTokenizer(x,Weka_control(min=3,max=3)) # three words shown
trigram_tdm <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer3))
trigram_tdm_m <- as.matrix(trigram_tdm)
# Term Frequency desc
tf3 <- rowSums(trigram_tdm_m)
tf3 <- sort(tf3,dec=TRUE)
# word frequency
word_freqs3 <- data.frame(term = names(tf3), num = tf3)
wordcloud(word_freqs3$term, word_freqs3$num,min.freq=5,max.words=200,colors=brewer.pal(9,"Blues"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Inverse Document Frequencies
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# Unigram 
tfidf_tdm <- TermDocumentMatrix(cleaned_mysearch_corp,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)
# Term Frequency desc
term_frequency <- rowSums(tfidf_tdm_m)
term_frequency <- sort(term_frequency,dec=TRUE)
# word frequency
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=500,colors=brewer.pal(9,"Blues"))


# Bigram
tfidf_tdm2 <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer2, weighting=weightTfIdf))
tfidf_tdm2_m <- as.matrix(tfidf_tdm2)
# Term Frequency desc
tf2 <- rowSums(tfidf_tdm2_m)
tf2 <- sort(tf2,dec=TRUE)
# word frequency
word_freqs2 <- data.frame(term = names(tf2), num = tf2)
wordcloud(word_freqs2$term, word_freqs2$num,min.freq=5,max.words=500,colors=brewer.pal(9,"Blues"))


# Trigram 
tfidf_tdm3 <- TermDocumentMatrix(cleaned_mysearch_corp,control = list(tokenize=tokenizer3, weighting=weightTfIdf))
tfidf_tdm3_m <- as.matrix(tfidf_tdm3)
# Term Frequency desc
tf3 <- rowSums(tfidf_tdm3_m)
tf3 <- sort(tf3,dec=TRUE)
# word frequency
word_freqs3 <- data.frame(term = names(tf3), num = tf3)
wordcloud(word_freqs3$term, word_freqs3$num,min.freq=5,max.words=500,colors=brewer.pal(9,"Blues"))

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Pos Neg Sentiments
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# dictionaries
qdapDictionaries::positive.words
qdapDictionaries::negative.words
qdapDictionaries::amplification.words
qdapDictionaries::deamplification.words
qdapDictionaries::negation.words

# use bing lexicon to create sentiment graph
tidy_mytext <- tidy(TermDocumentMatrix(cleaned_mysearch_corp))
bing_lex <- get_sentiments("bing")
mytext_bing <- inner_join(tidy_mytext, bing_lex, by = c("term" = "word"))
mytext_bing$sentiment_n <- ifelse(mytext_bing$sentiment=="negative", -1, 1)
mytext_bing$sentiment_score <- mytext_bing$count*mytext_bing$sentiment_n
aggdata <- aggregate(mytext_bing$sentiment_score, list(index = mytext_bing$document), sum)
sapply(aggdata,typeof)
aggdata$index <- as.numeric(aggdata$index)
ggplot(aggdata, aes(index, x)) + geom_point() 
ggplot(aggdata, aes(index, x)) + geom_smooth() + theme_bw()+
  geom_hline(yintercept = 0, color = "red")+xlab("sentence")+ylab("sentiment")+
  ggtitle("Sentiment")
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Split into 2 sets (positive and negative)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Comparison Cloud (positive and negative)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
tidy_mytext %>%
  inner_join(tidy_mytext, bing_lex, by = c("term" = "word")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

count(mytext_bing$term, mytext_bing$sentiment, sort = TRUE) %>%
  acast(mytext_bing$term ~ mytext_bing$sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Commonality Cloud (shared words)
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --



#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# Radar Chart
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
tidy_mytext <- tidy(TermDocumentMatrix(cleaned_mysearch_corp))
nrc_lex <- get_sentiments("nrc")
story_nrc <- inner_join(tidy_mytext, nrc_lex, by = c("term" = "word"))
story_nrc_noposneg <- story_nrc[!(story_nrc$sentiment %in% c("positive","negative")),]
aggdata <- aggregate(story_nrc_noposneg$count, list(index = story_nrc_noposneg$sentiment), sum)
chartJSRadar(aggdata)

