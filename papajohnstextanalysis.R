#install.packages('Rgraphviz', repos='http://cran.us.r-project.org')

library(twitteR)
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
#library("Rgraphviz")


download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

consumer_key <- 'Ec4FX1fJlX5bD218Wd80WbH4l'
consumer_secret <- '1EUtIdTP6WercTv96Cps6qm6Xi74qgYewS3nv8ySRoMqPgiWJe'
access_token <- '2186182109-q4pt6FcTxZMizNRDi3TqhLkPQoXhrmyC6poZKzx'
access_secret <- 'gtXZitCgZU99m5SzNV94QhV2l9QFu3jagw0QLXU8epBvB'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#search twitter for tweets about Papa Johns
tweets <- searchTwitter("papajohns",n=1500)

#convert to dataframe
tweets.df <- twListToDF(tweets)
dim(tweets.df)

#write to csv file
write.csv(tweets.df, file = "C:/Users/Serina Brenner/Documents/school/spring 2018/text analysis/papajtweets.csv",row.names=FALSE)

# look at df
head(tweets.df, 5)


tweets.df[tweets.df$text %like% "pizza", ]


# Clean 
# remove all non graphical characters 

Text <- sapply(tweets.df$text,function(row) iconv(row, "utf-8", "ASCII", sub=""))
Text <- str_replace_all(Text,"[^[:graph:]]", " ") 
# remove special marks
Text <- str_replace_all(Text, "@\\w+", " ")
# create a corpus by the text column vector from tweets dataframe
mycorpus <- Corpus(VectorSource(Text))
# convert text to lowercase
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
# remove punctuation
mycorpus <- tm_map(mycorpus, removePunctuation) 
# remove numbers
mycorpus <- tm_map(mycorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
mycorpus <- tm_map(mycorpus, content_transformer(removeURL)) 
# remove stopwords from corpus
myStopwords <- c(stopwords("english"), "rt", "papajohns", "Papa Johns", "Papa John's", "papa", "john's","johns","john","pj")
mycorpus <- tm_map(mycorpus, removeWords, myStopwords)
# extra whitespace is eliminated
mycorpus <- tm_map(mycorpus, stripWhitespace)    
#??? keep a copy as a dictionary for stem later

copy <- mycorpus
# stem words
mycorpus <- tm_map(mycorpus, stemDocument)

#view corpus
# check the first few cleaned tweets
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  writeLines(as.character(mycorpus[[i]]))
}

#Frequency of words
#Term Doc Matrix
tdm <- TermDocumentMatrix(mycorpus, control = list(wordLengths = c(1, Inf)))
tdm

# check words occur more or equal often than 100 times
(freq.terms <- findFreqTerms(tdm, lowfreq=100))

# create a word frequency list 
term.freq <- rowSums(as.matrix(tdm))
term.freq[1:8]
# convert into a dataframe
term.freq <- subset(term.freq, term.freq >= 70)
df <- data.frame(term = names(term.freq), freq = term.freq)
head(df,5)
# sort by descending frequency
df <- df[order(df$freq,decreasing = TRUE), c(1,2)]
head(df,10)

# convert TDM to matrix
m <- as.matrix(tdm)
head(m,2)
# calculate words frequency and sort by decending
word.freq <- sort(rowSums(m), decreasing = TRUE)
head(word.freq,10)

#create wordcloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20,random.order = F)

# Clustering
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

options(repr.plot.width=10, repr.plot.height=8)
plot(fit)
# 3 clusters  
rect.hclust(fit, k = 3) 

##Dendograms

# which words are associated with 'pizza'?
findAssocs(tdm, "pizza", 0.45)
# which words are associated with 'dominos'?
findAssocs(tdm, "dominos", 0.35)
# which words are associated with 'better'?
findAssocs(tdm, "better", 0.30)



























############################
#something is wrong
library(topicmodels)
library(MASS)
tdm
dtm <- as.DocumentTermMatrix(tdm)
dtm
lda <- lda(dtm, k = 3) # find 3 topics
term <- terms(lda, 10) # first 10 terms of every topic
term

# topic labels
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term

length(topic)

# classify each tweet into one of the topics
topic <- topics(lda, 1)
topic[1:3]
# create a dataframe with values of tweet time and topic 
topics <- data.frame(date=tweets.df$created, topic)
head(topics,3)