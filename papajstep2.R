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

setwd("C:/Users/Serina Brenner/Documents/school/spring 2018/text analysis/")


pjdata <- read.csv(file="papajtweets.csv", header=TRUE, sep=",")
for (i in pjdata[2]){
  print(polarity(i))
}
