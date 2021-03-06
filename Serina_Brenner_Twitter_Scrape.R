library(twitteR)
library(rtweet)
library(data.table)
library(stringr)
library(tidytext)
setwd("C:/Users/Serina Brenner/Documents/GitHub/Text-Analytics")

#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --
# read 2000 tweets and save to csv
#-- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o -- o --

# twitter keys
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
setup_twitter_oauth('ZbGmXn3PQ7iAE2Z0dEu7mfSMk', 'NesQtlEtVIUb6MCUCHaWRo8D0TFmL8LlJfibvwP1zFvf4OVago', '1309079071-uqqbxNGbk3k47GjYafbWguOUv25QhTlX8eb4UbR', 'aVJc8ov2vAiClaeWk4UYzbVXJfoir3KLWP67AOhsJkBu2')

# search and df to csv
mysearch <- searchTwitter("hawaiian pizza",n=2000)
mysearch.df <- twListToDF(mysearch)
write.csv(mysearch.df, file = "search.csv",row.names=FALSE)

# important columns:
#   text                    --> actual tweet
#   longitude/latitude      --> locations
#   favorited/favoriteCount --> fav info
