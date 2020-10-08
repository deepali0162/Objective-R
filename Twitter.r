install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("httpuv")
install.packages("tm")
installed.packages("syuzhet")
installed.packages("SnowballC")
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(tm)
library(syuzhet)
library(SnowballC)
### https://apps.twitter.com/

cred <- OAuthFactory$new(consumerKey='ClIoKVLANWrttpVQQWbYGPeXL',
                         consumerSecret='YRI8gPkrTzklHno8jBSqa5Txgw65NGpv0C5vDSf9uT4SElD3Eh',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#save(cred, file="twitter authentication.Rdata")
#load("twitter authentication.Rdata")

setup_twitter_oauth("ClIoKVLANWrttpVQQWbYGPeXL", 
                    "YRI8gPkrTzklHno8jBSqa5Txgw65NGpv0C5vDSf9uT4SElD3Eh",
                    "1285596116927434752-6Gs89mtEfK5vMOlOD7Xhw87buvijAt", # Access token
                    "vdbbr6bpwpaNdrMgxpMWHGXH6lQ9DGehGghoRk20yLZaa")  # Access token secret key

#We have invoked the Twitter app and extracted data from the twitter handle '@narendramodi'
Tweets <- userTimeline('narendramodi', n = 1000)

#Cleaning the tweets for further analysis
TweetsDF <- twListToDF(Tweets)
setwd("C:\\Data Science\\Assignments\\Text Mining")
#Writing the Tweets to csv file
write.csv(TweetsDF, "Tweets_file.csv")

head(TweetsDF$text)

#remove URLs, hashtags and other twitter handles using the gsub function
TweetsDF2 <- gsub("http.*","",TweetsDF$text)
TweetsDF2 <- gsub("https.*","",TweetsDF2)
TweetsDF2 <- gsub("#.*","",TweetsDF2)
TweetsDF2 <- gsub("@.*","",TweetsDF2)

TweetsDF2

#Getting sentiment score for each tweet
words.df <- as.vector(TweetsDF2)
emotions.df <- get_nrc_sentiment(words.df)
emotions.df2 <- cbind(TweetsDF2,emotions.df)
emotions.df2

#Now, we will use the get_sentiment function to extract sentiment score for each of the tweets.
sent.value <- get_sentiment(words.df)
most.positive <- words.df[sent.value == max(sent.value)]
most.positive

most.negative <- words.df[sent.value <= min(sent.value)]
most.negative

sent.value

#Now, we will segregate positive and negative tweets based on the score assigned to each of the tweets.
category_sentiments <- ifelse(sent.value<0, "Negative",
                              ifelse(sent.value>0, "Positive","Neutral"))

category_sentiments

#So, now we have analyzed the twitter handle of Narendra Modi and got the sentiment around tweets.
#The break of total number of tweets by sentiment is
table(category_sentiments)
##Output
# Negative  Neutral Positive 
# 16       76        8 