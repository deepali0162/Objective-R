#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
install.packages("caTools")
install.packages("plyr")
library(plyr)
library(recommenderlab)
library(Matrix)
library(caTools)
#movie rating data
movie_rate_data <- read.csv("Desktop/R/Movie.csv")
#metadata about the variable
#movie_rate_data<-Movie
str(movie_rate_data)
#rating distribution
hist(movie_rate_data$rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
movie_rate_data_matrix <- as(movie_rate_data, 'realRatingMatrix')
movie_rate_data_matrix@data

#User Based Collaborative Filtering

movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")
#UBCF : User Based Collaborative Recommendation

#Predictions for all users 
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix, n=5) #for all users
d<-as(recommended_items2, "list")
d

library(plyr)
df<-ldply(d, data.frame)
write.csv(df,"Desktop/R/Movie_result.csv")

#Prediction for user ID : 2,3,4
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix[c(2,3,4),], n=5) #for users for ID: 2,3,4
d1<-as(recommended_items2, "list")
d1