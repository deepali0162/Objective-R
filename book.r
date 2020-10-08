#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
install.packages("caTools")
install.packages("plyr")
library(plyr)
library(recommenderlab)
library(Matrix)
library(caTools)

#Book data
book_data <- read.csv('C:\\Data Science\\Assignments\\Recommendation System\\book.csv')
book_data <- book_data[-c(1)] #eliminating first column
View(book_data)
#metadata about the variable
str(book_data)

#rating distribution
hist(book_data$Book.Rating)

#Visualizations
library(moments)
qqnorm(book_data$Book.Rating)
qqline(book_data$Book.Rating)
boxplot(book_data$Book.Rating)
barplot(book_data$Book.Rating)


#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book_data, "realRatingMatrix")
book_rate_data_matrix@data

#User Based Collaborative Filtering
book_recomm_model2 <- Recommender(book_rate_data_matrix, method="UBCF")

#Predictions for all users 
recommended_items2 <- predict(book_recomm_model2, book_rate_data_matrix, n=15)
bk<-as(recommended_items2, "list")
























# #the datatype should be realRatingMatrix inorder to build recommendation engine
# book_rate_data_matrix <- as(book_data, 'realRatingMatrix')
# book_rate_data_matrix@data
# 
# #User based Collaborative Filtering
# book_recomm_model <- Recommender(book_rate_data_matrix, method="UBCF")
# 
# #Recommendation for all users
# recommended_items2 <- predict(book_recomm_model,book_rate_data_matrix,n=3)
# b <- as (recommended_items2,"list")
