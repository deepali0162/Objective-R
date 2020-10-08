install.packages("tidyverse")
library(tidyverse)
library(caret)
library(randomForest)
library(readr)
#Load the data and remove NAs
data <- read_csv("Desktop/Data Science/Assignments/Random Forest/Fraud_check.csv")
View(data)
data <- na.omit(data)
hist(data$Taxable.Income)

hist(data$Taxable.Income,main="Sales",xlim=c(0,100000),breaks = c(seq(40,60,80)),col=c("blue","red","violet","yellow"))

#if taxable_income <= 30000 as "Risky" and others are "Good"
Risky_Good <- ifelse(data$Taxable.Income<=30000, "Risky", "Good")
FC_Data <- data.frame(data,Risky_Good)

FC_Data$Undergrad <- as.factor(FC_Data$Undergrad)
FC_Data$Marital.Status <- as.factor(FC_Data$Marital.Status)
FC_Data$Urban <- as.factor(FC_Data$Urban)
FC_Data$Risky_Good <- as.factor(FC_Data$Risky_Good)

table(FC_Data$Risky_Good)

#Splitting the dataset into Train and Test
set.seed(123)
training.samples <- sample(2, nrow(FC_Data), replace=TRUE,prob = c(0.7,0.3))
train_data <- FC_Data[training.samples==1,]
test_data <- FC_Data[training.samples==2, ]
set.seed(125)
rf <- randomForest(Risky_Good ~., data=train_data)
rf
# Call:
#   randomForest(formula = Risky_Good ~ ., data = train_data) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 0.47%
# Confusion matrix:
#   Good Risky class.error
# Good   343     1 0.002906977
# Risky    0    79 0.012500000

# Out of bag estimate of error rate is 0.17 % in Random Forest Model.
attributes(rf)

#Prediction on Training data
pred1 <- predict(rf,train_data)
head(pred1)
head(train_data$Risky_Good)

#the predicted data between train and original dataset matches which means we get 100% accuracy
confusionMatrix(pred1, train_data$Risky_Good)

#Prediction with Test data
pred2 <- predict(rf,test_data)
confusionMatrix(pred2, test_data$Risky_Good)
#we get 100% accuracy on test data

#Error Rate in Random Forest Model
plot(rf)
# at 200 there is a constant line and it does not vary after 200 trees

rf1 <- randomForest(Risky_Good~., data=train_data, ntree=200, importance=TRUE, mtry=2, proximity=TRUE)
rf1

pred1_override <- predict(rf1, train_data)
confusionMatrix(pred1_override,train_data$Risky_Good) #100% accuracy

pred2_override <- predict(rf1,test_data)
confusionMatrix(pred2_override, test_data$Risky_Good) #100% accuracy

hist(treesize(rf1), main = "No. of nodes for the tree", col = "red")

#Variable Importance
varImpPlot(rf1)
#Top 5 Variable Importance
varImpPlot(rf1, Sort=T, n.var = 5, main = "No. of nodes for the tree")
#Quantitative values
importance(rf1)

varUsed(rf1) #which predictor values are used in the Random Forest

partialPlot(rf1, train_data ,Taxable.Income, "Good")
#we can see that if taxable income is 30000 or greater than 30000 then there are Good Customers
#else others are Risky Customers.

#Extract A Single Tree From A Forest
tr1 <- getTree(rf1, 2, labelVar = TRUE)

