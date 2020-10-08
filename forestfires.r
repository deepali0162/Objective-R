library(readr)
forestfires <- read_csv("C:/Data Science/Assignments/Support Vector Machines/forestfires.csv")
forestfires$size_category <- as.factor(forestfires$size_category)

# library(dplyr)
# forestfires <- select(forestfires,month,day,FFMC,DMC,DC,ISI,temp,RH,wind,rain,size_category)

#divide data into Training and Test data
forestfires_train <- forestfires[1:400, ]
forestfires_test  <- forestfires[401:517,]

#Training model on the data
#Using Simple Linear SVM
library(kernlab)
forestfires_classifier <- ksvm(size_category ~ ., data = forestfires_train, kernel="vanilladot")

#Evaluating model performance
#prediction on testing data
library(e1071)
forestfires_predictions <- predict(forestfires_classifier, forestfires_test)

