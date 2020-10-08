Glass <- read.csv("C:\\Data Science\\Assignments\\KNN\\Glass.csv")
Glass<-Glass[,-1]
# table of diagnosis
prop.table(table(Glass$Type))*100
# recode Type as a factor
Glass$Type<-factor(Glass$Type)
str(Glass)

#Creating training and testing dataset
Glass_train <- Glass[1:150,]
Glass_test  <- Glass[151:214,]

#Create labels for train and test data
Glass_label_train <- Glass[1:150,9]
Glass_label_test <- Glass[151:214,9]

#---- Training a model on the data ----
library(class)
#Preparing model
Glass_test_pred <- knn(train = Glass_train[,-1],
                       test = Glass_test[,-1],
                       cl = Glass_label_train, k=16)

#----------------------#
# load the library
install.packages("e1071")
library(e1071)
library(caret)

# load the dataset
data<- read.csv("C:\\Data Science\\Assignments\\KNN\\Glass.csv")
ctrl <- trainControl(method="cv") 
my_knn_model <- train(Species ~ .,
                      method = "knn",
                      data = iris,
                      trControl=ctrl,
                      tuneGrid = expand.grid(k = c(1:100)))
my_knn_model
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 16.

