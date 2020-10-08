## Bagging ----
# Using the ipred bagged decision trees
install.packages("ipred")
library(ipred)
library(caret)
set.seed(7)
data("iris")
inTraininglocal<-createDataPartition(iris$Species,p=.70,list = F)
training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

mybag <- bagging(training$Species ~ ., data = training, nbagg = 50)
pred <- predict(mybag, testing)
table(pred, testing$Species)

# Bagging using Caret bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
model_cv<-train(Species ~ ., data = training, method = "treebag",trControl = ctrl)
pred <- predict(model_cv, testing)
table(pred, testing$Species)
