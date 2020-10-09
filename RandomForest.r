install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(randomForest)
library(caret)
data("iris")
model<-randomForest(iris$Species~.,data=iris,ntree=50)
# View the forest results.
print(model)
#Imoporantce of the variable - Lower Gini
print(importance(model))
#Prediction

pred<- predict(model,iris[,-5])
table(pred,iris$Species)
