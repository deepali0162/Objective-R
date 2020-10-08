library(caret)
library(C50)
library(moments)
library(tidyverse)

Fraud<-read.csv("C:\\Data Science\\Assignments\\Decision Trees\\Fraud_check.csv")
Fraud <- na.omit(Fraud)

skewness(Fraud$Taxable.Income)
kurtosis(Fraud$Taxable.Income)

hist(Fraud$Taxable.Income)
barplot(Fraud$Taxable.Income)
boxplot(Fraud$Taxable.Income)
##qqplot
qqnorm(Fraud$Taxable.Income)
qqline(Fraud$Taxable.Income)


##Standardisation
standard_values = scale(Fraud$Taxable.Income)
summary(standard_values)
summary(Fraud)

####Applying Decision Tree Model on the Fraud Dataset
Beh = ifelse(Fraud$Taxable.Income<= 30000, "Risky", "Good")
Fraud = data.frame(Fraud, Beh)
Fraud$Beh <- as.factor(Fraud$Beh)

set.seed(117)
#Data Partition
inTraininglocal<-createDataPartition(Fraud$Beh,p=.70,list = F)
training<-Fraud[inTraininglocal,]
testing<-Fraud[-inTraininglocal,]

#Model Building
model<-C5.0(Beh~.,data = training) 
#Generate the model summary
summary(model)
#Predict for test data set
pred<-predict.C5.0(model,testing[,-7]) #type ="prob"

#Accuracy of the algorithm
a<-table(testing$Beh,pred)
sum(diag(a))/sum(a)
#Visualize the decision tree
plot(model)
