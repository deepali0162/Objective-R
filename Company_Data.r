require(tree)
Company<-read.csv("C:\\Data Science\\Assignments\\Decision Trees\\Company_Data.csv")
attach(Company)

# Company<- Company[,c(1:6,8,9)]
#Making a factor variable from Sales 
Company$Sales <- as.factor(Company$Sales)
str(Company)

HighSales <- ifelse(Sales <= 8, "No", "Yes")
head(HighSales)

#Making a data frame
Company<- data.frame(Company,HighSales)
Company

set.seed(1001)
#A training sample of 250  examples sampled without replacement
train<-sample(1:nrow(Company), 250)
#Fitting another Model
tree1<-tree(HighSales ~ .-Sales , data = Company, subset = train)
summary(tree1)


#Plotting
plot(tree1);text(tree1)

#Predicting the Class labels for Test set
pred<-predict(tree1, newdata = Company[-train,],type = "class")
head(pred)

#Confusion Matrix to check number of Misclassifications
with(Company[-train,],table(pred,HighSales))

#Misclassification Error Rate on Test Set
mean(pred!=Company[-train,]$HighSales)

#Confusion Matrix
with(Company[-train,],table(pred1,HighSales))
##      HighSales
## pred1 No Yes
##   No  74  23
##   Yes 15  38