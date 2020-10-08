bank <- read_excel("C:\\Data Science\\Assignments\\Logistic Regression\\Bank.xlsx")
View(bank)

bank <- na.omit(bank)
bank$y <- as.factor(bank$y)
model <- glm(y~.,data=bank[,-1],family = "binomial")

##Confusion matrix
conf <- predict(model,type=c("response"),bank)
conf

confusion <- table(conf>0.5,bank$y)
confusion
# no   yes
# FALSE 38940  3455
# TRUE    982  1834

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.9018602

pred_values <- NULL
yes_no <- NULL
for (i in 1:45211){
  pred_values[i] <- ifelse(conf[i]>=0.5,1,0)
  yes_no[i] <- ifelse(conf[i]>=0.5,"yes","no")
}

bank[,"conf"] <- conf
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no

View(bank[,c(2,18,19,20)])

##Accuracy 
acc <- table(bank$y,pred_values)
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 0.9018602

##ROC Curve 
library(ROCR)
rocrpred<-prediction(conf,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)
##More area under the ROC Curve better is the logistic regression model obtained