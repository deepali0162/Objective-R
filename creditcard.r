CC <- read.csv(file.choose())
View(CC)
colnames(CC)

CC <- na.omit(CC)
CC$card <- as.factor(CC$card)
model <- glm(card~.,data=CC[,-1],family = "binomial")

##Confusion matrix
conf <- predict(model,type=c("response"),CC)
conf

confusion <- table(conf>0.5,CC$card)
confusion
# no  yes
# FALSE  116    5
# TRUE   180 1018
# (116 + 1018)/1319 = 0.86

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.8597422 ~ 0.86

pred_values <- NULL
yes_no <- NULL
for (i in 1:1319){
  pred_values[i] <- ifelse(conf[i]>=0.5,1,0)
  yes_no[i] <- ifelse(conf[i]>=0.5,"yes","no")
}

CC[,"conf"] <- conf
CC[,"pred_values"] <- pred_values
CC[,"yes_no"] <- yes_no

View(CC[,c(2,14,15,16)])

##Accuracy 
acc <- table(CC$card,pred_values)
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 0.8597422

##ROC Curve 
library(ROCR)
rocrpred<-prediction(conf,CC$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)
##More area under the ROC Curve better is the logistic regression model obtained
  
