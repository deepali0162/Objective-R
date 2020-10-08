##Predict delivery time using sorting time 

Del<- read.csv(file.choose())
View(Del)
colnames(Del)

##Renaming Column names
names(Del)[1] <- "DT"
names(Del)[2] <- "ST"
colnames(Del)
View(Del)

attach(Del)

### Graphical representation ###
library(lattice)

#For Delivery Time
boxplot(Del$DT,main="Delivery Time")
dotplot(Del$DT)
qqnorm(Del$DT)
qqline(Del$DT)
hist(Del$DT)
hist(Del$DT, prob=TRUE)            # prob=TRUE for probabilities not counts
lines(density(Del$DT))             # add a density estimate with defaults
lines(density(Del$DT, adjust=2), lty="dotted")   # add another "smoother" density

#for Sorting Time
boxplot(Del$ST,main="Sorting Time")
dotplot(Del$ST)
qqnorm(Del$ST)
qqline(Del$ST)
hist(Del$ST)
hist(Del$ST, prob=TRUE)            # prob=TRUE for probabilities not counts
lines(density(Del$ST))             # add a density estimate with defaults
lines(density(Del$ST, adjust=2), lty="dotted")   # add another "smoother" density

plot(Del$DT,Del$ST,xlab="Delivery Time",ylab="Sorting Time")

cor(Del$DT,Del$ST) #0.8259973

#**********************************************************************#
reg <- lm(DT~ST, data=Del) # Y ~ X
summary(reg)
#R-squared:  0.6823 and p-value: 3.983e-06

reg$coefficients
reg$residuals

sqrt(sum(reg$residuals^2)/nrow(Del)) #2.79165 ## RMSE 
#**********************************************************************#
reg_sqrt <- lm(ST~sqrt(DT), data=Del)
summary(reg_sqrt)
#R-squared:  0.704 and p-value: 2.001e-06

reg_sqrt$coefficients
reg_sqrt$residuals
sqrt(sum(reg_sqrt$residuals^2)/nrow(Del)) #1.349568 ## RMSE 
#**********************************************************************#
reg_log<-lm(ST~log(DT), data=Del)
summary(reg_log)
#R-squared:  0.7109 and p-value: 1.593e-06

reg_log$coefficients
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(Del)) #1.333748 ## RMSE 
#**********************************************************************#
reg1<-lm(log(ST)~DT + I(DT*DT), data=Del)
summary(reg1)
#R-squared:  0.7937 and p-value: 6.771e-07

reg1$coefficients
reg1$residuals
sqrt(sum(reg1$residuals^2)/nrow(Del)) #0.2074381 ## RMSE 

confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)

##We can predict the model for predicting Delivery time is better at R square=0.7937~0.80

