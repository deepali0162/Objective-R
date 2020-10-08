EmpData = read.csv(file.choose())
View(EmpData)

names(EmpData)[1]<-"sal"
names(EmpData)[2]<-"rate"
View(EmpData)

attach(EmpData)
#**********************************************************************#
r1<- lm(sal~rate, data=EmpData) # Y ~ X
summary(r1)
#R-squared:  0.8312 and p-value: 0.0002386

r1$coefficients
r1$residuals
sqrt(sum(r1$residuals^2)/nrow(EmpData)) #35.89264 ## RMSE 
#**********************************************************************#
confint(r1,level=0.95)
predict(r1,interval="predict")
pred<-predict(r1,interval="predict")

pred<-as.data.frame(pred)
View(pred)

#We can predict model for Churn_Out_Rate is better at R square value = 0.8312 > 0.80#