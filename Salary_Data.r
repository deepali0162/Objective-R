SalHike<- read.csv(file.choose())
View(SalHike)

names(SalHike)[1]="Exp"
names(SalHike)[2]="Sal"

attach(SalHike)

r1 <- lm(Sal~Exp , data=SalHike)
summary(r1)
#R-squared:  0.957 and p-value: < 2.2e-16

r1$coefficients
r1$residuals

sqrt(sum(r1$residuals^2)/nrow(wc.at)) #2933.715 # RMSE 

pred <- predict(r1,interval="confidence")

pred <- as.data.frame(pred)

confint(r1,level=0.95)
x = predict(r1,interval = "predict")
x  = as.data.frame(x)

pred
View(pred)

#CONCLUSION : We can predict the model for Salary hike is better at R square 0.957 > 0.80#
