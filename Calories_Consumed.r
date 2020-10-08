wc.cc = read.csv(file.choose())
View(wc.cc)

colnames(wc.cc)

library(lattice)

###Graphical Representation
#DOTPLOT
dotplot(wc.cc$Weight.gained..grams.,main="Dot Plot of Weight Gained")
dotplot(wc.cc$Calories.Consumed,main="Dot Plot of Calories Consumed")
#BOXPLOT
boxplot(wc.cc$Weight.gained..grams.,col="red")
boxplot(wc.cc$Calories.Consumed,col="blue")
#HISTOGRAM
hist(wc.cc$Weight.gained..grams.)
hist(wc.cc$Calories.Consumed)
#QQPLOT
qqnorm(wc.cc$Weight.gained..grams.)
qqline(wc.cc$Weight.gained..grams.)
qqnorm(wc.cc$Calories.Consumed)
qqline(wc.cc$Calories.Consumed)

#SCATTER PLOT
plot(wc.cc$Weight.gained..grams.,wc.cc$Calories.Consumed,main="Scatter Plot", 
     col="Red", col.main="Red", col.lab="Dodgerblue4", xlab="Weight Gained", 
     ylab="Calories Gained", pch=20) 

#Calculating Correlation
attach(wc.cc)

cor(Weight.gained..grams., Calories.Consumed) #0.946991
plot(Weight.gained..grams.,Calories.Consumed)

reg <- lm(Calories.Consumed~Weight.gained..grams., data=wc.cc) # Y ~ X
summary(reg)
##Residual standard error: 251.5 on 12 degrees of freedom
##Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
##F-statistic: 104.3 on 1 and 12 DF,  p-value: 2.856e-07

reg$coefficients
reg$residuals

pred <- predict(reg,interval="confidence")

pred <- as.data.frame(pred)

x = predict(reg,interval = "predict")
x  = as.data.frame(x)


sqrt(sum(reg$residuals^2)/nrow(wc.at)) ## RMSE 
## = 232.8335

pred
View(pred)


##We can conclude the model for predicting Weight is better at R Square value =  0.8968
