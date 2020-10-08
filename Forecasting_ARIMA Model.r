# ARIMA Models  ###########
install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)
# Converting data into time series object
# Loading Aviation Data

aviation<-read.csv("C:\\Data Science\\Aviation.csv") # Aviation.csv
View(aviation)
amts<-ts(aviation$Sales,frequency = 4,start=c(86))
View(amts)
plot(amts)

# dividing entire data into training and testing data 
train<-amts[1:38]
test<-amts[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(train)


# Auto.Arima model
library(forecast)
model_AA <- auto.arima(train) 
#m<-arima(train,order = c(0,1,0))
model_AA
acf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=4),xaxt="n") #xaxt- x axis text
