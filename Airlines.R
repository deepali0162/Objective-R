install.packages("Metrics")
library(Metrics)
library(readxl)
Airlines_Data <- read_excel("Desktop/Data Science/Assignments/Forecasting/Airlines+Data.xlsx")

View(Airlines_Data) # Seasonality 12 months 

plot(Airlines_Data$Passengers,type="l")
# So creating 11 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
#outer gives us a matrix ; "==" + 0 -> we want to update diagonal value as 1 and rest as 0

colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(Airlines_Data,X)
View(trakdata)

trakdata["t"]<- 1:96
View(trakdata)
trakdata["log_passenger"]<-log(trakdata["Passengers"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

##Data Partition
train<-trakdata[1:84,]
test<-trakdata[85:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-rmse(test$Passengers,linear_pred$fit)
rmse_linear #53.19924

######################### Exponential #################################


expo_model<-lm(log_passenger~t,data=train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Passengers,exp(expo_pred$fit))

rmse_expo #46.05736

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Passengers,Quad_pred$fit)
rmse_Quad #48.05189

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Passengers,sea_add_pred$fit)

rmse_sea_add #132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Passengers,Add_sea_Linear_pred$fit)

rmse_Add_sea_Linear #35.34896

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Passengers,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad #26.36082

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Passengers,exp(multi_sea_pred$fit))

rmse_multi_sea #140.0632

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Passengers,exp(multi_add_sea_pred$fit))

rmse_multi_add_sea #10.51917

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Multiplicative Seasonality Linear Trend has least RMSE value
new_model <- lm(log_passenger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)

# Getting residuals 
resid <- residuals(new_model)
acf(resid,lag.max = 10)

# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 
k <- arima(resid, order=c(1,0,0))
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)
#write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)
####################### Predicting new data #############################
library(readxl)
test_data<-read_excel(file.choose(),1) #Load Predict_new.xlsx
View(test_data)

#test_data<-Predict_new
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
#pred_re<-pred_res$pred[1:12]
pred_new$fit <- pred_new$fit + pred_res$pred
View(pred_new)
