install.packages("Metrics")
library(Metrics)

Amtrak<-read.csv('C:\\Data Science\\Amtrak.csv') # read the Amtrack data

View(Amtrak) # Seasonality 12 months 

plot(Amtrak$Ridership,type="l")
# So creating 11 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 120), month.abb,"==") + 0 )# Creating dummies for 12 months

colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(Amtrak,X)
View(trakdata)

trakdata["t"]<- 1:120
View(trakdata)
#trakdata["log_rider"]<-log(trakdata["Ridership"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
#attach(trakdata)

##Data Partition
train<-trakdata[1:108,]
test<-trakdata[109:120,]

########################### LINEAR MODEL #############################

linear_model<-lm(Ridership~t,data=train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-rmse(test$Ridership,linear_pred$fit)
rmse_linear

######################### Exponential #################################


expo_model<-lm(log_rider~t,data=train)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Ridership,exp(expo_pred$fit))

rmse_expo 

######################### Quadratic ####################################

Quad_model<-lm(Ridership~t+t_square,data=train)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Ridership,Quad_pred$fit)
rmse_Quad 

######################### Additive Seasonality #########################

sea_add_model<-lm(Ridership~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Ridership,sea_add_pred$fit)

rmse_sea_add

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Ridership~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Ridership,Add_sea_Linear_pred$fit)

rmse_Add_sea_Linear

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)

Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Ridership,Add_sea_Quad_pred$fit)

rmse_Add_sea_Quad 

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_rider~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Ridership,exp(multi_sea_pred$fit))

rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_rider~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)

multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Ridership,exp(multi_add_sea_pred$fit))

rmse_multi_add_sea 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Additive seasonality with Quadratic has least RMSE value
new_model <- lm(Ridership~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)

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
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)