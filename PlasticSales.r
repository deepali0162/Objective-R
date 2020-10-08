install.packages("Metrics")
library(Metrics)
library(readr)

PlasticSales <- read_csv("C:/Data Science/Assignments/Forecasting/PlasticSales.csv")
#Read PlasticSales file

View(PlasticSales) # Seasonality 12 months 

plot(PlasticSales$Sales,type="l")
# So creating 11 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months

colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(PlasticSales,X)
View(trakdata)

trakdata["t"]<- 1:60
View(trakdata)
trakdata["log_sales"]<-log(trakdata["Sales"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

##Data Partition
train<-trakdata[1:48,]
test<-trakdata[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-rmse(test$Sales,linear_pred$fit)
rmse_linear #260.9378

######################### Exponential #################################


expo_model<-lm(log_sales~t,data=train)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-rmse(test$Sales,exp(expo_pred$fit))
rmse_expo #268.6938

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Sales,Quad_pred$fit)
rmse_Quad #297.4067

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Sales,sea_add_pred$fit)
rmse_sea_add #235.6027

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Sales,Add_sea_Linear_pred$fit)
rmse_Add_sea_Linear #135.5536

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Sales,Add_sea_Quad_pred$fit)
rmse_Add_sea_Quad #218.1939

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Sales,exp(multi_sea_pred$fit))
rmse_multi_sea #239.6543

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Sales,exp(multi_add_sea_pred$fit))
rmse_multi_add_sea #160.6833

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea","rmse_Add_sea_Linear'"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea,rmse_Add_sea_Linear))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Use entire data : Additive Seasonality with Linear has least RMSE value
new_model <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=trakdata)

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

