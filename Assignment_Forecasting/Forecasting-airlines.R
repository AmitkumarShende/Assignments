library(readr)
library(caret)
library(dplyr)
library(readxl)

setwd("D:/Data_science/Assignments/Assignment_Forecasting")
Airline<-read_xlsx(file.choose()) # read the Airlines+Data
View(Airline) # Seasonality 12 months 
windows()
plot(Airline$Passengers,type="o") # Seasonality 12 months, multiplicative
# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
Airlinedata<-cbind(Airline,X)
View(Airlinedata)
colnames(Airlinedata)[2]<-"Passenger"
colnames(Airlinedata)
Airlinedata["t"]<- 1:96
View(Airlinedata)
Airlinedata["log_passenger"]<-log(Airlinedata["Passenger"])
Airlinedata["t_square"]<-Airlinedata["t"]*Airlinedata["t"]
attach(Airlinedata)

train<-Airlinedata[1:84,]

test<-Airlinedata[85:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passenger~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passenger-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19



######################### Exponential #################################

expo_model<-lm(log_passenger~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passenger-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.057

######################### Quadratic ####################################

Quad_model<-lm(Passenger~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passenger-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05

######################### Additive Seasonality #########################

sea_add_model<-lm(Passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passenger-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #132.81

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passenger-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.35

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passenger~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passenger-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passenger-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.06

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passenger-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend has least RMSE value

new_model <- lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Airlinedata)

summary(new_model)


