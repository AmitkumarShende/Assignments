#####Support Vector Machines -------------------
library(readr)
library(dplyr)
library(kernlab)
library(caret)
library(ggplot2)e1071
library(e1071)

ForestFire= read.csv("D:/Data_science/Assignments/Assignment_SVM/forestfires.csv")

class(ForestFire) # As Dataframe

str(ForestFire)

summary(ForestFire) # Confirms on the different scale and demands normalizing the data.

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
ForestFire$temp = normalize(ForestFire$temp)
ForestFire$RH   = normalize(ForestFire$RH)
ForestFire$wind = normalize(ForestFire$wind)
ForestFire$rain = normalize(ForestFire$rain)

# We need to tweak this as a classification problem.lets base out the Size using this criteria :

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(ForestFire), replace = TRUE, prob = c(0.7,0.3))
FF_train <- ForestFire[ind==1,]
FF_test  <- ForestFire[ind==2,]
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

# kvsm() function uses gaussian RBF kernel 

# Building model 


model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "vanilladot")
model1

Area_pred <- predict(model1, FF_test)

table(Area_pred,FF_test$size_category)
mean(Area_pred==FF_test$size_category) #67.8%
agreement <- Area_pred == FF_test$size_category
table(agreement)
prop.table(table(agreement))

## Improving model performance ----rbfdot
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= FF_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=FF_test)
mean(pred_rfdot==FF_test$size_category) # 68.49

# kernel = polydot

model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= FF_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = FF_test)
mean(pred_poly==FF_test$size_category) # 67.80

#********************END**************************