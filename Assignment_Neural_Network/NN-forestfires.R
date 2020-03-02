# Using multilayered feed forward nueral network
install.packages("neuralnet")
install.packages("nnet")
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(neuralnet)  # regression
library(nnet) # classification 
library(plyr)
setwd("D:/Data_science/Assignments/Assignment_Neural_Network")

ForestFire= read.csv("D:/Data_science/Assignments/Assignment_Neural_Network/forestfires.csv")
names(ForestFire)[11]<-"Burned_area" #.....[1]--column number, "Burned-area"----name to change for R
ForestFire<-ForestFire[,7:11]
class(ForestFire) # As Dataframe

str(ForestFire)

summary(ForestFire) # Confirms on the different scale and demands normalizing the data.

# Prediction of Forest fires requires only prediction from 
# temperature, rain, relative humidity and wind speed

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
ForestFire_norm<-as.data.frame(lapply(ForestFire,FUN=normalize))

# We need to tweak this as a classification problem.lets base out the Size using this criteria :

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(ForestFire), replace = TRUE, prob = c(0.7,0.3))
FF_train <- ForestFire[ind==1,]
FF_test  <- ForestFire[ind==2,]

summary(ForestFire) # Confirms on the different scale and demands normalizing the data.


# Creating a neural network model on training data


FF_model <- neuralnet(Burned_area~temp+RH
                            +wind+rain,data = FF_train)
str(FF_model)
attributes(FF_model)

# visualize the network topology
plot(FF_model)

plot(FF_model, rep = "best")

summary(FF_model)

windows()
par(mar = numeric(4), family = 'serif')
plotnet(FF_model, alpha = 0.6)

model_results <- compute(FF_model,FF_test[1:5])
predicted_Burned_area <- model_results$net.result

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(ForestFire$Burned_area)
str_min <- min(ForestFire$Burned_area)

set.seed(12345)
ForestFire_model2 <- neuralnet(Burned_area~temp+RH
                               +wind+rain,data = FF_train, hidden = 2)

plot(ForestFire_model2 ,rep = "best")

summary(ForestFire_model2)

model_results2<-compute(ForestFire_model2,FF_test[1:5])
predicted_Burned_area2<-model_results2$net.result


plot(predicted_Burned_area2,FF_test$Burned_area)

par(mar = numeric(4), family = 'serif')
plotnet(ForestFire_model2, alpha = 0.6)

# SSE(Error) has reduced and training steps had been increased as the number of neurons 
#under hidden layer are increased
