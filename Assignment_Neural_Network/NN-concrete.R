# Using multilayered feed forward nueral network
install.packages("neuralnet")
install.packages("nnet")
library(NeuralNetTools)
library(neuralnet)  # regression
library(nnet) # classification 
setwd("D:/Data_science/Assignments/Assignment_Neural_Network")

concrete <- read.csv(file.choose())

# Exploratory data Analysis :

hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))


hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))


View(concrete)
str(concrete)

#Normalising data before processing

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalize))

head(summary(concrete_norm))#checking Normalisation

# Adding Strength column to norm data set
concrete_norm <- cbind(concrete_norm,concrete$strength)
colnames(concrete_norm)[9] <- "strength"

# Splitting data for train and test 
ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
train <- concrete_norm[ind==1,]
test  <- concrete_norm[ind==2,]

# Using multilayered feed forward nueral network
# Building model

formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse ="+"),sep="~")

#concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
concrete_model <- neuralnet(formula = formula_nn,data = train)
str(concrete_model)
plot(concrete_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared

set.seed(123)
model_results <- compute(concrete_model,test[1:8])
str(model_results)

# predicted_strength

predicted_strength <- model_results$net.result

# model_results$neurons

cor(predicted_strength,test$strength)

plot(predicted_strength,test$strength)
