install.packages('AER')
library('AER')
library(plyr)
setwd("D:/Data_science/Assignments/Assignment_Logistic_Regression")
Bank_Data <- read.csv(file.choose()) # Choose the bank Data set
colnames(Bank_Data)
#bank_data1 <- as.data.frame(Bank_Data)
bank_data1=Bank_Data
summary(bank_data1)
table(bank_data1$y)
# Preparing a linear regression 
mod_lm <- lm(y~.,data=bank_data1)
pred1 <- predict(mod_lm,bank_data1)
# pred1
plot(age,pred1)

# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(y~.,data=bank_data1,family = "binomial")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,bank_data1,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank_data1$y)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 90.18

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank_data1[,"prob"] <- prob
bank_data1[,"pred_values"] <- pred_values
bank_data1[,"yes_no"] <- yes_no

# View(bank_data1[,c(1,31,36:38)])

table(bank_data1$y,bank_data1$pred_values)

# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
# install.packages("ROCR")
library(ROCR)

rocrpred<-prediction(prob,bank_data1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)