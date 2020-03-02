install.packages(caret)
install.packages(randomForest)
install.packages(MASS)
library(caret)
library(randomForest)
library(MASS)
library(readr)
setwd("D:/Data_science/Assignments/Assignment_Random_Forest")

FraudCheck <- read.csv(file.choose())


# EDA

max(FraudCheck$Taxable.Income)

hist(FraudCheck$Taxable.Income)

hist(FraudCheck$Taxable.Income, main = "FraudCheck$Taxable.Income",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)

table(FC$Risky_Good)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]

# Creating The Model
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting 2
#number of tress:500
# each tree node.
# Out of bag estimate of error rate is 47 % in Random Forest Model.
attributes(rf)

# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)

head(train$Risky_Good)

# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$Risky_Good)  # 100 % accuracy on training data 

# more than 97% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 

# Error Rate in Random Forest Model :
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)

rf1 <- randomForest(Risky_Good~., data=train, ntree = 300, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1  # with the new values after tuning, the OOB estimate error is 16.84 %

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)  # 100 % accuracy on training data 

# more than 99% Confidence Interval. 
# Sensitivity for Yes and No is 100 % 

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good) # 100 % accuracy on test data 

# Confidence Interval between 97 to 100 %

# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")

# Majority of the trees has an average number has close to 40 nodes. 

# Variable Importance :

varImpPlot(rf1)

# Mean Decrease Accuracy graph shows that how worst the model performs without each variable.
# say Taxable.Income is the most important variable for prediction.on looking at City population,it has no value.

# MeanDecrease gini graph shows how much by average the gini decreases if one of those nodes were 
# removed. Taxable.Income is very important and Urban is not that important.

# Quantitative values 
importance(rf1)

varUsed(rf)   # which predictor variables are actually used in the random forest.

# Partial Dependence Plot 
partialPlot(rf1, train, Taxable.Income, "Good")

          
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")

# On that graph, i see that if the taxable Income is 30000 or greater,
# than they are good customers else those are risky customers.
# Extract single tree from the forest :

tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)
