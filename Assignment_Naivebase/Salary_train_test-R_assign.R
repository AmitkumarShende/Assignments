#Import the salary train and test dataset 
install.packages("e1071")
library(e1071)
install.packages("tm")
library(tm)
library(readr)
library(caret)
# Data(Train)
train_sal <- read.csv("D:/Data_science/Assignments/Assignments_naivebase/SalaryData_Train.csv")
str(train_sal)

View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)


test_sal <- read.csv("D:/Data_science/Assignments/Assignments_naivebase/SalaryData_Test.csv")
str(test_sal)
View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

# Naive Bayes Model 
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)
Model

Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)

confusionMatrix(Model_pred,test_sal$Salary)