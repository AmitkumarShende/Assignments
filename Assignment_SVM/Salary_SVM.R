#####Support Vector Machines -------------------
library(readr)
library(dplyr)
library(kernlab)
library(caret)
library(ggplot2)

salary_train= read.csv("D:/Data_science/Assignments/Assignment_SVM/SalaryData_Train.csv")
salary_test= read.csv("D:/Data_science/Assignments/Assignment_SVM/SalaryData_Test.csv")

str(salary_train)

# Get into factor form to TRAIN model performance
col.names =colnames(salary_train, do.NULL = TRUE, prefix = "col")
#col.names

salary_train$educationno <- as.factor(salary_train$educationno)
salary_test$educationno <- as.factor(salary_test$educationno)

str(salary_train)
str(salary_test)

#Visualization OR EDA
# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

plot(salary_train$workclass,salary_train$Salary)

plot(salary_train$maritalstatus,salary_train$Salary)

#Density Plot 

ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+ ggtitle("Age - Density Plot") #  graph Heading

ggplot(data=salary_train,aes(x = salary_train$workclass, fill = salary_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')+ggtitle("workclass - Density Plot") #  graph Heading

##Training a model on the data ----
# Begin by training a simple linear SVM----vanilladot

salary_classifier <- ksvm(salary_train$Salary ~ ., data = salary_train,
                          kernel = "vanilladot")
# Basic information about the model

salary_classifier

## Evaluating model performance ----
# predictions on testing dataset
salary_predictions <- predict(salary_classifier, salary_test)

head(salary_predictions)

table(salary_predictions, salary_test$Salary)

agreement <- salary_predictions == salary_test$Salary

table(agreement)
prop.table(table(agreement))


## Improving model performance ----rbfdot
salary_classifier_rbf <- ksvm(Salary ~ ., data = salary_train, kernel = "rbfdot")

# predictions on testing dataset

salary_predictions_rbf <- predict(salary_classifier_rbf, salary_test)
agreement_rbf <- salary_predictions_rbf == salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))

## Improving model performance ----polydot 
salary_classifier_polydot <- ksvm(Salary ~ ., data = salary_train, kernel = "polydot")

# predictions on testing dataset

salary_predictions_polydot <- predict(salary_classifier_polydot, salary_test)
agreement_polydot <- salary_predictions_polydot == salary_test$Salary
table(agreement_polydot)
prop.table(table(agreement_polydot))
#********************END**************************