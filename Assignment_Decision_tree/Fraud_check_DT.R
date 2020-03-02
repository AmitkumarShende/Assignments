install.packages("C50")
install.packages("tree")
install.packages("caret")
install.packages("gmodels")
install.packages("party")
install.packages("knitr")
install.packages("png")
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
setwd("D:/Data_science/Assignments/Assignment_Decision_tree")

FraudCheck <- read.csv(file.choose())

head(FraudCheck)

colnames(FraudCheck, do.NULL = TRUE, prefix = "col")

hist(FraudCheck$Taxable.Income)

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)

# Splitting data into training and testing.
# splitting the data based on Sales

FC_train <- FC[1:300,]
FC_test <- FC[301:600,]

###Using Party Function 

#considering whole data before splitting

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
windows()
plot(opall_tree)
# From the above tree, It looks like the data has 20 % of Risky patients and 80 % good patients


# using the training Data 

png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)

# prediction of the model

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)

#Model Performance

mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)
