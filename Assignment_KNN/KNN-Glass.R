install.packages('caret')
library(caret)
# Read the dataset
glass <- read.csv(file.choose())
#View(glass)
#glass <- glass[,c(ncol(glass),1:(ncol(glass)-1))]


#table of Type 1 <- 70 and 2 <- 76 .....Type 7 <-29
table(glass$Type)

# Replace 1 with Type1 and 2 with Type2 and so on. We also replacing these enteries with Type1, Type2 and so on...
glass$Type <- factor(glass$Type, levels = c("1","2","3","5","6","7"), 
                     labels = c("Type1","Type2","Type3","Type5","Type6","Type7" ))

glass$Type <- as.factor(glass$Type) # Factorize the Type in Glass dataset
View(glass)
str(glass)

# Data partition
set.seed(123)
ind <- sample(2,nrow(glass), replace = T, prob = c(0.7,0.3))
train <- glass[ind==1,]
test <- glass[ind==2,]

train_labels<-glass[ind==1,10]
test_labels<-glass[ind==2,10]

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

#method = "repeatedcv", number = 10
#10-fold CV mean dividing your training dataset randomly into 10 parts 
#and then using each of 10 parts as testing dataset for the model trained on other 9

#repeats: For repeated k-fold cross-validation only: the number of complete sets of folds to compute

set.seed(123)
fit <- train(Type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))
# default metric is accuracy but if u want to use ROC, then mention the same
# Model Performance :
fit # the optimum value for k is 5

#Plotting the data
plot(fit)

#Checking Variable Importance
varImp(fit)

pred <- predict(fit, newdata = test )

confusionMatrix(pred, test$Type)   # 68.42 % is accuracy
