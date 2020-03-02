#**************install the packages if unavailable**************
#install.packages('caret')
#install.packages('dplyr')

library(caret)
# Read the dataset
zoo <- read.csv(file.choose())
#EDA
table(zoo$type)

summary(zoo)

str(zoo)
# excluding 1st column having categorical values
zoo1 <- zoo[,2:18]
str(zoo1)

#converting int variable type to factor type
library(dplyr)
con.names = zoo1 %>% select_if(is.numeric) %>% colnames()
#con.names
zoo1[,con.names] = data.frame(apply(zoo1[con.names], 2, as.factor))
str(zoo1)

# Data partition
set.seed(123)
ind <- sample(2,nrow(zoo1), replace = T, prob = c(0.7,0.3))
train <- zoo1[ind==1,]
test <- zoo1[ind==2,]

#Creating performance Model

# KNN Model 

trcontrol <- trainControl(method = "repeatedcv", number = 10,repeats = 3
                          # classprobs are needed when u want to select ROC for optimal K Value
)
set.seed(123)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = trcontrol, preProc = c("center","scale"))

# Default metric is accuracy but if u want to use ROC, then mention the same
# Model Performance :
fit # the optimum value for k should be 5

plot(fit)

varImp(fit)

pred <- predict(fit, newdata = test )

confusionMatrix(pred, test$type) #Accuracy of modelis 86.21
