install.packages("Matrix")
library(recommenderlab)
library(dplyr)
library(caTools)
library(reshape2)
library(Matrix)
library(data.table)
library(ggplot2)

setwd("D:/Data_science/Assignments/Assignment_Recommandataion")
#book rating data
book_rate_data <- read.csv("D:/Data_science/Assignments/Assignment_Recommandataion/books.csv", header=TRUE)

book_rate_data <- book_rate_data[,-c(1)]   # removing column 1 as not required for analysis

names(book_rate_data)[1]<-"users" #.....[1]--column number, "users"----name to change for R
names(book_rate_data)[5]<-"ratings" #.....[5]--column number, "retings"----name to change for R
names(book_rate_data)[2]<-"bookname" #.....[2]--column number, "bookname"----name to change for R
names(book_rate_data)[3]<-"author" #.....[3]--column number, "author"----name to change for R

head(book_rate_data)

### Removing Duplicated Entries
book_rate_data<- book_rate_data[!duplicated(book_rate_data$bookname), ]

### Ordering tha data according to ratings
book_rate_data <- book_rate_data[order(book_rate_data[,5]), ]

### use only users with more than 0 ratings
book_rate_data <- filter(book_rate_data, book_rate_data$ratings >= 1)

#rating distribution
hist(book_rate_data$ratings)

## covert to matrix format
book_rate_data_g<-acast(book_rate_data, bookname ~ ratings)
class(book_rate_data_g)
book_rate_dataM<-as.matrix(book_rate_data_g)
dim(book_rate_dataM)

#the datatype should be realRatingMatrix inorder to build recommendation of books

book_rate_data_matrix <- as(book_rate_dataM, 'realRatingMatrix')

#View in other possible ways

book_rate_data_matrix
as(book_rate_data_matrix, "list")


#User Based model for recommender base

book_recomm_model1 <- Recommender(book_rate_data_matrix, method="POPULAR")
book_recomm_model2 = Recommender(book_rate_data_matrix, method="UBCF") ## User-based collaborative filtering
book_recomm_model3 = Recommender(book_rate_data_matrix, method="IBCF") ## Item-based collaborative filtering

#User Based Popular Filtering
#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, book_rate_data_matrix, n=5)
R1<-as(recommended_items1, "list")
head(R1)
tail(R1)

## Popularity model recommends the same books for all users , we need to improve our model 

#User Based Collaborative Filtering
#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, book_rate_data_matrix, n=5)
R2<-as(recommended_items2, "list")
head(R2)
tail(R2)

# Item-based collaborative filtering
#Predictions for two users 
recommended_items3 <- predict(book_recomm_model3, book_rate_data_matrix, n=5)
R3<-as(recommended_items3, "list")
head(R3)
tail(R3)
