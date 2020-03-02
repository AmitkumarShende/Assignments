install.packages("tree")
install.packages("caret")
install.packages("gmodels")
install.packages("party")

library(party)
library(caret)
library(tree)
library(plyr)
library(readr)

data("iris")
View(iris)
head(iris)

#change iris class into a factor 
iris$Species<-as.factor(iris$Species) 

#overall descriptives of the dataset 
summary(iris)

#######  function 'ctree() ##########
#The function ctree() is used to create conditional inference trees. 
#The main components of this function are formula and data. 
#Other components include subset, weights, controls, xtrafo, ytrafo, and scores.

#Decision Trees using 1 variable
#First we will create 4 different decision trees consisting of one variable to predict which 
#Species of iris a given iris belongs to

#Tree1==(Sepal.Length)
tree1<-ctree(Species~Sepal.Length, data=iris) #set the model for the tree, predicting class by sepal length , data set being used is iris 
windows()
plot(tree1) #view the decision tree
table(predict(tree1), iris$Species)

#Tree2==(Sepal.Width)
tree2<-ctree(Species~Sepal.Width, data=iris) #set the model for the tree, predicting class by sepal length , data set being used is iris 
windows()
plot(tree2) #view the decision tree
table(predict(tree2), iris$Species)

#Tree3==(Petal.Length)
tree3<-ctree(Species~Petal.Length, data=iris) #set the model for the tree, predicting class by sepal length , data set being used is iris 
windows()
plot(tree3) #view the decision tree
table(predict(tree3), iris$Species)

#Tree4==(Petal.Width)
tree4<-ctree(Species~Petal.Width, data=iris) #set the model for the tree, predicting class by sepal length , data set being used is iris 
windows()
plot(tree4) #view the decision tree
table(predict(tree4), iris$Species)

#Decision Trees with 2 variables

#With Sepal Length and Sepal Width
tree5<-ctree(Species~Sepal.Length+Sepal.Width, data=iris)
windows()
plot(tree5)#view the decision tree
table(predict(tree5), iris$Species)

#With Petal Length and petal Width
tree6<-ctree(Species~Petal.Length+Petal.Width, data=iris)
windows()
plot(tree6)#view the decision tree
table(predict(tree6), iris$Species)


#Decision Trees with all variables
tree7<-ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
windows()
plot(tree7)#view the decision tree
table(predict(tree7), iris$Species)
