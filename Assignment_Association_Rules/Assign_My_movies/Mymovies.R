install.packages('arules')
install.packages('arulesViz')

library(arules)
library(arulesViz)

Mymovies = read.csv("D:/Data_science/Assignments/Assignments_association/Assign_My_movies/my_movies_final.csv")
View(Mymovies)
str(Mymovies)

rules <- apriori(as.matrix(Mymovies,parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules

inspect(head(sort(rules, by = "lift")))  

head(quality(rules))



