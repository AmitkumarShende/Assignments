install.packages('arules')
install.packages('arulesViz')

library(arules)
library(arulesViz)

Mymovies = read.csv("D:/Data_science/Assignments/Assignment_Association---------------------------completed/Assign_My_movies/my_movies.csv")
View(Mymovies)
Mymovies<-Mymovies[6:15]
Mymovies=as.matrix(Mymovies)
str(Mymovies)
rules = apriori((Mymovies))


# No of rules 77
rules <- apriori(as.matrix(Mymovies, parameter =list(support=0.2, confidence = 0.5,minlen=1)))
# No of rules 77
rules <- apriori(as.matrix(Mymovies, parameter =list(support=0.005, confidence = 0.1,minlen=1)))
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
plot(rules,method = "scatterplot")

plot(rules, method = "grouped")

# It looks ike most of them has wateched Lord of the rings movies along with Gladiator and Greenville
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 
plot(rules,method = "graph")



# No of support and confidence is decreased
rules1 <- apriori(as.matrix(Mymovies, parameter=list(support=0.005, confidence = 0.25,minlen=2)))

inspect(head(sort(rules1, by = "lift")))  

head(quality(rules1))



