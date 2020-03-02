install.packages('arules')
install.packages('arulesViz')

library(arules)
library(arulesViz)

Book = read.csv("D:/Data_science/Assignments/Assignment_Association---------------------------completed/Assign_Book/book.csv")
summary(Book)
str(Book)
#cor(Book)
library(dplyr)

con.names = Book %>% select_if(is.numeric) %>% colnames()
#con.names

Book[,con.names] = data.frame(apply(Book[con.names], 2, as.factor))
str(Book[,con.names])

Bookrules <- apriori(Book, parameter = list( support = 0.7, confidence = 0.7, minlen=2 ))
#No. of rules are 77
arules::inspect(Bookrules)

inspect(head(sort(Bookrules, by = "lift")))

head(quality(Bookrules))

plot(Bookrules,method = "scatterplot")

plot(Bookrules,method = "grouped")

# The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other chld, art, geo, Doit books)

plot(Bookrules,method = "graph")

#support and confidence level decreased
Bookrules1 <- apriori(Book, parameter = list( support = 0.6, confidence = 0.6, minlen=1 ))
#No. of rules are 349
arules::inspect(Bookrules1)

inspect(head(sort(Bookrules1, by = "lift")))

head(quality(Bookrules1))

plot(Bookrules1,method = "scatterplot")

plot(Bookrules1,method = "grouped")

# The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other chld, art, geo, Doit books)

plot(Bookrules1,method = "graph")




#support and confidence level decreased furter
Bookrules2 <- apriori(Book, parameter = list( support = 0.5, confidence = 0.7, minlen=1 ))
#No. of rules are 1193
arules::inspect(Bookrules2)

inspect(head(sort(Bookrules2, by = "lift")))

head(quality(Bookrules2))
#write(Bookrules, file = "Bookrules.csv")