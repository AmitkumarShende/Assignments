install.packages('arules')
install.packages('arulesViz')
install.packages('datasets')
library(arules)
library(arulesViz)
library(datasets)

groceries = read.transactions("D:/Data_science/Assignments/Assignment_Association---------------------------completed/Assign_groceries/groceries.csv",sep = ",")
summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[, 1:5])

itemFrequencyPlot(groceries, support = 0.1)


itemFrequencyPlot(groceries, topN = 5)

#Using an image() on the sprased matrix, 
#the first 5 transactions in the groceries data is observed. 
#Since columns represents unique items bought by the customer per transaction, 
#the black dot shown in the plot will indicates specific items bought per customer.

image(groceries[1:5])
groceryrules <- apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.005, confidence = 0.25, minlen = 2))
# 662 rules
arules::inspect(groceryrules[1:8])

arules::inspect(sort(groceryrules, by = "lift")[1:5])

plot(groceryrules,method = "scatterplot",jitter =0)

plot(groceryrules, method = "grouped")

# It looks like people who purchase Heart, Holder, T-Light and White would definitely purchase Hanging.
# People who purchase 72, Cake,Of,Pack would purchase Cases.

plot(groceryrules,method = "graph")

# Incresed the  confidence
groceryrules1 <- apriori(groceries, parameter = list(support =
                                                      0.005, confidence = 0.5, minlen = 1))
# No of Rules decreased to 120
arules::inspect(groceryrules1[1:8])

arules::inspect(sort(groceryrules1, by = "lift")[1:5])

plot(groceryrules1,method = "scatterplot",jitter =0)

plot(groceryrules1, method = "grouped")

plot(groceryrules,method = "graph")

write(groceryrules1, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

groceryrules1_df <- as(groceryrules1, "data.frame")

str(groceryrules1_df)
