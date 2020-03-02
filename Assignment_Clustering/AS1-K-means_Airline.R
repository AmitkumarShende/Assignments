install.packages("plyr") # data manipulation package
install.packages("animation")# data animation plot package

library(plyr)
library(animation)

input = read.csv("D:/Data_science/Assignments/Assignment_clustering/EastWestAirlines.csv")

normalized_data <- scale(input[,2:12])

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))	# Determine number of clusters by scree-plot 

for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)

plot(wss)

# Look for an "elbow" in the scree plot #

plot(wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
title(sub = "K-Means Clustering Scree-Plot")

plot(normalized_data)

fit <- kmeans(normalized_data, 3) # 3 cluster solution
fit
km <- kmeans.ani(normalized_data, 3) # To check how the clustering is happening

final<- data.frame(input, fit$cluster) # append cluster membership

names(final)

final

#Arranging according to cluster solution

final2<- final[order(as.integer(fit$cluster),decreasing = FALSE),]

aggregate(input[,2:12], by=list(fit$cluster), FUN=mean)

table(fit$cluster)
