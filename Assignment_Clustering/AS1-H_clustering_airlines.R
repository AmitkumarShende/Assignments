# Load the data set
input = read.csv("D:/Data_science/Assignments/Assignment_clustering/EastWestAirlines.csv")
summary(input)

# Normalize the data set as input have large variation of variable values
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(input[,2:12],normalize))
summary(normalized_data)
cor(normalized_data) #Co-relation of the data

normalized_data <- scale(input[,2:12]) #excluding the ID Column as it has only seriel no. before normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="ward.D2")

plot(fit) # display dendrogram

plot(fit, hang=-1) # levels the segmaentation in single line
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Award<-as.matrix(groups)

final <- data.frame(input, Award)

aggregate(input[,2:7], by=list(final$Award), FUN=mean)

#Sample Split to check stability
require(caTools)
set.seed(101) #number can vary => 101 times with same samples

sample = sample.split(input$Award, SplitRatio = .95)# Award variable can be changed 
train = subset(input, sample == TRUE)
test  = subset(input, sample == FALSE)
getwd()
#write.xlsx
write.csv(final, file="final_airline.csv")


