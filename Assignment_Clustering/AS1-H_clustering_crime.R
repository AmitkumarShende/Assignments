# Load the data set
input = read.csv("D:/Data_science/Assignments/Assignment_clustering/crime_data.csv")
summary(input)

# Normalize the data set as input have large variation of variable values
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(input[,2:5],normalize))
summary(normalized_data)
cor(normalized_data) #Co-relation of the data

normalized_data <- scale(input[,2:5]) #excluding the X Column has categorical data before normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="complete")

plot(fit) # display dendrogram

plot(fit, hang=-1) # levels the segmaentation in single line
groups <- cutree(fit, k=4) # cut tree into 4 clusters

rect.hclust(fit, k=4, border="red")

#Sample Split to check stability
require(caTools)
set.seed(101) #number can vary => 101 times with same samples

sample = sample.split(input$Rape, SplitRatio = .95)# Rape variable can be changed 
train = subset(input, sample == TRUE)
test  = subset(input, sample == FALSE)

#********************END******************