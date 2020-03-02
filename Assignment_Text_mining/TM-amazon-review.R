library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(devtools)
library(tm)
library(slam)
library(topicmodels)
install.packages("devtools")
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

setwd("D:/Data_science/Assignments/Assignment_Text_mining")
# Amazon Reviews #############################
aurl <- "https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_3?showViewpoints=1&pageNumber"
amazon_reviews <- NULL
for (i in 1:30){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
amazon_reviews
length(amazon_reviews)
#write.csv(amazon_reviews, "AppleMacbook.csv",row.names = F)
write.table(amazon_reviews,"AppleMacbook.txt",row.names = F)

AppleMacbook_Lap <- read.delim('AppleMacbook.txt')
str(AppleMacbook_Lap)

View(AppleMacbook_Lap)

# Build Corpus and DTM/TDM
library(tm)
#corpus1 <- AppleMacbook_Lap[-1,]
# Build Corpus and DTM/TDM
corpus1 <- iconv(AppleMacbook_Lap[-1,], to = "UTF-8")
View(corpus1)
head(corpus1)

class(corpus1)

corpus <- Corpus(VectorSource(corpus1))

# Clean the text 

corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('apple', 'amazon','just','air','macbook'))
cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))
inspect(cleanset[1:5])

# Since the word Laptop and can is used more, this can be removed as we are 
# mining the tweets for this laptop only.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.


cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')
# the barplot pulls both Computer and Machine as separate words. this should be 
# counted as one as both holds the same synonym.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
# the terms indicate that there are 2224 words and 1141 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word product,good and battery as the highest frequency. This implies
# that laptop has got more reviews about the product,good and battery and 
# most of them liked the laptop.

# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
#set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
windows()
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)
# lettercloud 

letterCloud(w,word = 'A',frequency(5), size=1)

# install.packages("syuzhet")

# Read File 
AppleMacbook_Lap_reviews <- read.delim('AppleMacbook.TXT')
reviews <- as.character(AppleMacbook_Lap_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
# Light weight and super fast response timeLight weight and super fast response time2. Highly optimize which avoids any kind of process lag.3. Beautiful looks and feels like a prime product.4. Long battery life and awesome sound clarity5. Purchased during Amazon sale cost around 52k.Cons:1. Not enough memory2. Limited softwares available3. Operating system is not that user-friendlyProbably one of the best option for 50k+ laptops
# on review 4, you have 2 for anger, each one work for disgust, fear 
# and 2 for sadness,3 for joy, 3 for trust , 4 words for negative and 5 positive.
get_nrc_sentiment('Beautiful')

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for amazon Reviews
        for Apple Macbook')
