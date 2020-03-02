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
# IMDBReviews #############################
aurl <- "https://www.imdb.com/title/tt1477834/reviews?ref_=tt_ov_rt"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"Aquaman.txt",row.names = F)


Aquaman <- read.delim('Aquaman.txt')
str(Aquaman)

View(Aquaman)

# Build Corpus and DTM/TDM
library(tm)
corpus <- iconv(Aquaman[-1,], to = "UTF-8")
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


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

cleanset<-tm_map(cleanset,removeWords, c('can','film'))
inspect(cleanset[1:5])

# Since the word laptop and can were used, this can be removed as we are 
# mining the tweets for this film.Also the word "Can" is common english word.
# we can pull back the word "can"  if needed.

cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))

# Removing the word movie and movies on similar grounds - as unnecessary.


cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
# the barplot pulls both character and characters as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
# the terms indicate that there are 1073 words and 479 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# the word Aquaman,Like and James as the highest frequency. This implies
# that Movie Aquaman has got more reviews about the James and 
# most of them liked the movie.

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

# Sentiment Analysis for IMDB

# install.packages("syuzhet")

# Read File 
IMDB_reviews <- read.delim('Aquaman.TXT')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[6]
# I have no idea why som6ebody would give this movie a good score, let alone 10. After 20 minutes I have decided that its for the best to switch my brain off and enjoy the show
# on review 6, you have 4 for anger,4 each for anti, fear, surprise and 5 for sadness,7 for trust , 11 words for negative and 9 positive.
get_nrc_sentiment('twists')

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for Aquaman')
