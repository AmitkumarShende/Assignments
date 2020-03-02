library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
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
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth") # Third party Anothitication with R

setwd("D:/Data_science/Assignments/Assignment_Text_mining")

cred <- OAuthFactory$new(consumerKey='1g2jM8GbPTS485dGxT7qT6QLS', # Consumer Key (API Key)
                         consumerSecret='HZtdKswqrxOoHQ3mex1CtjEdVMq6x9dHFUhrf3cwrxtPJ53Pf2', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata") # credential

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc) #encoding

#install.packages("httpuv")
library(httpuv) #Provides low-level socket and protocol support for handling HTTP and WebSocket requests directly from within R 

setup_twitter_oauth("1g2jM8GbPTS485dGxT7qT6QLS", # Consumer Key (API Key)
                    "HZtdKswqrxOoHQ3mex1CtjEdVMq6x9dHFUhrf3cwrxtPJ53Pf2", #Consumer Secret (API Secret)
                    "268613265-Q3VqZ1MypXzzLbC4eYkH6Vmp0SS9TTjV3HNVP6ui",  # Access Token
                    "zxTHA9D7pxKFBGIQeLoG7PYEbobXTwVB6kcyRrNhqZs5o")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('@ShashiTharoor', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
#texttweet<-TweetsDF[,1]

#write.csv(TweetsDF, "ShashiTharoor.csv",col.names = F)
write.csv(TweetsDF, "ShashiTharoor.csv",row.names = F)

###txt = readLines(file.choose())
txt <- read.csv(file.choose())

View(txt)
str(txt)
length(txt)
# Build Corpus and DTM/TDM
corpus <- iconv(txt$text, to = "UTF-8")
#corpus <- txt$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

removeURL <- function(x) gsub('https://[[alnum:]]*','',x)
corpus <- tm_map(corpus, content_transformer(removeURL))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(corpus[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(corpus[1:5])


cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
head(cleanset)
# the barplot pulls both page and pages as separate words. this should be 
# counted as one.
cleanset<-tm_map(cleanset,removeWords, c('amp', 'uuu', 'uub','uuuu', 'shashitharoor', '...', '"'))
cleanset<-tm_map(cleanset,removeWords, c('...'))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 4772 words and 1000 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25 ) # Pull words that were used more than 25 times.
head(w)
barplot(w, las = 2, col = rainbow(50))

# the word India as the highest frequency. This implies
# that twitterhandle is more concerned about political account

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
#set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 100,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')

#wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
wordcloud2(w,size = 1, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 
#letterCloud(w,word = "F",frequency(5), size=1)
letterCloud(w,word = "F", size=1)

# Sentiment Analysis for tweets:


# install.packages("syuzhet")

# Read File 
twdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(twdata$text)
class(tweets)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweets)

head(s)

tweets[4]

# "@prpltnkr Hi there. You can learn how to report a #Page that's 
# pretending to be you in our Help Center: https://t.co/n1CJLpv30Z. -KN
# the above tweet has value 1 for anger, value 1 for Negative 
# and value 2 for positive which reinstates that it has a mixture of 
# all three emotions in the above statement.
get_nrc_sentiment('pretending')

# Pretend has one value of negative and one value for anger
get_nrc_sentiment('can learn') #1 for positive

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Tweets')
