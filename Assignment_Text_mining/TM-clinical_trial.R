#Extract the clinical trails performed on Hepatocellular Carcinoma
#patients on the outcome events to perform word cloud and sentimental
#analysis to predict the outcome of the results.

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
install.packages("wordcloud2")
library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
install_github("sachsmc/rclinicaltrials")
library(rclinicaltrials)
setwd("D:/Data_science/Assignments/Assignment_Text_mining")

# ClinicalTrailReviews #############################

z <- clinicaltrials_download(query = 'Hepatocellular Carcinoma', count = 10, include_results = TRUE)
str(z)

zstudyinfo <- z$study_information$study_info
zoutcomes <- z$study_information$outcomes
zinterventions <- z$study_information$interventions
zoutcomes <- as.character(zoutcomes)
zinterventions <- as.character(zinterventions)
zhcc <- rbind(zoutcomes,zinterventions)

write.table(zhcc,"zhcc.txt",row.names = F)
write.table(zinterventions,"inv.txt",row.names = F)
write.table(zoutcomes,"out.txt",row.names = F)

hcc <- read.delim('out.txt')
str(hcc)


# Build Corpus and DTM/TDM
library(tm)
corpus <- iconv(hcc[-1,], to = "UTF-8")
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

cleanset<-tm_map(cleanset,removeWords, c('nct','cnct'))
# Since the nct and cnct  were used, this can be removed as we 
# dont need those registry numbers for this exercise.


# cleanset <- tm_map(cleanset, gsub,pattern = 'Compared', replacement = 'Comparison')
# the barplot pulls both Compare and Comparison as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 276 words and 4 documents in this TDM
# Sparsity is 74% which indicates that there are zero values
tdm <- as.matrix(tdm)
tdm[1:10,1:4]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 5) # Pull words that were used more than 2 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
#set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)

# Sentiment Analysis for tweets:


# Read File 
HCC_Outcome <- read.delim('out.TXT')
trail_out <- as.character(HCC_Outcome[-1,])
class(trail_out)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(trail_out)

head(s)

get_nrc_sentiment('diagnosis')

# diagnosis has 1 anticipation, 1 fear, 1 trust and 1 negative value
get_nrc_sentiment('death') 

# death has one anger, one anticpation, 1 disgust, 1 fear, 1 sadness, 
# 1 surprise, and 1 Negative



# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for HCC Clinical Trail')

# Overall, it looks like the outcome had lots of Anticipation on the 
# results, encountered lots of fear , anger. Most of their work had a 
# lot of negative thoughts with sadness. However the outcome also showed
# great signs of trust which show cased a lot of positive outcomes.
