library(wordcloud)
library(tm)
library(readr)
library(dplyr)



wc1=readLines("C:/Users/Guhanesvar/Documents/romneyone.txt") 
wc1
wc_corpus1=Corpus(VectorSource(wc))
inspect(wc_corpus1)

wc_clean_corpus1=tm_map(wc_corpus,tolower)
wc_clean_corpus1=tm_map(wc_clean_corpus,removeNumbers)
wc_clean_corpus1=tm_map(wc_clean_corpus,removePunctuation)
wc_clean_corpus1=tm_map(wc_clean_corpus,stripWhitespace)
wc_clean_corpus1=tm_map(wc_clean_corpus,removeWords,stopwords())
wc_clean_corpus1=tm_map(wc_clean_corpus,stemDocument)

inspect(wc_clean_corpus1[1:10])



dtm1=DocumentTermMatrix(wc_clean_corpus)
dtm21=as.matrix(dtm)



dtm_v1 <- sort(colSums(dtm2),decreasing=TRUE)
dtm_d1 <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)


barplot(dtm_d1[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d1$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


findAssocs(dtm1, terms = c("will","presid","america","nation","world"), corlimit = 0.25)			

findAssocs(dtm1, terms = findFreqTerms(dtm1, lowfreq = 50), corlimit = 0.25)

library(syuzhet)

syuzhet_vector <- get_sentiment(wc1, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)


d1<-get_nrc_sentiment(wc1)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d1,10)

td<-data.frame(t(d1))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

barplot(
  sort(colSums(prop.table(d1[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


