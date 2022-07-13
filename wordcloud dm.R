library(wordcloud)
library(tm)
library(readr)
library(dplyr)



wc=readLines("C:/Users/Guhanesvar/Documents/obamaone.txt") 
wc
wc_corpus=Corpus(VectorSource(wc))
inspect(wc_corpus)

wc_clean_corpus=tm_map(wc_corpus,tolower)
wc_clean_corpus=tm_map(wc_clean_corpus,removeNumbers)
wc_clean_corpus=tm_map(wc_clean_corpus,removePunctuation)
wc_clean_corpus=tm_map(wc_clean_corpus,stripWhitespace)
wc_clean_corpus=tm_map(wc_clean_corpus,removeWords,stopwords())
wc_clean_corpus=tm_map(wc_clean_corpus,stemDocument)

inspect(wc_clean_corpus[1:10])



dtm=DocumentTermMatrix(wc_clean_corpus)
dtm2=as.matrix(dtm)



dtm_v <- sort(colSums(dtm2),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)


barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


findAssocs(dtm, terms = c("will","change","american","time","war"), corlimit = 0.25)			

findAssocs(dtm, terms = findFreqTerms(dtm, lowfreq = 50), corlimit = 0.25)

library(syuzhet)

syuzhet_vector <- get_sentiment(wc, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)


d<-get_nrc_sentiment(wc)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

td<-data.frame(t(d))
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
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


