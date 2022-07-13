library(tidyverse) 
library(ggplot2)
library(stringr)
library(caret)
library(quanteda)
library(doSNOW)
library(e1071)
library(irlba)
library(tidytext)
library(textdata)
library(keras)
library(wordcloud)
library(reshape2)
library(tm)
library(ROCR)
library(plotROC)

input_file <- "Gungor_2018_VictorianAuthorAttribution_data-train.csv"

read_data <- function(num) {
  dataset <- read.csv(input_file, header = T, stringsAsFactors = FALSE, nrows = num)
}

df <- read_data(1294)

split <- function(data) {
  splits <- sample(1:3, size = nrow(df), prob = c(.5, .2, .3), replace = T)
  training <<- df[splits == 1,]
  validation <<- df[splits == 2,]
  testing <<- df[splits == 3,]
}
split(df)


shuffleRows <- function(df){
  return(df[sample(nrow(df)),])
}

prob_dist <- function(x) {
  prop.table(summary(x))
}
prob_dist(df$label[-1])


training <- shuffleRows(training)
validation <- shuffleRows(validation)
testing <- shuffleRows(testing)

bag_of_words <- function(data, word, ngram) {
  corpus <- tokens(data, what = word, remove_numbers = TRUE,remove_punct = TRUE, 
                   remove_symbols = TRUE, ngrams = ngram, remove_url = TRUE)
  corpus <- tokens_tolower(corpus)
  corpus <- tokens_select(corpus, stopwords(), selection = "remove")
  corpus <- tokens_wordstem(corpus, language = quanteda_options("language_stemmer"))
  corpus_dfm <- dfm(corpus, tolower = F)
}

corpus_prep <- function(training) {
  corpus_dfm <- bag_of_words(training$text, "word",  ngram = 1L)
  corpus <- as.matrix(corpus_dfm)
  corpus <- cbind(label = training$label, data.frame(corpus))
}

corpus <- corpus_prep(training)
names(corpus) <-  make.names(names(corpus))

corpus_process <- function(training) {
  corpus_dfm <- bag_of_words(training$text, "word",  ngram = 1L)
  corpus_tfidf <- dfm_tfidf(corpus_dfm, scheme_tf = "count", scheme_df = "inverse", base = 10)
  corpus_tfidf <- cbind(label = training$label, convert(corpus_tfidf, to = "data.frame"))
}

corpus_tfidf <- corpus_process(training)
names(corpus_tfidf) <-  make.names(names(corpus_tfidf))

cv_folds <- function() {
  multifolds <- createMultiFolds(validation$label, k = 10, times = 2)
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, 
                          index = multifolds, verboseIter = T)
}
cv <- cv_folds()




corpus_tfidf <- droplevels(corpus_tfidf) 
colnames(corpus_tfidf) <- make.names(colnames(corpus_tfidf))
corpus_tfidf <- corpus_tfidf[, !duplicated(colnames(corpus_tfidf))]

# Random Forest Model 
model <- function(label, ., corpus, method, cv) {
  cm <- train(label~., data = corpus, method = method, trControl = cv, tuneLength = 7)
}

# Training the model 
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
start_time <- Sys.time()
rf_model <- model(label, ., corpus, "rf", cv)
stopCluster(cl)
Sys.time() - start_time

rf_model

# Checking var importance
var_imp <- varImp(rf_model)

svm_imp <- varImp(svm_model)
tibble(var_imp)
ggplot(rf_model, top = dim(data$importance)[1]) +
  labs(title="Random Forest Model")
ggsave("var_imp.png")

