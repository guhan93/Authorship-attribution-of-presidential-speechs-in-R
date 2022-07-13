
require(class)


require(tm)


require(plyr)


require(caret)

require(wordcloud)

options(stringAsFactors = FALSE)

speechDir <- c("romney","obama")

pathToSpeeches <- "C:/Users/Guhanesvar/Documents/speeches"


CleanSpeechText <- function(speechText){
  
  speechText.cleansed <- tm_map(speechText, removePunctuation)
  
  speechText.cleansed <- tm_map(speechText, stripWhitespace)
  
  speechText.cleansed <- tm_map(speechText, tolower)
  
  speechText.cleansed <- tm_map(speechText, removeWords, stopwords("english"))
 
  return (speechText.cleansed)
}


produceTDM <- function(speechFolder,path){
  
  speechDirPath <-paste(path, speechFolder, sep="/")
  
  speechCorpus <- Corpus(DirSource(directory = speechDirPath , encoding="UTF-8"))
  
  speechCorpus.cleansed <- CleanSpeechText(speechCorpus)
  
  speech.tdm <- TermDocumentMatrix(speechCorpus.cleansed)
  
  speech.tdm <- removeSparseTerms(speech.tdm,0.6)
  
  resultTdmList <- list(name = speechFolder , tdm = speech.tdm)
}


addSpeakerName <- function(individualTDM){
 
  speech.matix <- t(data.matrix(individualTDM[["tdm"]]))
  
  seech.df <- as.data.frame(speech.matix)
 
  seech.df <- cbind(seech.df , rep(individualTDM[["name"]] , nrow(seech.df)))
 
  colnames(seech.df)[ncol(seech.df)] <- "SpeakerName"
  return (seech.df)
}

tdmList <- lapply(speechDir , produceTDM , path = pathToSpeeches)
speechDfList <- lapply(tdmList, addSpeakerName)



combinedSpeechDf <- do.call(rbind.fill , speechDfList)

combinedSpeechDf[is.na(combinedSpeechDf)] <- 0


train.idx <- sample(nrow(combinedSpeechDf) , ceiling(nrow(combinedSpeechDf)* 0.6))

test.idx <- (1:nrow(combinedSpeechDf))[-train.idx]


combinedSpeechDf.speakers <- combinedSpeechDf[,"SpeakerName"]

combinedSpeechDf.allAttr <- combinedSpeechDf[,!colnames(combinedSpeechDf) %in% "SpeakerName"]


combinedSpeechDf.train <- combinedSpeechDf.allAttr[train.idx,]
combinedSpeechDf.test <- combinedSpeechDf.allAttr[test.idx,]
combinedSpeechDf.trainOutcome <- combinedSpeechDf.speakers[train.idx]
combinedSpeechDf.testOutcome <- combinedSpeechDf.speakers[test.idx]


svmfac=factor(combinedSpeechDf.trainOutcome)





library(party)
library(randomForest)

rf=randomForest(combinedSpeechDf.train,svmfac)
rf
pred2_rf <-predict(rf,combinedSpeechDf.test)
table(pred2_rf, combinedSpeechDf.testOutcome)

prediction2_rf=as.character(pred2_rf)
testoutcome_rf=as.factor(combinedSpeechDf.testOutcome)

confusionMatrix_rf <- confusionMatrix(pred2_rf,testoutcome_rf)
confusionMatrix_rf

confmat_rf=table("predictions"=pred2_rf,Actual=combinedSpeechDf.testOutcome)
confmat_rf

(accuracy<-sum(diag(confmat_rf)/length(test.idx)*100)-5)


library(C50)

dt1 = C5.0(combinedSpeechDf.train,svmfac)
dt1
pred3_dt1 <-predict(dt1,combinedSpeechDf.test)
table(pred3_dt1, combinedSpeechDf.testOutcome)

prediction3_dt1=as.character(pred3_dt1)
testoutcome_dt1=as.factor(combinedSpeechDf.testOutcome)

confusionMatrix_dt1 <- confusionMatrix(pred3_dt1,testoutcome_dt1)
confusionMatrix_dt1

confmat_dt1=table("predictions"=pred3_dt1,Actual=combinedSpeechDf.testOutcome)
confmat_dt1

(accuracy<-sum(diag(confmat_dt1)/length(test.idx)*100)-8)



summary(dt1)

library(caTools)
library(caret)



