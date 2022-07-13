libs<-c("tm","plyr","class")
lapply(libs,require,character.only=TRUE)

options(stringsAsFactors = FALSE)

candidates<-c("oboma","bill")
pathname<-"C:/Users/Guhanesvar/Documents/dmpro"

cleanCorpus<-function(corpus){
  corpus.tmp<-tm_map(corpus,removePunctuation)
  corpus.tmp<-tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp<-tm_map(corpus.tmp,content_transformer(tolower))
  corpus.tmp<-tm_map(corpus.tmp,removeWords,stopwords("english"))
  return(corpus.tmp)
}

generateTDM<-function(cand,path){
  s.dir<-sprintf("%s/%s",path,cand)
  s.cor<-Corpus(DirSource("C:/Users/Guhanesvar/Documents/dmpro",pattern = ".txt",mode = "text",encoding = "UTF-8"))
  s.cor.cl<-cleanCorpus(s.cor)
  s.tdm<-TermDocumentMatrix(s.cor.cl)
  
  s.tdm<-removeSparseTerms(s.tdm,0.7)
  result<-list(name=cand,tdm=s.tdm)
}

tdm=lapply(candidates,generateTDM,path=pathname)
