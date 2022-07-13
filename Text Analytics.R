require(qdap)
require(qdapDictionaries)
require(tm)
require(wordcloud)
require(wordcloud2)

x=readLines("speech.txt")
x=paste(x, collapse = " ")
x=tolower(x)
t=Top200Words
x=removeWords(x, t)
x=removeWords(x, stopwords("en"))
x=gsub(pattern = "\\W", replacement = " ",x)
x=gsub(pattern = "\\d", replacement = " ",x)
x=gsub(pattern = "\\b[a-z]\\b{1}", replacement = " ",x)
x=stripWhitespace(x)
x

wordcloud(x, colors = rainbow(4), min.freq = "5", random.order = F )

xx=freq_terms(x)
wordcloud2(data=xx, minSize = 10)

require(stringr)
s=str_split(x, pattern = " ")
ss=unlist(s)

pos=readLines("PositiveWords.txt")
str(pos)
pos=tolower(pos)
pos=paste(pos, collapse = "")

neg=readLines("NegativeWords.txt")
str(neg)
neg=paste(neg, collapse = "")

m=match(x,pos)
sum(!is.na(m))