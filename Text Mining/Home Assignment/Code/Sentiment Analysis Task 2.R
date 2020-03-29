library(dplyr)
library(readr)
library(base)
library(ggplot2)
library(tm)
library(stringr)
library(wordcloud)
library(corpus)
library(tidytext)
library(data.table)
#library(tidyverse)
library(wordcloud2)
library(reshape2)
library(radarchart)
library(RWeka)
library(textdata)
library(DT)
library(igraph)
library(ggraph)
library(caret)

cleanCorpus <- function(corpus){
  
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"), c("thats","weve","hes","theres","ive","im",
                                           "will","can","cant","dont","youve","us",
                                           "youre","youll","theyre","whats","didnt"))
  corpus.tmp <- tm_map(corpus.tmp, removeWords, v_stopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  return(corpus.tmp)
  
}

frequentTerms <- function(text){
  
  s.cor <- Corpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}

textfiles <- ("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files/Aviation copy")
setwd(textfiles)
allfiles <- list.files(path = textfiles, pattern = "*.txt")
#allfiles
allfiles <- paste(textfiles, "/", allfiles, sep="")
#allfiles
typeof(allfiles)
class(allfiles)

newpath <- ("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files")
setwd(newpath)
datan <- lapply(allfiles, FUN = readLines)
#datan
newdata <- lapply(datan, FUN = paste, collapse = " ")
#write.table(newdata, file = "without.txt")


newdata2 <- gsub(pattern = "\\W", replace = " ", newdata)
newdata2 <- gsub(pattern = "\\d", replace= " ", newdata2)
newdata2 <- tolower(newdata2)
newdata2 <- removeWords(newdata2, stopwords("english"))
newdata2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace= " ", newdata2)
newdata2 <- stripWhitespace(newdata2)

words <- str_split(newdata2, pattern = "\\s+")

class(words)
words <- unlist(words)
write.table(words, file = "afterclean.txt")
newtable <- read.table("afterclean.txt", comment="", header=TRUE)
write.csv(newtable, "afterclean.csv", row.names=FALSE, quote=FALSE)


frame <- read.table("afterclean.csv")
View(frame)
length(frame$V1)
length(levels(frame$V1))
top.chars <- as.data.frame(sort(table(frame$V1), decreasing=TRUE))[1:50,]
ggplot(data=top.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1))

wordcloud2(frequentTerms(frame$V1), size=0.5)
wordcloud2(frequentTerms(frame$V1), size=1, figPath = "~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files/Airplane.png")

tokens <- frame %>%  
  mutate(V1=as.character(frame$V1)) %>%
  unnest_tokens(word, V1)

tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=100)

sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 


#Another Sentiment Analysis
newdata2 <- gsub(pattern = "\\W", replace = " ", newdata)
newdata2 <- gsub(pattern = "\\d", replace= " ", newdata2)
newdata2 <- tolower(newdata2)
newdata2 <- removeWords(newdata2, stopwords("english"))
newdata2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace= " ", newdata2)
newdata2 <- stripWhitespace(newdata2)

words <- str_split(newdata2, pattern = "\\s+")

class(words)
words <- unlist(words)
write.table(words, file = "afterclean.txt")
newtable <- read.table("afterclean.txt", comment="", header=TRUE)
write.csv(newtable, "afterclean.csv", row.names=FALSE, quote=FALSE)
#OR
write.table(words, file = "beforeclean.txt")
newtable <- read.table("beforeclean.txt", comment="", header=TRUE)
write.csv(newtable, "beforeclean.csv", row.names=FALSE, quote=FALSE)

wordcloud(newdata2, min.freq=2000, random.order=FALSE, scale = c(3,0.5), col=rainbow(3))
comparison.cloud(newdata2)
newdata3 <- Corpus(VectorSource(newdata2))
newdata3
tdm <- TermDocumentMatrix(newdata3)
tdm
mat <- as.matrix(tdm)
a<- rownames(tdm)
colnames(mat)
comparison.cloud(mat)
#data(mat)
#View(mat)


#Sentiment Analysis
postive <- scan("postivewords.txt",what='character',comment.char=";")
negative <- scan("negativewords.txt",what='character',comment.char=";")
str(postive)

newdataforsem <- str_split(newdata2, pattern = "\\s+")
sumofpos <- lapply(newdataforsem, function(x){sum(!is.na(match(x, postive)))})
sumofpos
sumofneg <- lapply(newdataforsem, function(x){sum(!is.na(match(x, negative)))})
sumofneg
total <- lapply(newdataforsem, function(x){sum(!is.na(match(x, postive))) - sum(!is.na(match(x,negative)))})
total
total <- unlist(total)
total
mean(total)
sd(total)
hist(total)
