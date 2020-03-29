install.packages("ggridges")
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
library(tidyverse)
library(wordcloud2)
library(reshape2)
library(radarchart)
#library(RWeka)
library(topicmodels)
library(ggridges)

#library(pryr)
mem_used()
??pryr
?dplyr
?memory.limit
gc()
setwd("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files/CSV")
?lapply
?base
combined = list.files(pattern = "*.csv")
myfiles = lapply(combined, read.delim)
#or
df <- list.files(full.names = TRUE) %>% 
  lapply(read_csv, skip = 1) %>% 
  bind_rows 
dim(df)
df
str(df)
setwd("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files")
write.csv(df,'combinedfile.csv')
names(df)
df
tempdf <- read.table("combinedfile.csv",header = TRUE,sep =",", dec=".")
names(tempdf)
ggplot(tempdf, aes(Publication.Year, fill = Size)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, fill = Region)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year)) + 
  geom_line() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, fill = Sector)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, fill = Type)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, fill = Listed.Non.listed)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, Country, fill = Listed.Non.listed)) + 
  geom_raster() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, Status, fill = Listed.Non.listed)) + 
  geom_raster() +
  xlab("Publication Year")
#ggplot(tempdf, aes(Publication.Year, Country, fill = Listed.Non.listed)) + 
#  geom_hex() +
#  xlab("Publication Year")
#ggplot(tempdf, aes(Publication.Year, Status, fill = Listed.Non.listed)) + 
#  geom_hex() +
#  xlab("Publication Year")
#ggplot(tempdf, aes(Size,Publication.Year, fill = Listed.Non.listed)) + 
#  geom_raster() +
#  xlab("Publication Year")
ggplot(tempdf, aes(Size, Publication.Year)) + 
  geom_count() +
  ylab("Publication Year")
ggplot(tempdf, aes(Size, Publication.Year)) + 
  geom_violin() +
  ylab("Publication Year")
ggplot(tempdf, aes(Type, Publication.Year)) + 
  geom_violin() +
  ylab("Publication Year")
ggplot(tempdf, aes(Country)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempdf, aes(Publication.Year, Type, fill=Type))+
  geom_density_ridges() +
  labs(x ="Year of Publication", y = "GRI Standards", title = "All Companies") +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "buttom")
ggplot(tempdf, aes(Country, Sector, colour = Size)) +
  geom_point()

ggplot(tempdf[which(tempdf$Publication.Year == c(2001,2005, 2009, 2013, 2017 )),], 
       aes(Sector, fill = Size)) +
  scale_fill_manual(values = c("red4", "red2", "grey")) +
  geom_bar(colour = "black")+
  theme_bw() +
  facet_wrap(~Publication.Year, nrow = 5) +
  labs(title = "Number of publications by firms over different years and sectors:") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), 
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_text(size = 10),
    strip.background = element_rect(color = "white", fill = "white"),
    panel.border = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9, color = "black" ),
    plot.title = element_text(size = 12),
    legend.position = "top"
  ) 
# by number of companies per region over the years
ggplot(tempdf[which(tempdf$Publication.Year<2018),],aes(Region, Publication.Year,color = ..n..),alpha = 0.5) +
  geom_count(show.legend = TRUE) +
  scale_fill_continuous(name = "Number of Publications") +
  theme_bw() +
  guides(color = FALSE) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10, hjust = 1, angle = 90),
    legend.title = element_text(size = 10, hjust = 1, angle = 90),
    legend.key.size = unit(1, "cm"),
    
    legend.direction = "vertical",
    axis.text.x = element_text(color = "darkblue",angle = 90,hjust = 1, size = 10),
    axis.title.x = element_blank(),
    axis.text.y = element_text(color = "darkblue",size = 10, angle = 90),
    axis.title.y = element_blank(),
    panel.border = element_blank()
  ) + 
  coord_fixed(ratio = 0.7) +
  labs(title = "Publications per Regions per Year", subtitle = "(dot size represents number of publications)")




aviation <- filter(df, Sector == "Aviation")
aviation
write.csv(aviation,'aviation.csv')
tempav <- read.table("aviation.csv",header = TRUE,sep =",", dec=".")
names(tempav)
ggplot(tempav, aes(Publication.Year, fill = Region)) + 
  geom_bar() +
  xlab("Publication Year")
ggplot(tempav, aes(Publication.Year, Type, fill=Type))+
  geom_density_ridges() +
  labs(x ="Year of Publication", y = "GRI Standards", title = "Aviation") +
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")
#combined is the data file which is the combination of all the individual files
#or
#Df is the data file which is the combination of all the individual files
#tempdf is the data frame where we are reading the combined data file
#aviation is the df where we filtered the data according to the industry
#tempav is the df where we are reading the aviation file


# For a text working with only one text file
readLines("Airbus_2012.txt")
str(readLines("Airbus_2012.txt"))
airbus <- paste(readLines("Airbus_2012.txt"), collapse = " ")
nairbus <- gsub(pattern="\\W", replace=" ", airbus) # Removing Punctuations
nairbus
nairbus <- gsub(pattern = "\\d", replace = " ", nairbus) # Removing Digits
nairbus <- tolower(nairbus) # To lower case the letters
stopwords()
nairbus <- removeWords(nairbus, stopwords()) # Removing Stopwords
nairbus <- gsub(pattern = "\\b[A-z]\\b{1}", replace=" ", nairbus) #Removig 1 letter words
nairbus <- stripWhitespace(nairbus) # removing extraspaces
nairbus
#sentement Analysis
nairbus <- str_split(nairbus, pattern = "\\s+") # Divding the Strings
nairbus
str(nairbus)
finalairbus <- unlist(nairbus) # Converting list to a char
class(finalairbus)
finalairbus
## Preaparing postive words files
####postive <- scan('p.....txt',what='character',comment.char=";")
match(finalairbus,postive) #matching the postive words
sum(!is.na(match(finalairbus,postive)))
sum(!is.na(match(finalairbus,negative)))
score <- sum(!is.na(match(finalairbus,postive))) - sum(!is.na(match(finalairbus, negative)))
mean(score)
sd(score)
hist(score)

#########we need to have the scores for the multiple files so you
#### can perform mean,sd and sentement analysis
wordcloud(finalairbus)
wordcloud(finalairbus, min.freq = 40)
worldcloud(finalairbus, min.freq = 20, random.order = FALSE)
wordcloud(finalairbus, min.freq = 10, random.order = FALSE, scale =c(7,0.5), color = rainbow(7))
#Combining multiple files
#file.choose()
textfiles <- ("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files/Aviation copy")
setwd(textfiles)
allfiles <- list.files(path = textfiles, pattern = "*.txt")
#allfiles
class(allfiles)
allfiles <- paste(textfiles, "/", allfiles, sep="")
#allfiles
typeof(allfiles)
class(allfiles)
newpath <- ("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files")
setwd(newpath)
#datan <- scan(allfiles)
datan <- lapply(allfiles, FUN = readLines)
#datan
newdata <- lapply(datan, FUN = paste, collapse = " ")
#newdata
#class(newdata2)
#write.table(newdata2, file = "alldata.txt")
newdata2 <- gsub(pattern = "\\W", replace = " ", newdata)
newdata2 <- gsub(pattern = "\\d", replace= " ", newdata2)
newdata2 <- tolower(newdata2)
newdata2 <- removeWords(newdata2, stopwords("english"))
newdata2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace= " ", newdata2)
newdata2 <- stripWhitespace(newdata2)
write.table(newdata2, file = "afterclean.txt")
#a <- scan("afterclean.txt", what = "character")
#b <- str_split(a, pattern = "\\s+")
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
data(mat)
view(mat)
#######
dim(newdata3)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
#beta <- extracting the per-topic-per-word probabilities
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
data(AssociatedPress)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
  
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
tidy(tdm) %>%
  filter(document == 6) %>%
  arrange(desc(count))

#Sentiment Analysis
postive <- scan("postivewords.txt",what='character',comment.char=";")
negative <- scan("negativewords.txt",what='character',comment.char=";")
str(postive)

newdataforsem <- str_split(newdata2, pattern = "\\s+")
write.table(newdataforsem, file = "newdataforsem.txt")
sumofpos <- lapply(newdataforsem, function(x){sum(!is.na(match(x, postive)))})
#sumofpos
sumofneg <- lapply(newdataforsem, function(x){sum(!is.na(match(x, negative)))})
#sumofneg
total <- lapply(newdataforsem, function(x){sum(!is.na(match(x, postive))) - sum(!is.na(match(x,negative)))})
total
total <- unlist(total)
total
mean(total)
sd(total)
hist(total)

myDict <- dictionary(list(terror = c("terror*"),
                          economy = c("job*", "business*", "econom*")))
dict_tdm <- dfm_lookup(tdm, myDict, nomatch = "_unmatched") 
tail(dict_tdm)


set.seed(2)
# create a document variable indicating pre or post war 
docvars(tdm, "is_prewar") <- docvars(tdm, "Year") < 1945
# sample 40 documents for the training set and use remaining (18) for testing 
train_tdm <- dfm_sample(tdm, size = 40)
test_tdm <- tdm[setdiff(docnames(tdm), docnames(train_tdm)), ]
# fit a Naive Bayes multinomial model and use it to predict the test data 
nb_model <- textmodel_NB(train_tdm, y = docvars(train_tdm, "is_prewar")) 
pred_nb <- predict(nb_model, newdata = test_tdm)
# compare prediction (rows) and actual is_prewar value (columns) in a table 
table(prediction = pred_nb$nb.predicted, is_prewar = docvars(test_tdm, "is_prewar"))


texts = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
par_tdm <- dfm(texts, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
               par_tdm <- dfm_trim(par_tdm, min_count = 5) # remove rare terms
               par_tdm <- convert(par_tdm, to = "topicmodels") # convert to topicmodels format
               set.seed(1)
               lda_model <- topicmodels::LDA(par_tdm, method = "Gibbs", k = 5) terms(lda_model, 5)

### Done First part of Word Cloud
## LDA
#write.table(newdata, file = "beforeclean.txt")
#text <- scan("beforeclean.txt", what = "character")
#write.csv(b, "1afterclean.csv")

alldata <- scan("afterclean.txt", what = "character")
write.csv(alldata, "new2.csv")
data <- fread("new.csv")
data <- data %>% select(a,x)
data
####NEW METHOD
frame <- read.table("afterclean.csv")
frame2 <- gsub(pattern = "\\W", replace = " ", frame)
frame2 <- gsub(pattern = "\\d", replace= " ", frame2)
frame2 <- tolower(frame2)
frame2 <- removeWords(frame2, stopwords("english"))
frame2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace= " ", frame2)
frame2 <- stripWhitespace(frame2)

cleanCorpus <- function(frame){
  
  corpus.tmp <- tm_map(frame, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  v_stopwords <- c(stopwords("english"))
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
tokenizer  <- function(x){
  
  NGramTokenizer(x, Weka_control(min=2, max=2))
  
}
frequentBigrams <- function(text){
  
  s.cor <- VCorpus(VectorSource(text))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize=tokenizer))
  s.tdm <- removeSparseTerms(s.tdm, 0.999)
  m <- as.matrix(s.tdm)
  word_freqs <- sort(rowSums(m), decreasing=TRUE)
  dm <- data.frame(word=names(word_freqs), freq=word_freqs)
  return(dm)
  
}
length(frame$V1)
length(levels(frame$V1))
top.chars <- as.data.frame(sort(table(frame$V1), decreasing=TRUE))[1:20,]
ggplot(data=top.chars, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#56B4E9", colour="black") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="Character", y="Number of dialogues")

#data$x <- sub("RT.*:", "", data$x)
#data$x <- sub("@.* ", "", data$x)

#text_cleaning_tokens <- data %>% 
#  tidytext::unnest_tokens(a, x)
#text_cleaning_tokens$x<- gsub('[[:digit:]]+', '', text_cleaning_tokens$x)
#text_cleaning_tokens$x <- gsub('[[:punct:]]+', '', text_cleaning_tokens$x)
#text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(x) == 1))%>% 
#  anti_join(stop_words)
#tokens <- text_cleaning_tokens %>% filter(!(x==""))
#tokens <- tokens %>% mutate(ind = row_number())
#tokens <- tokens %>% group_by(a) %>% mutate(ind = row_number()) %>%
#  tidyr::spread(key = ind, value = x)
#tokens [is.na(tokens)] <- ""
#tokens <- tidyr::unite(tokens, text,-id,sep =" " )
#tokens$a <- trimws(tokens$a)
#dtm <- CreateDtm(tokens$x, 
#                 doc_names = tokens$a, 
#                 ngram_window = c(1, 2))
#ggplot(newdata2, aes(Publication.Year, fill = Sector)) + 
#  geom_bar() +
#  xlab("Publication Year")
#?text_tokens
#text_ntoken(newdata2)
#text <- c(newdata2)
#write.table(text, file = "readwrite.txt")
#text_df <- tibble(line= 1:n,text = text)
#text_df %>% unnest_tokens(word, newdata2)
#?dplyr

#allfiles2 <- list.files(textfiles, pattern = "*.txt")
#mydata <- lapply(allfiles2, readLines(), sep=" ", header=T, row.names=NULL)
#class(mydata)
#write.file<-""
#alldata <- dir(textfiles, pattern ="*.txt")
#for(i in 1:length(alldata)){
#  file <- readLines(alldata[i])
#  write.file <- rbind(write.file, file)
#}
#corpus <- paste(write.file, collapse = "")
#write.table(write.file, file = "alldata.txt")
#corpus <- paste(corpus, collapse = "")
#write.table(corpus, file = "alldata.txt", sep = "")
#OR

##### cat *.txt > alldata.txt

#mypath <- ("~/Documents/Master Program/Data Analysis & Visualization/Home Assignment/Files")
#setwd(mypath)
#alldata <- scan("all data.txt", what = "character")
#class(alldata)
#alldata <- paste(readLines("all data.txt"), collapse = " ")
#alldata2 <- paste(alldata, collapse = " ")
#write.table(alldata, file = "New data.txt")
#alldata2 <- gsub(pattern = "\\W", replace = " ", alldata)
#alldata2 <- gsub(pattern = "\\d", replace= " ", alldata2)
#alldata2 <- tolower(alldata2)
#alldata2 <- removeWords(alldata2, stopwords("english"))
#alldata2 <- gsub(pattern = "\\b[A-z]\\b{1}", replace= " ", alldata2)
#alldata2 <- stripWhitespace(alldata2)
#write.table(alldata2, file = "New data.txt")

#alldata3 <- paste(alldata2, collapse = " ")
#alldata3 <- stripWhitespace(alldata3)
#alldata4 <- strsplit(alldata3, " ")[[1]]
#write.table(alldata4, file = "New data.txt")
#class(alldata4)
#wordcloud(alldata2, min.freq = 2000, random.order = FALSE, scale = c(3,0.4))
#wordcloud(alldata3, min.freq = 2000, random.order = FALSE)
#wordcloud(alldata3, min.freq = 1000, random.order = FALSE, scale =c(3,0.4), color = rainbow(5))
#wordcloud(alldata2, min.freq = 2000, random.order = FALSE, scale =c(3,0.4), color = rainbow(5))
#comparison.cloud(alldata2)
#alldata3 <- Corpus(VectorSource(alldata2))
#alldata3
#td <- TermDocumentMatrix(alldata3)
#td
#mats <- as.matrix(td)
#comparison.cloud(mats)

#Sentiment Analysis
#postiveall <- scan("postivewords.txt",what='character',comment.char=";")
#negativeall <- scan("negativewords.txt",what='character',comment.char=";")
#str(postiveall)

#newdatasem <- str_split(alldata2, pattern = "\\s+")
#sumpos <- lapply(newdatasem, function(x){sum(!is.na(match(x, postiveall)))})
#sumpos
#sumneg <- lapply(newdatasem, function(x){sum(!is.na(match(x, negativeall)))})
#sumofneg
#newtotal <- lapply(newdatasem, function(x){sum(!is.na(match(x, postiveall))) - sum(!is.na(match(x,negativeall)))})
#newtotal
#newtotal <- unlist(newtotal)
#newtotal
#mean(newtotal)
#sd(newtotal)
#hist(newtotal)
