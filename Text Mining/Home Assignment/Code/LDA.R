install.packages("Matrix")
library(quanteda)
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
library(textstem)
library(Matrix)

list_data <- list()
for (i in list.files(path = "Aviation copy")) {
  data <- paste(readLines(paste('Aviation copy/', i, sep='')), collapse = ' ')
  data <- gsub(pattern="\\W", replace=" ", data)
  data <- gsub(pattern = "\\d", replace = " ", data)
  data <- tolower(data)
  data <- removeWords(data, stopwords())
  data <- gsub(pattern = "\\b[A-z]\\b{1}", replace=" ", data)
  data <- stripWhitespace(data)
  data <- str_split(data, pattern = "\\s+")
  data <- lemmatize_words(unlist(data))
  data <- paste(data, collapse=' ')
  list_data[[i]] <- data
}

corpus_data = Corpus(VectorSource(list_data))
tdm_data = DocumentTermMatrix(corpus_data)
tdm_data

terms <- Terms(tdm_data)
head(terms)
ap_td <- tidy(tdm_data)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 1400) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()
ap_td %>%
  cast_dtm(document, term, count)
ap_td %>%
  cast_dfm(document, term, count)

# cast into a Matrix object
m <- ap_td %>%
  cast_sparse(document, term, count)
m
class(m)

dim(m)


##LDA
ap_lda <- LDA(tdm_data, k = 4, control = list(seed = 1234))
ap_lda
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

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

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log ratio of beta") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(tdm_data) %>%
  arrange(desc(count))
tidy(tdm_data) %>%
  filter(document == 6) %>%
  arrange(desc(count))
