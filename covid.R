library(tm)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tidyr)
library(stringr)
library(gutenbergr)
library(ggplot2)
library(wordcloud)
library(igraph)

covid <- read_csv('~/Downloads/covid.csv')

##EDA basics for the base csv
head(covid)
structure(covid)
dim(covid)
summary(covid)

## make the tweet text into a corpus, for word cloud and viewing
covid_text <- paste(covid$tweet, collapse = " ")
corpus <- Corpus(VectorSource(covid_text))

## clean corpus
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

##Document term matrix for examining frequency
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
head(word_freq)

wordcloud(names(word_freq), word_freq, max.words = 100, colors = brewer.pal(8, "Dark2"))

cleaned_text <- sapply(corpus, as.character)
write_csv(data.frame(text = cleaned_text), "cleaned_corpus_text.csv")

## 2. Export word frequencies to CSV
word_freq_df <- data.frame(word = names(word_freq), frequency = word_freq)
write_csv(word_freq_df, "word_frequencies.csv")

## 3. Export TDM as CSV (sparse matrix)
tdm_df <- as.data.frame(as.matrix(tdm))
write_csv(tdm_df, "term_document_matrix.csv")
