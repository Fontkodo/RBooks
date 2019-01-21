library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(stringr)
library(ggplot2)
library(tidytext)
library(gutenbergr)

book_sentiment <- function(id, seg_length) {
  return(
    gutenberg_download(id) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing")) %>%
      count(index = linenumber %/% seg_length, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    )
}

book_sentiment_graph <- function(id, seg_length) {
  book_sentiment(id, seg_length) %>%
    ggplot(aes(index, sentiment)) +
    geom_col()
}

book_sentiment_absolute <- function(id, seg_length) {
  return(
    gutenberg_download(id) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing")) %>%
      count(index = linenumber %/% seg_length, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive + negative)
  )
}

book_sentiment_absolute_graph <- function(id, seg_length) {
  book_sentiment_absolute(id, seg_length) %>%
    ggplot(aes(index, sentiment)) +
    geom_col()
}

book_sentiment_intensity <- function(id, seg_length) {
  return(
    gutenberg_download(id) %>%
      mutate(linenumber = row_number()) %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("afinn")) %>%
      count(index = linenumber %/% seg_length, score) %>%
      spread(score, n, fill = 0) %>%
      mutate(intensity = ())
  )
}
