library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
library(stringr)
library(ggplot2)
library(tidytext)
library(gutenbergr)

book_sentiment <- function(id, seg_length) {
  #id is the gutenberg_id of the desired document
  #seg_length is the number of lines that are analyzed at a time
  #This only works with text documents
  #This returns a tibble which associates each segment of the book with
  #a score based on the sentiment values of the words in the segment
  #This uses the Bing lexicon for scoring
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
  #This creates a visual of the book_sentiment function
  book_sentiment(id, seg_length) %>%
    ggplot(aes(index, sentiment)) +
    geom_col()
}

book_sentiment_absolute <- function(id, seg_length) {
  #This is similar to the book_sentiment function, but the score
  #is, instead of the number of positives minus the number of negatives, the
  #sum of the positive and negative counts
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
  #This produces a visual of the book_sentiment_absolute
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

book_sentiment_ratio <- function(id, seg_length) {
  temp_book <- gutenberg_download(id) %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(word, text) %>%
    left_join(get_sentiments("bing")) %>%
    count(index = linenumber %/% seg_length, sentiment)
  return(
    sum((filter(temp_book, (sentiment == "positive")|(sentiment == "negative")))[, 3])
    / sum(temp_book[, 3])
  )
}
