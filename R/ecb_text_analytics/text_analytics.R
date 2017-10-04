library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringr)


#' Remove standard stopwords or custom stopwords from a unigram or bigram
#'
#' @param ngram a dataframe which is an ngram
#' @param custom_stop_words 
#'
#' @return unibigram a dataframe with stop words removed
#' @export
#'
#' @examples
#' #'#' library(gutenbergr)
#'#' kjv <- gutenberg_download(10)
#'#' kjv_bigrams <- kjv %>% make_bigrams(remove_stop_words = FALSE) %>% remove_stopwords
remove_stopwords <- function(ngram, custom_stop_words = NULL) {
  if (is.null(custom_stop_words)) {
    data(stop_words)
    custom_stop_words <- stop_words
  }
  
  word_cols <- paste("word", c("", as.character(1:5)), sep="")
  if (sum(word_cols %in% names(ngram)) > 0) {
    for (i in word_cols[word_cols %in% names(ngram)]) {
      ngram <- ngram %>% filter_(paste("!", i, "%in% custom_stop_words$word"))
    }
    
    ngram
  } else {
    stop("Error, please ensure you input an ngram with col names 'word', 'word1', 'word2' up to 'word5' etc")
  }
}

#' Generates unigram from text input
#'
#' @param dataset dataframe with column name "text" 
#' @param custom_stop_words 
#' @param remove_stop_words 
#'
#' @return unigram dataframe
#' @export
#'
#' @examples
#' #'#' library(gutenbergr)
#'#' kjv <- gutenberg_download(10)
#'#' kjv_bigrams <- kjv %>% make_unigrams
make_unigrams <- function(dataset, custom_stop_words = NULL, remove_stop_words = TRUE) {
  temp_gram <- dataset %>%
    unnest_tokens(word, text)
  
  if (remove_stop_words) {
    temp_gram <- remove_stopwords(temp_gram, custom_stop_words)
  }
  
  temp_gram
}

#' Generate bigrams from text input
#'
#' @param dataset dataframe with column name "text" 
#' @param custom_stop_words 
#' @param remove_stop_words
#'
#' @return bigram dataframe
#' @export
#'
#' @examples
#'#' library(gutenbergr)
#'#' kjv <- gutenberg_download(10)
#'#' kjv_bigrams <- kjv %>% make_bigrams
make_ngrams <- function(dataset, n = 1, custom_stop_words = NULL, remove_stop_words = TRUE) {
  temp_gram <- dataset %>%
    unnest_tokens(word, text, token = "ngrams", n = n)
  
  # Split into individual columns if ngrams
  if (n > 1) {
    word_names = paste("word", 1:n, sep="") 
    temp_gram <- separate(temp_gram, word, word_names, sep = " ")
  }
  
  # Remove stop words if necessary
  if (remove_stop_words) 
    temp_gram <- remove_stopwords(temp_gram, custom_stop_words)
  
  temp_gram
}

#' Calculate count of words from ngrams
#'
#' @param ngrams dataframe with columns "wprd", "word1" and "word2" etc
#'
#' @return tidytext dataframe of ngram counts
#' @export
#'
#' @examples
#'#' library(gutenbergr)
#'#' kjv <- gutenberg_download(10)
#'#' kjv_bigrams <- kjv %>% make_bigrams %>% count_bigrams
count_ngrams <- function(ngrams, reorder = TRUE) {
  word_found <- names(ngrams) %>%
    .[grepl("^word[0-9]{0,1}", .)]
  
  temp_grams <- ngrams %>%
    count_(paste(word_found, sep=", "), sort = TRUE) 
  
  if (reorder) {
    temp_grams[[word_found[1]]] <- reorder(temp_grams[[word_found[1]]], temp_grams$n)
  }
    
}

#' Visualize bigrams as a directed graph
#'
#' @param bigrams dataframe with columns "word1" and "word2"
#' @param min.n minimum count of word to be displayed
#' @param ignore.digits to ignore occurrence of digits in the text
#'
#' @return NULL
#' @export
#'
#' @examples
#'#' library(gutenbergr)
#'#' kjv <- gutenberg_download(10)
#'#' kjv_bigrams <- count_bigrams(kjv)
#'#' visualize_bigrams(kjv_bigrams, n = 50, ignore.digits = TRUE)
visualize_bigrams <- function(bigrams_count, min.n = 40, ignore.digits = TRUE, seed = 1) {
  set.seed(seed)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  if (ignore.digits) {
    bigrams_count <- bigrams_count %>%
      filter(!str_detect(word1, "\\d"),
             !str_detect(word2, "\\d"))
  }
  
  bigrams_count %>%
    filter(n > min.n) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
