library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringr)

#' Calculate the sentiment score
#'
#' @param ngrams 
#'
#' @return
#' @export
#'
#' @examples
calculate_sentiment_score <- function(ngrams) {
  others <- ngrams %>%
    filter(method != "afinn") %>%
    
  
  if ("afinn" %in% ngrams$method) {
    afinn <- ngrams %>%
      filter(method = "afinn") %>%
      summarise(sentiment = sum(score))
  }
  
  
}
#' Compare words sentiment in wide form
#'
#' @param ngrams long form dataframe with columns "method" and "sentiment"
#'
#' @return
#' @export
#'
#' @examples
compare_words_sentiment <- function(ngrams) {
  temp_grams <- ngrams %>%
    group_by(word, method) %>%
    summarise(sentiment = paste0(sentiment, collapse="|")) %>%
    distinct(word, sentiment, method) %>% 
    spread(method, sentiment, convert = TRUE)
  
  left_join(ngrams, temp_grams, by = "word") %>%
    select(-sentiment, -method)
}

#' To add sentiment/s to dataframe on matching with "word" column
#'
#' @param ngram dataframe of ngram with column "word"
#' @param sentiment_libs library of sentiments "afinn", "bing", "nrc" or "loughran"
#' @param name_using_method use the input methods as name of columns, always truE if multiple sentiments are used
#'
#' @return dataframe with added column for sentiment
#' @export
#'
#' @examples
add_sentiments <- function(ngrams, sentiment_libs = c("loughran", "nrc", "bing")) {
  bind_rows(lapply(sentiment_libs, function(x) {single_add_sentiment(ngrams, x)}))
}

#' Helper method to add sentiment to dataframe on matching with "word" column, avoid using directly, use add_sentiments() instead
#'
#' @param ngram dataframe of ngram with column "word"
#' @param sentiment_lib library of sentiments "afinn", "bing", "nrc" or "loughran"
#' @param name_using_method use the input methods as name of columns

#'
#' @return dataframe with added columns
#' @export
#'
#' @examples
single_add_sentiment <- function(ngrams, sentiment_lib) {
  temp_grams <- ngrams %>%
    inner_join(get_sentiments(sentiment_lib), by = "word") %>%
    mutate(method = sentiment_lib)
  
  if (sentiment_lib == "afinn") {
    temp_grams <- temp_grams %>%
      mutate(sentiment = as.character(score)) %>%
      select(-score)
  }

  if ("method" %in% names(ngrams) && ("sentiment" %in% names(ngrams) || "score" %in% names(ngrams))) {
    bind_rows(ngrams, temp_grams)
  } else {
    temp_grams
  }
}


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
make_ngrams <- function(dataset, n = 1, custom_stop_words = NULL, remove_stop_words = TRUE, summarise_count = TRUE) {
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
  
  if (summarise_count) 
    temp_gram <- temp_gram %>%
      group_by_(.dots = names(.)) %>%
      summarise(n = n()) %>%
      ungroup %>%
      arrange(desc(n))
  
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
#'#' kjv_bigrams <- kjv %>% make_ngrams(2) %>% count_ngrams
count_ngrams <- function(ngrams, reorder = TRUE) {
  word_found <- names(ngrams) %>%
    .[grepl("^word[0-9]{0,1}", .)]
  
  if ("n" %in% names(ngrams)) {
    ngrams %>% 
      group_by(.dots = word_found, add = TRUE) %>%
      summarise(n = sum(n)) %>%
      ungroup() %>%
      arrange(desc(n))
  }
  else {
    ngrams %>%
      count_(paste(word_found, sep=", ")) %>%
      arrange(desc(n))
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
