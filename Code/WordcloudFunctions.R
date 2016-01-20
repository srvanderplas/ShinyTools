library(wordcloud)

#' Create a word frequency table from a list of words or phrases
#' @param wordlist vector of strings
#' @param stem T/F - stem?
#' @param rm.stopwords T/F - remove english stopwords?
#' @param stopword.list List of stopwords to remove in addition to english stopwords (only matters if rm.stopwords is T)
#' @return word frequency list
#' @export
MakeWordFreq <- function(wordlist, stem = T, rm.stopwords = T,
                         stopword.list = NULL){

  # Function to make a word frequency table
  wordlist %<>%
    str_to_lower() %>%
    str_replace_all("'", "") %>%
    str_replace_all("\\n|\\r|\\\"|/", " ") %>%
    str_replace_all("[\\W“”≤]", " ") %>%
    iconv(to = "ASCII", sub = " ") %>%
    str_replace_all("[\\d]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[^[A-z] ]", " ") %>%
    str_replace_all("[\\s]{1,}", " ") %>%
    paste(., collapse = " ", sep = " ") %>%
    str_split(boundary("word")) %>%
    unlist() %>%
    str_trim()

  if (rm.stopwords) {
    wordlist <- wordlist[!wordlist %in% c(tm::stopwords("en"), stopword.list)]
  } else if (length(stopword.list) > 0) {
    wordlist <- wordlist[!wordlist %in% stopword.list]
  }

  wordlist %<>%
    table() %>%
    as.data.frame(stringsAsFactors = F) %>%
    magrittr::set_names(c("word", "freq"))

  if (stem) {
    wordlist %<>%
      dplyr::mutate(stems = tm::stemDocument(word))
  } else {
    wordlist %<>%
      dplyr::mutate(stems = word)
  }

  wordlist %<>%
    group_by(stems) %>%
    summarize(word = word[which.max(freq)], freq = sum(freq)) %>%
    ungroup() %>%
    select(-stems)

  if (rm.stopwords) {
    wordlist %<>%
      subset(!word %in% c(tm::stopwords("en"), stopword.list))
  } else if (length(stopword.list) > 0) {
    wordlist <- wordlist[!wordlist %in% stopword.list]
  }

  wordlist %<>%
    dplyr::arrange(desc(freq))
}

#' Function to make a wordcloud
#' @param x word frequency data frame (from MakeWordFreq), with columns "word" and "freq"
#' @param color.set vector of colors
#' @param max.words maximum words
#' @param rot.per percentage of words to rotate
#' @param random.order plot words in random order?
#' @param random.color color words randomly?
#' @param ... arguments to wordcloud()
#' @return wordcloud
#' @export
#' @examples
#' readLines("./data/compileText.txt") %>% str_replace_all("[[^[A-z] _]\\\\`]", " ") %>% str_split(" ") %>% unlist %>%  str_trim() %>% table() %>% as.data.frame(stringsAsFactors = F) %>% set_names(c("word", "freq")) %>% filter(nchar(word) > 0) -> tmp
#' tmp %>% MakeWordcloud()
MakeWordcloud <- function(x, color.set = RColorBrewer::brewer.pal(6, "Dark2"), colors = color.set, max.words = 50, rot.per = .3, random.order = F, random.color = T, ...){
  stopifnot(sum(c("word", "freq") %in% names(x)) == 2)
  args <- list(...)
  if ("word" %in% names(x)) {
    args$words <- x$word
  }

  if ("freq" %in% names(x)) {
    args$freq <- x$freq
  }

  args$colors <- colors
  args$random.color <- random.color
  args$max.words <- max.words
  args$random.order <- random.order
  args$rot.per <- rot.per

  do.call(wordcloud, args)
}
