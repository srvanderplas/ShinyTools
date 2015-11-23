
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tm)
library(stringr)
library(magrittr)
library(dplyr)
library(wordcloud)

stemCompletion_mod <- function(x, dict, type = "shortest") {
  x %>% as.character() %>%
    strsplit(split = " ") %>%
    paste() %>%
    stemCompletion(dictionary = dict, type = type) %>%
    paste(sep = "", collapse = " ") %>%
    stripWhitespace() %>%
    PlainTextDocument
}

MakeWordFreq <- function(wordlist, stem = T, rm.stopwords = T,
                         stopword.list = NULL){
  # Function to make a word frequency table
  wordlist %<>%
    paste(., collapse = " ") %>%
    VectorSource() %>%
    Corpus() %>%
    tm_map(., removeNumbers) %>%
    tm_map(., removePunctuation)

  if (rm.stopwords) {
    wordlist %<>%
    tm_map(., removeWords, stopwords("en"))
  }

  wordlist %<>%
    TermDocumentMatrix %>%
    as.matrix() %>%
    as.data.frame() %>%
    set_names("freq")

  if (stem) {
    wordlist %<>%
      mutate(original = row.names(.), stems = stemDocument(row.names(.)))
  } else {
    wordlist %<>%
      mutate(original = row.names(.), stems = row.names(.))
  }

  wordlist %<>%
    group_by(stems) %>%
    summarize(freq = sum(freq), original = original[which.min(nchar(original))]) %>%
    ungroup() %>%
    select(-stems) %>%
    set_names(c("freq", "word")) %>%
    select(word, freq)

  if (rm.stopwords) {
    wordlist %<>%
      subset(!word %in% c(stopwords("en"), stopword.list))
  }

  wordlist %<>%
    arrange(desc(freq))
}

MakeWordcloud <- function(x, color.set = brewer.pal(6, "Dark2"), max.words = 50,  ...){
  wordcloud(x$word, x$freq, colors = color.set, random.color = T, max.words = max.words, random.order = F, rot.per = .3, ...)
}


shinyServer(function(input, output) {

  output$wordcloud <- renderPlot({
    validate(
      need(input$words != "",
           "Please paste text into the box labeled \"Wordcloud text\"")
    )

    if (input$stopwords) {
      stopword.list <- str_replace(input$ignore, "Remove common English words", "")
    } else {
      stopword.list <- NULL
    }

    color.pal <- brewer.pal(6, input$palette)
    if (input$palette == "Greys") {
      color.pal <- brewer.pal(8, input$palette)[-c(1:2)]
    }

    data <- str_split(input$words, "\\n") %>%
      unlist() %>%
      as.character()

    data <- data %>%
      MakeWordFreq(stopword.list = stopword.list,
                   stem = input$stem,
                   rm.stopwords = input$stopwords) %>%
      MakeWordcloud(color.set = color.pal, max.words = input$nWords)

  })

})
