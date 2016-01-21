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
library(xtable)
library(RColorBrewer)
library(nppd)
# library(wordcloud)

# source("./Code/WordcloudFunctions.R")
withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, str_replace(w$message, "(.*) could not be .*", "\\1"))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

# --- CNS specific functions ---------------------------------------------------

cns_proc_regex <- "\\b(?!CNS-\\d{4}-\\d{5})(?!1-\\d{7})(?!1-[DHNPR])(?!9-HV)(?!9-SS)(?!201[123456])(?!0\\.\\d{1,3}[E]\\d{1,3})(?!\\d\\.\\d{1,3}-\\d\\.\\d{1,3})(?!\\d{3}-\\d{3,4})(?!\\d{1,2}-\\d{1,2}-\\d{2,4})(?!\\d{2}-\\d{2})(?!SS-)(?!EE-)(\\d{1}|1\\d{1}|ENN?)[\\._-][A-Z\\d\\._-]{1,}\\b"

cfr_regex <- "\\b(\\d{1,2}CFR[[:alnum:]\\.]*(?:[[:punct:]]{1}[[:alnum:]]{1,}[[:punct:]]{1})?)"

cns_cr_regex <- "\\b((?:CNS|CR|CR[ -]CNS)[ -]\\d{4}[ -]\\d{4,5})\\b"

cns_wo_regex <- "\\b((?:WO|wo|W\\.O\\.|work order|Work Order)[ #-]*\\d{6,8})\\b"

cns.stopwords <- c("per", "will", "method", "of", "discovery", "condition", "description", "requirement", "require", "not", "met", "action", "attached", "document", "cns", "attachment", "also", "however", "says")

# Fixes raw input text before processing
cns_fix_input_text <- function(x) {
  x %>%
    str_replace_all("\\n|\\r|\\\"|/", " ") %>%
    str_replace_all("\\(|\\)", " ") %>%
    str_replace_all(cns_proc_regex, " ") %>%
    str_replace_all(cfr_regex, " ") %>%
    str_replace_all(cns_cr_regex, " ") %>%
    str_replace_all(cns_wo_regex, " ") %>%
    str_to_lower() %>%
    str_replace_all("ann -", " ann ") %>%
    str_replace_all("[\\b ]sh[\\b ]", " sheet ") %>%
    str_replace_all("[\\b ]work order[\\b#\\. ]", " workorder ") %>%
    str_replace_all("[\\b ]wo[\\b #-]", " workorder ") %>%
    str_replace_all("[\\b ]dg[\\b ]", " diesel generator ") %>%
    str_replace_all("[\\b ]non[- ](\\w)", " non\\1") %>%
    str_replace_all("[\\b ]sap[\\b ]", "sssaaappp") %>%
    str_replace_all("[\\b ]performance deficiency[\\b ]", " performancedeficiency ") %>%
    str_replace_all("[\\b ]operator[\\b ]", "operatorperson") %>%
    str_replace_all("[\\b ]ops[\\b ]", " operationsdept ") %>%
    str_replace_all("ca&a", "correctiveactionassess") %>%
    str_replace_all("ep&c", "eeepppccc") %>%
    str_replace_all("[\\b ]pi | performance indicators?[\\b ]", " performanceindicator ") %>%
    str_replace_all("[\\b ]clearance order[\\b ]", " clearanceorder ") %>%
    str_replace_all("[\\b ]gnf[\\b ]", " globalnuclearfuels ") %>%
    str_replace_all("[\\b ]g[\\. ]?e[\\. ]?(h?)[\\b ]", " generalelectric\\1 ") %>%
    str_replace_all("[ \\b]cat[ \\\"\\(-]{1,}([abcdwq])(?:[\\\"\\)])?(?:[[:punct:]]([HL])[[:punct:]])?[\\b \\.]", " classificationcategory\\1\\2 ") %>%
    str_replace_all("[ \\b]classificationcategory([abcdwq])[ -]([trendcboa]{5})[ \\b]",
                    " classificationcategory\\1\\2 ") %>%
    str_replace_all("\\\\", " ") %>%
    str_replace_all("\\s{1,}", " ")
}

# Fixes text after processing so that it is displayed correctly
cns_fix_word_freq <- function(x) {
  str_replace_all(
    x,
    c("performanceindicator" = "performance indicator",
      "performancedeficiency" = "performance deficiency",
      "clearanceorder" = "clearance order",
      "eeepppccc" = "EP&C",
      "sssaaappp" = "SAP",
      "correctiveactionassess" = "CA&A",
      "generalelectrich" = "GEH",
      "generalelectric" = "GE",
      "globalnuclearfuels" = "GNF",
      "classificationcategory" = "CAT ",
      "operatorperson" = "operator",
      "operationsdept" = "Operations",
      "^cfr$" = "CFR",
      "^ero$" = "ERO",
      "^annann$" = "ANN-ANN",
      "^eof$" = "EOF",
      "^rhr$" = "RHR",
      "^jic$" = "JIC",
      "^qc$" = "QC",
      "^epip$" = "EPIP",
      "^mro$" = "MRO",
      "^nis$" = "NIS",
      "^tsc$" = "TSC",
      "^tpp$" = "TPP",
      "^ann$" = "ANN",
      "^cda$" = "CDA",
      "^en$" = "EN",
      "^dg$" = "DG",
      "^ep$" = "EP",
      "^re$" = "RE",
      "^ro$" = "RO",
      "^rp$" = "RP",
      "^cr$" = "CR",
      "^qa$" = "QA",
      "^cas$" = "CAS",
      "^cns$" = "CNS",
      "^erp$" = "ERP",
      "alara" = "ALARA",
      "^pmis$" = "PMIS",
      "^cooper$" = "Cooper",
      "^nrc$" = "NRC", "^wano$" = "WANO", "^inpo$" = "INPO")
  )
}

collapse_values <- function(x){
  lapply(x, function(i) paste(i, collapse = ", "))
}

# ------------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observe({
    if ("words" %in% names(input))
      session$sendCustomMessage(
        type = "shinySetValues",
        message = ""
      )
  })

  formatStopwords <- reactive({
    if (input$stopwords) {
      input$ignore %>%
        str_to_lower() %>%
        str_replace_all("[\\n,]", " ") %>%
        str_split("\\W|\\s") %>%
        unlist()
    } else {
      NULL
    }
  })

  stopwordList <- reactive({
    stopword.list <- formatStopwords()
    if (input$CNSstopwords) {
      stopword.list <- c(stopword.list, cns.stopwords)
    }
    stopword.list
  })

  formatWords <- reactive({
    if (nchar(input$words) > 0) {
      if (input$fixCNS) {
        input$words %>%
          str_split("(\\n{1,})") %>%
          unlist() %>%
          as.character() %>%
          cns_fix_input_text() %>%
          MakeWordFreq(stopword.list = stopwordList(),
                       stem = input$stem, rm.stopwords = input$stopwords) %>%
          mutate(word = cns_fix_word_freq(word)) %>%
          filter(nchar(word) > input$wordSize)
      } else {
        input$words %>%
          str_split("(\\n{1,})") %>%
          unlist() %>%
          as.character() %>%
          str_to_lower() %>%
          MakeWordFreq(stopword.list = stopwordList(),
                       stem = input$stem, rm.stopwords = input$stopwords) %>%
          filter(nchar(word) > input$wordSize)
      }
    } else {
      data.frame(word = rep("", 0), freq = rep(0, 0), stringsAsFactors = F) %>% as_data_frame()
    }
  })


  wordPlot <- reactive({
    color.pal <- brewer.pal(6, input$palette)
    if (input$palette == "Greys") {
      color.pal <- rev(brewer.pal(9, input$palette)[-c(1:3)])
    }

    df <- formatWords()

    withWarnings(
      MakeWordcloud(x = df, color.set = color.pal, max.words = input$nWords,
                    min.freq = input$wordFreq)
    )
  })

  warning.msgs <- reactiveValues(list = NULL)

  output$wordcloud <- renderPlot({
    validate(
      need(input$words != "",
           "Please paste text into the box labeled \"Wordcloud text\".")
    )

    wp <- wordPlot()
    print(wp$value)
    warning.msgs$list <- wp$warnings
    # message(paste(names(warnings()), collapse = "\n"))
  })

  output$testtext <- renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))

  observe({
    tmp <- formatWords()
    sw <- formatStopwords()

    if (nrow(tmp) > 0) {
      cat(
        paste0(
          paste(
            Sys.time(),
            input$ipid,
            input$fingerprint,
            sum(tmp$freq),
            nrow(tmp),
            length(sw),
            input$stem,
            input$stopwords,
            input$CNSstopwords,
            input$fixCNS,
            input$nWords,
            input$wordSize,
            input$wordFreq,
            input$palette,
            sep = ", "
          ),
          "\n"),
        file = "userInfo.txt",
        append = T
      )
    }
  })

  output$warningMessages <- renderUI({
    wl <- warning.msgs$list %>% unlist() %>% as.character()
    if (length(wl) > 0) {
      tt <- data.frame(matrix(wl, ncol = min(length(wl), 4)), stringsAsFactors = F) %>%
        xtable() %>% print.xtable(include.rownames = F, include.colnames = F, type = 'html') %>%
        HTML()

      tagList(
        h3("Warning: The following words would not fit within the boundaries"),
        tt
      )
    }
  })
})
