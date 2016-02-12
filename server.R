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
library(gridGraphics)
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
  rep.list <- c(
    "performanceindicator" = "performance indicator",
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
    "^aac$" = "AAC",
    "^afi$" = "AFI",
    "^alara$" = "ALARA",
    "^ann$" = "ANN",
    "^annann$" = "ANN-ANN",
    "^atws" =  "ATWS",
    "^cas$" = "CAS",
    "^cda$" = "CDA",
    "^cfr$" = "CFR",
    "^cns$" = "CNS",
    "^cooper$" = "Cooper",
    "^cr$" = "CR",
    "^crd$" = "CRD",
    "^crs$" = "CRs",
    "^dg$" = "DG",
    "^dpis$" = "DPIS",
    "^edg$" = "EDG",
    "^egm" = "EGM",
    "^en$" = "EN",
    "^eof$" = "EOF",
    "^eop$" = "EOP",
    "^ep$" = "EP",
    "^epip$" = "EPIP",
    "^erat$" = "ERAT",
    "^ero$" = "ERO",
    "^erp$" = "ERP",
    "^hpci$" = "HPCI",
    "^hra$" = "HRA",
    "^ilt$" = "ILT",
    "^inpo$" = "INPO",
    "^jic$" = "JIC",
    "^lco" = "LCO",
    "^ler$" = "LER",
    "^lhra$" = "LHRA",
    "^met$" = "MET",
    "^mpff$" = "MPFF",
    "^mro$" = "MRO",
    "^msiv$" = "MSIV",
    "^mux$" = "MUX",
    "^nis$" = "NIS",
    "^nrc$" = "NRC",
    "^opdrvs$" = "OPDRVS",
    "^pcm$" = "PCM",
    "^pmis$" = "PMIS",
    "^qa$" = "QA",
    "^qc$" = "QC",
    "^rca$" = "RCA",
    "^rcs$" = "RCS",
    "^re$" = "RE",
    "^rhr$" = "RHR",
    "^ro$" = "RO",
    "^rp$" = "RP",
    "^rps$" = "RPS",
    "^rwp$" = "RWP",
    "^sfsp$" = "SFSP",
    "^ssst$" = "SSST",
    "^svc$" = "SVC",
    "^tpp$" = "TPP",
    "^tsc$" = "TSC",
    "^wano$" = "WANO"
  )

  if (is.vector(x)) {
    str_replace_all(x, rep.list)
  } else if (ncol(x) > 1) {
    rownames(x) <- str_replace_all(rownames(x), rep.list)
  } else {
    stop("Input is of an unexpected form. Cannot fix word freq.")
  }
}

# Filters words which are not at least nch long out
filter_word_size <- function(x, nch = 2) {
  if (is.data.frame(x)) {
    filter(x, nchar(word) > nch)
  } else if (is.matrix(x)) {
    x[nchar(rownames(x)) > nch, ]
  } else {
    stop("Unexpected input. Could not filter words by length.")
  }
}

# Scales count by log likelihood of frequency difference
log_like_scale_count <- function(tab){
  t2 <- tab
  c.d <- sum(colSums(tab))
  c <- sum(tab[,1])
  d <- sum(tab[,2])
  a.b <- rowSums(tab)
  t2[,1] <- 2*tab[,1]*log(tab[,1]*(c.d)/(c*a.b) + 1)
  t2[,2] <- 2*tab[,2]*log(tab[,2]*(c.d)/(d*a.b) + 1)
  t2
}

collapse_values <- function(x){
  lapply(x, function(i) paste(i, collapse = ", "))
}

# ------------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observe({
    if ("words" %in% names(input) | "words1" %in% names(input) | "words2" %in% names(input))
      session$sendCustomMessage(
        type = "shinySetValues",
        message = ""
      )
  })

  observe({
    if (input$wcType == 2) {
      updateSelectInput(session, "palette2", "Color Options",
                        choices =  c("Black" = "Black",
                                     "Dark" = "Dark2", "Paired" = "Paired",
                                     "Rainbow" = "Set1", "Grey" = "Greys"))
    } else {
      updateSelectInput(session, "palette2", "Color Options",
                        choices =  c("Dark" = "Dark2", "Paired" = "Paired",
                                     "Rainbow" = "Set1", "Grey" = "Greys"))
    }
  })

  formatStopwords <- function(ignore, str){
    if (str) {
      ignore %>%
        str_to_lower() %>%
        str_replace_all(pattern = "[\\n,]", replacement = " ") %>%
        str_split("\\W|\\s") %>%
        unlist()
    } else {
      NULL
    }
  }

  stopwordList <- reactive({
    stopword.list <- formatStopwords(input$ignore, input$stopwords)
    if (input$CNSstopwords) {
      stopword.list <- c(stopword.list, cns.stopwords)
    }
    stopword.list
  })

  stopwordList2 <- reactive({
    stopword.list <- formatStopwords(input$ignore2, input$stopwords2)
    if (input$CNSstopwords2) {
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

  formatWords2 <- reactive({
    if (nchar(input$words1) > 0 & nchar(input$words2) > 0) {
      if (input$fixCNS2) {
        wl <- list(
          "Text 1" = input$words1 %>%
            str_split("(\\n{1,})") %>%
            unlist() %>%
            as.character() %>%
            cns_fix_input_text(),
          "Text 2" = input$words2 %>%
            str_split("(\\n{1,})") %>%
            unlist() %>%
            as.character() %>%
            cns_fix_input_text()
        )

        if (nchar(input$title1) > 0 & nchar(input$title2) > 0) {
          names(wl) <- c(input$title1, input$title2)
        }

        MakeWordFreqCompare(wordlists = wl,
          stopword.list = stopwordList2(),
          stem = input$stem2,
          rm.stopwords = input$stopwords2
        ) %>%
          cns_fix_word_freq() %>%
          filter_word_size(nch = input$wordSize2)

      } else {
        input$words1 %>%
          str_split("(\\n{1,})") %>%
          unlist() %>%
          as.character() %>%
          str_to_lower() %>%
          MakeWordFreq(stopword.list = stopwordList2(),
                       stem = input$stem2, rm.stopwords = input$stopwords2) %>%
          filter(nchar(word) > input$wordSize2)

        wl <- list(
          "Text 1" = input$words1 %>%
            str_split("(\\n{1,})") %>%
            unlist() %>%
            as.character() %>%
            str_to_lower(),
          "Text 2" = input$words2 %>%
            str_split("(\\n{1,})") %>%
            unlist() %>%
            as.character() %>%
            str_to_lower()
        )

        if (nchar(input$title1) > 0 & nchar(input$title2) > 0) {
          names(wl) <- c(input$title1, input$title2)
        }

        MakeWordFreqCompare(wordlists = wl,
                            stopword.list = stopwordList2(),
                            stem = input$stem2,
                            rm.stopwords = input$stopwords2
        ) %>%
          filter_word_size(nch = input$wordSize2)
      }
    } else {
      cbind("Text 1" = rep("", 0), "Text 2" = rep(0, 0))
    }
  })

  wordPlot <- reactive({
    color.pal <- brewer.pal(6, input$palette)
    if (input$palette == "Greys") {
      color.pal <- rev(brewer.pal(9, input$palette)[-c(1:3)])
    }

    df <- formatWords()

    scale.size <- c(max(input$textSize), min(input$textSize))

    # Clean device
    p <- recordPlot()
    plot.new()
    p

    # Draw plot
    wp1 <- withWarnings(
      MakeWordcloud(x = df, color.set = color.pal, max.words = input$nWords,
                    min.freq = input$wordFreq, scale = scale.size)
    )

    # grab the plot as a grid object
    grid.echo()
    a <- grid.grab()

    wp1$value <- a

    wp1
  })

  comparisonPlot <- reactive({

    df <- formatWords2()

    # Clean device
    p <- recordPlot()
    plot.new()
    p

    if (input$wcType == 1) {
      if (input$palette2 == "Dark2") {
        color.pal <- c("#7570b3", "#66a61e")
      } else if (input$palette2 == "Paired") {
        color.pal <- c("#ff7f00", "#1f78b4")
      } else if (input$palette2 == "Set1") {
        color.pal <- c("#ff7f00", "#984ea3")
      } else if (input$palette2 == "Greys" | input$palette2 == "Black") {
        color.pal <- c("#737373", "#252525")
      }

      df <- df %>%
        log_like_scale_count

      scale.size <- c(max(input$textSize2), min(input$textSize2))

      # Draw plot
      wp1 <- withWarnings(
        df  %>%
          comparison.cloud(max.words = input$nWords2,
                           colors = color.pal, scale = scale.size)
      )
    } else {

      if (input$palette2 == "Black") {
        color.pal <- "#000000"
      } else if (input$palette2 == "Greys") {
        color.pal <- rev(brewer.pal(9, input$palette2)[-c(1:3)])
      } else {
        color.pal <- brewer.pal(6, input$palette2)
      }

      scale.size <- c(max(input$textSize2), min(input$textSize2))

      # Draw plot
      wp1 <- withWarnings(
        df %>%
          commonality.cloud(max.words = input$nWords2, random.order = F, scale = scale.size, colors = color.pal)
      )
    }

    # grab the plot as a grid object
    grid.echo()
    a <- grid.grab()

    wp1$value <- a

    wp1
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

  output$comparisoncloud <- renderPlot({
    validate(
      need(input$words1 != "" & input$words2 != "",
           "Please paste text into the boxes labeled \"Wordcloud text\".")
    )

    wp <- comparisonPlot()
    print(wp$value)
    warning.msgs$list <- wp$warnings
    # message(paste(names(warnings()), collapse = "\n"))
  })

  output$testtext <- renderText(paste("     fingerprint: ", input$fingerprint, "     ip: ", input$ipid))

  output$downloadPlot <- downloadHandler(
    filename = function() {
      fname <- str_replace_all(input$filename, "[\"\\*\\/\\>\\<\\?\\!\\.:\\\\\\|]{1,}", " ") %>%
        str_trim()
      paste(fname, '.png', sep='')
    },
    content = function(file) {
      wp2 <- wordPlot()

      ggsave(filename = file,
             plot = wp2$value,
             width = input$imgwidth,
             height = input$imgwidth,
             units = "in",
             dpi = 300)
    }
  )

  output$downloadcomparisonPlot <- downloadHandler(
    filename = function() {
      fname <- str_replace_all(input$filename2, "[\"\\*\\/\\>\\<\\?\\!\\.:\\\\\\|]{1,}", " ") %>%
        str_trim()
      paste(fname, '.png', sep = '')
    },
    content = function(file) {
      wp2 <- comparisonPlot()

      ggsave(filename = file,
             plot = wp2$value,
             width = input$imgwidth2,
             height = input$imgwidth2,
             units = "in",
             dpi = 300)
    }
  )

  # Observe tab 1
  observe({
    tmp <- formatWords()
    sw <- formatStopwords(input$ignore, input$stopwords)

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
            "Tab1",
            sep = ", "
          ),
          "\n"),
        file = "userInfo.txt",
        append = T
      )
    }
  })

  # Observe tab 2
  observe({
    tmp <- formatWords2()
    sw <- formatStopwords(input$ignore2, input$stopwords2)

    if (nrow(tmp) > 0) {
      cat(
        paste0(
          paste(
            Sys.time(),
            input$ipid,
            input$fingerprint,
            sum(colSums(tmp)),
            nrow(tmp),
            length(sw),
            input$stem2,
            input$stopwords2,
            input$CNSstopwords2,
            input$fixCNS2,
            input$nWords2,
            input$wordSize2,
            input$wordFreq2,
            input$palette2,
            "Tab2",
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
      tt <- data.frame(
        matrix(wl,
               ncol = min(length(wl),
                          max(floor(sqrt(length(wl))), 4))
        ),
        stringsAsFactors = F) %>%
        xtable() %>% print.xtable(include.rownames = F, include.colnames = F, type = 'html') %>%
        HTML()

      tagList(
        h3("Warning: The following words would not fit within the boundaries"),
        tt,
        p("Try decreasing the minimum text size, increasing the text size range. You may also increase the minimum worod length or the number of words allowed.")
      )
    } else if (nchar(input$words) > 0) {
      tagList(
        helpText("If your wordcloud does not fill the square, try adding more words, increasing the word size, or decreasing the word size range. You may also want to increase the number of words allowed or decrease the minimum word length.")
      )
    }
  })

  output$warningMessages2 <- renderUI({
    wl <- warning.msgs$list %>% unlist() %>% as.character()
    if (length(wl) > 0) {
      tt <- data.frame(
        matrix(wl,
               ncol = min(length(wl),
                          max(floor(sqrt(length(wl))), 4))
        ),
        stringsAsFactors = F) %>%
        xtable() %>% print.xtable(include.rownames = F, include.colnames = F, type = 'html') %>%
        HTML()

      tagList(
        h3("Warning: The following words would not fit within the boundaries"),
        tt,
        p("Try decreasing the text size range or the number of words allowed.")
      )
    } else if (nchar(input$words1) > 0 & nchar(input$words2) > 0) {
      tagList(
        helpText("If your wordcloud does not fill the square, try adding more words, increasing the word size, or decreasing the word size range. You may also want to increase the number of words allowed or decrease the minimum word length.")
      )
    }
  })
})
