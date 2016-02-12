# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS) # Additional Bootstrap Controls

# --- Extra Shiny UI functions -------------------------------------------------
checkboxInputPopover <- function(
  inputId, title, value = F,
  btnclass = "btn btn-xs btn-primary", buttonText = "help",
  popoverTitle = title, popoverContent = "", popoverPlacement = "right",
  popoverTrigger = "click", popoverOptions = list(container =  "body")){
  inputList <- list(id = inputId,
                 class = "shiny-bound-input",
                 type = "checkbox")
  if (value) inputList$checked <- "checked"
  div(
    class = "checkbox",
    tags$label(
      tag("input", inputList),
      span(title)
    ),
    tags$button(id = paste0(inputId, "-help"), class = btnclass, buttonText),
    bsPopover(id = paste0(inputId, "-help"),
              title = popoverTitle,
              content = popoverContent,
              placement = popoverPlacement, trigger = popoverTrigger, options = popoverOptions)
  )
}

# --- User ID functions --------------------------------------------------------

inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "md5.js", type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = "shinyBindings.js", type = 'text/javascript'))),
    # tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value = as.character(value), type = "text", style = "display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "md5.js", type = 'text/javascript'))),
    singleton(tags$head(tags$script(src = "shinyBindings.js", type = 'text/javascript'))),
    # tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value = as.character(value), type = "text", style = "display:none;")
  )
}

# --- Wordcloud Panel ----------------------------------------------------------
wordcloud_panel <- tabPanel(
  title = "Word Clouds",
  value = "wordCloud",
  fluidRow(
    column(
      4,
      wellPanel(
        tags$label("Wordcloud text"),
        br(),
        tags$textarea(id = "words", style = "min-width:100%; min-height = 200px;",
                      rows = 8,
                      placeholder = "Input text here (paste from external document/table)",
                      maxlength = 25000, autofocus = T),
        br(),
        tags$label("List of words to ignore"),
        tags$textarea(id = "ignore", rows = 2,
                      style = "min-width:100%;",
                      placeholder = "Ignored words (Separate words by spaces, commas, etc.)", maxlength = 5000),
        br(),
        fluidRow(
          style = "padding-top:25px;",
          column(
            6,
            style = "padding-right:15px;",
            sliderInput("nWords", "Include up to __ words:",
                        min = 25, max = 500, value = 300, step = 25),
            sliderInput("wordSize", "Words must be at least this long:",
                        min = 1, max = 10, value = 2, step = 1),
            sliderInput("wordFreq", "Words must appear __ times in the text:",
                        min = 1, max = 10, value = 1, step = 1),
            sliderInput("textSize", "Text size range:",
                        min = .1, max = 5, value = c(.2, 3.5), step = .1)
          ),
          column(
            6,
            style = "padding-left:15px;",
            selectInput("palette", "Color Options",
                        choices =  c("Dark" = "Dark2", "Paired" = "Paired",
                                     "Rainbow" = "Set1", "Grey" = "Greys")),
            br(),
            checkboxInputPopover(
              inputId = "stem", title = "Remove word stems?", value = T,
              popoverContent = HTML("Stems are common endings, like -s, -ed, and -ing. <br/> Words are replaced with the most common variant in the text that has the same stem.")),
            checkboxInputPopover(
              inputId = "stopwords", title = "Remove common words", value = T,
              popoverContent = "e.g. the, our, who, whom, have, has"),
            br(),
            checkboxInputPopover(
              inputId = "fixCNS",
              title = "Fix CNS-related words?",
              popoverContent =
                HTML("Performs the following operations: <ul><li>Capitalizes common acronyms</li><li>prevents \"operator\" and \"operations\" from being grouped with words like \"operate(d)\"</li><li>removes work order numbers, procedure numbers, and CR references from the text</li></ul>")
            ),
            checkboxInputPopover(
              inputId = "CNSstopwords", title = "Remove common CNS CR words", value = F,
              popoverContent = "Words such as \"condition\", \"description\", \"attached/attachment\", \"CNS\""
            )
          )
        )
      ),
      br(),
      wellPanel(
        h3("Download Wordcloud"),
        fluidRow(
          column(
            6,
            textInput(inputId = "filename", label = "File Name (no extension)", value = "wordcloud")
          ),
          column(
            6,
            numericInput(inputId = "imgwidth", label = "Image Width/Height (in)",
                         value = 6, min = 3, max = 9, step = .25)
          )
        ),
        downloadButton("downloadPlot", label = "Download", class = 'btn btn-info')
      )
    ),
    column(
      8,
      plotOutput("wordcloud", width = "450px", height = "450px"),
      br(),
      div(
        class = "hidden-xs",
        uiOutput("warningMessages")
      )
    )
  )
)

# --- Comparison/Commonality Cloud Panel ---------------------------------------
comparisoncloud_panel <- tabPanel(
  title = "Comparison Clouds",
  value = "comparisonCloud",
  fluidRow(
    column(
      4,
      wellPanel(
        radioButtons("wcType", "Wordcloud Type:",
                     choices = c("Comparison - show words more common in one text than the other" = 1,
                                 "Commonality - show words common to both texts" = 2),
                     inline = F
        ),
        br(),
        tags$label("Wordcloud text"),
        br(),
        fluidRow(
          column(
            6,
            textInput(inputId = "title1", label = "Text is from:", placeholder = "Some set of documents"),
            tags$textarea(id = "words1", style = "min-width:100%; min-height = 200px;",
                          rows = 8,
                          placeholder = "Input first set of text here (paste from external document/table)",
                          maxlength = 25000, autofocus = T)
          ),
          column(
            6,
            textInput(inputId = "title2", label = "Text is from:", placeholder = "A different set of documents"),
            tags$textarea(id = "words2", style = "min-width:100%; min-height = 200px;",
                          rows = 8,
                          placeholder = "Input second set of text here (paste from external document/table)",
                          maxlength = 25000, autofocus = T)
          )
        ),
        tags$label("List of words to ignore"),
        tags$textarea(id = "ignore2", rows = 2,
                      style = "min-width:100%;",
                      placeholder = "Ignored words (Separate words by spaces, commas, etc.)", maxlength = 5000),
        br(),
        fluidRow(
          style = "padding-top:25px;",
          column(
            6,
            style = "padding-right:15px;",
            sliderInput("nWords2", "Include up to __ words:",
                        min = 25, max = 520, value = 300, step = 25),
            sliderInput("wordSize2", "Words must be at least this long:",
                        min = 1, max = 10, value = 2, step = 1),
            sliderInput("textSize2", "Text size range:",
                        min = .1, max = 5, value = c(.2, 3.5), step = .1)
          ),
          column(
            6,
            style = "padding-left:15px;",
            selectInput("palette2", "Color Options",
                          choices =  c("Dark" = "Dark2", "Paired" = "Paired",
                                       "Rainbow" = "Set1", "Grey" = "Greys")),
            br(),
            checkboxInputPopover(
              inputId = "stem2", title = "Remove word stems?", value = T,
              popoverContent = HTML("Stems are common endings, like -s, -ed, and -ing. <br/> Words are replaced with the most common variant in the text that has the same stem.")),
            checkboxInputPopover(
              inputId = "stopwords2", title = "Remove common words", value = T,
              popoverContent = "e.g. the, our, who, whom, have, has"),
            br(),
            checkboxInputPopover(
              inputId = "fixCNS2",
              title = "Fix CNS-related words?",
              popoverContent =
                HTML("Performs the following operations: <ul><li>Capitalizes common acronyms</li><li>prevents \"operator\" and \"operations\" from being grouped with words like \"operate(d)\"</li><li>removes work order numbers, procedure numbers, and CR references from the text</li></ul>")
            ),
            checkboxInputPopover(
              inputId = "CNSstopwords2", title = "Remove common CNS CR words", value = F,
              popoverContent = "Words such as \"condition\", \"description\", \"attached/attachment\", \"CNS\""
            )
          )
        )
      ),
      br(),
      wellPanel(
        h3("Download Wordcloud"),
        fluidRow(
          column(
            6,
            textInput(inputId = "filename2", label = "File Name (no extension)", value = "wordcloud")
          ),
          column(
            6,
            numericInput(inputId = "imgwidth2", label = "Image Width/Height (in)",
                         value = 6, min = 3, max = 9, step = .25)
          )
        ),
        downloadButton("downloadcomparisonPlot", label = "Download", class = 'btn btn-info')
      )
    ),
    column(
      8,
      plotOutput("comparisoncloud", width = "450px", height = "450px"),
      br(),
      div(
        class = "hidden-xs hidden-sm",
        uiOutput("warningMessages2")
      )
    )
  )
)


# --- Header -------------------------------------------------------------------
headerPanel <- tagList(
  includeCSS("www/extra.css"),
  inputIp("ipid"),
  inputUserid("fingerprint")
)

# --- Shiny UI Function --------------------------------------------------------
shinyUI(
  navbarPage(
    title = "Analysis Tools",
    theme = "bootstrap.css",
    id = "tab",
    header = headerPanel,
    inverse = TRUE,
    wordcloud_panel,
    comparisoncloud_panel
  )
)
