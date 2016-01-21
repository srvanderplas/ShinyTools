# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

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
  "Word Clouds",
  fluidRow(
    column(
      12,
      wellPanel(
        id = "inputwell",
        fluidRow(
          column(
            5,
            tags$label("Wordcloud text"),
            br(),
            tags$textarea(id = "words", style = "min-width:100%; min-height = 200px;",
                          rows = 8,
                          placeholder = "Input text here (paste from external document/table)",
                          maxlength = 25000, autofocus = T)
          ),
          column(
            3,
            tags$label("List of words to ignore"),
            tags$textarea(id = "ignore", rows = 6,
                          style = "min-width:100%;",
                          placeholder = "Ignored words (Separate words by spaces, commas, etc.)", maxlength = 5000),
            helpText("Words with 1 or 2 characters are ignored by default.")
          ),
          column(
            4,
            selectInput("palette", "Color Options",
                        choices =  c("Dark" = "Dark2", "Paired" = "Paired",
                                     "Rainbow" = "Set1", "Grey" = "Greys")),
            checkboxInput(inputId = "stem", "Remove word stems?", value = T),
            helpText("Word stems, such as -s, -ed, -ing, are removed.",
                     "Words are replaced with the most common variant which appears in the text."),

            checkboxInput(inputId = "stopwords",
                          "Remove common words", value = T),
            helpText("Words like: the, our, who, whom, have, has")
          )
        ),
        br(),
        fluidRow(
          column(
            4,
            sliderInput("nWords", "Maximum words to include:",
                        min = 25, max = 500, value = 300, step = 25)
          ),
          column(
            4,
            sliderInput("wordSize", "Words must be at least this long:",
                        min = 1, max = 10, value = 2, step = 1)
          ),
          column(
            4,
            sliderInput("wordFreq", "Words must appear this many times in the text:",
                        min = 1, max = 10, value = 1, step = 1)
          )
        ),
        fluidRow(
          column(
            6,
            checkboxInput(inputId = "fixCNS", "Fix CNS-related words?", value = F),
            helpText("Capitalizes common acronyms, prevents \"operator\" and \"operations\" from being grouped with words like \"operate(d)\", and removes work order numbers, procedure numbers, and CR references from the base text.")
          ),
          column(
            6,
            checkboxInput(inputId = "CNSstopwords", "Remove common CNS CR words", value = F),
            helpText("Words such as \"condition\", \"description\", \"attached/attachment\", \"CNS\"")
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      6, offset = 1,
      div(
        style = "margin: auto; display:table;",
        plotOutput("wordcloud", width = "450px", height = "450px")
      )
    ),
    column(
      4,
      tableOutput("warningMessages")
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
    header = headerPanel,
    inverse = TRUE,
    wordcloud_panel
  )
)
