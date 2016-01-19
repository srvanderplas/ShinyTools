
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

wordcloud_panel <- tabPanel(
  "Word Clouds",
  fluidRow(
    column(
      10, offset = 1,
      wellPanel(
        fluidRow(
          column(
            5,
            tags$label("Wordcloud text"),
            br(),
            tags$textarea(id = "words", style = "min-width:100%; min-height = 100px;",
                          placeholder = "Input text here", maxlength = 25000,
                          rows = 8, cols = 40, autofocus = T)
          ),
          column(
            3,
            tags$label("List of words to ignore"),
            tags$textarea(id = "ignore", rows = 4, cols = 20,
                          style = "min-width:100%;",
                          placeholder = "Ignored words", maxlength = 5000),
            helpText("Words with 1 or 2 characters are ignored by default.")
          ),
          column(
            4,
            sliderInput("nWords", "Maximum words to include",
                        min = 25, max = 200, value = 50, step = 25),
            selectInput("palette", "Color Options",
                        choices = c("Dark" = "Dark2", "Paired" = "Paired",
                                    "Rainbow" = "Set1", "Grey" = "Greys"))
          )
        ),
        fluidRow(
          column(
            4,
            checkboxInput(inputId = "stopwords",
                          "Remove common English words", value = T),
            helpText("Words like: the, our, who, whom, have, has"),
            br(),
            checkboxInput(inputId = "stem", "Remove word stems?", value = T),
            helpText("Word stems, such as -s, -ed, -ing, are removed.",
                     "Words are replaced with the most common variant which appears in the text.")
          ),
          column(
            8,
            checkboxInput(inputId = "fixCNS", "Fix CNS-related words?", value = F),
            helpText("Capitalizes common acronyms, prevents \"operator\" and \"operations\" from being grouped with words like \"operate(d)\", and removes work order numbers, procedure numbers, and CR references from the base text."),
            br(),
            checkboxInput(inputId = "CNSstopwords", "Remove common CNS CR words", value = F),
            helpText("Words such as \"condition\", \"description\", \"attached/attachment\", \"CNS\"")
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      8, offset = 2,
      div(
        style = "margin: auto; display:table;",
        plotOutput("wordcloud", width = "600px", height = "600px")
      )
    )
  )
)


shinyUI(
  navbarPage(
    title = "Analysis Tools",
    theme = "bootstrap.css",
    header = includeCSS("www/extra.css"),
    inverse = TRUE,
    wordcloud_panel
  )
)
