
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

wordcloud_panel <- tabPanel(
  "WordCloud",
  fluidRow(
    column(
      10, offset = 1,
      wellPanel(
        fluidRow(
          column(
            4,
            tags$label("Wordcloud text"),
            br(),
            tags$textarea(id = "words", style = "min-width:100%; min-height = 100px;",
                          placeholder = "Input text here", maxlength = 25000,
                          rows = 8, cols = 40, autofocus = T)
          ),
          column(
            2,
            tags$label("List of words to ignore"),
            helpText("Words with fewer than 3 characters will be ignored by default."),
            tags$textarea(id = "ignore", rows = 4, cols = 20,
                          placeholder = "Ignored words", maxlength = 5000)
          ),
          column(
            2,
            checkboxInput(inputId = "stopwords",
                          "Remove common English words", value = T),
            helpText("Words like: the, our, who, whom, have, has"),
            checkboxInput(inputId = "stem", "Remove word stems?", value = T),
            helpText("Word stems, such as -s, -ed, -ing, are removed.",
                     "Words are replaced with the most common variant")
          ),
          column(
            4,
            sliderInput("nWords", "Maximum words to include",
                        min = 15, max = 200, value = 50),
            selectInput("palette", "Color Options",
                        choices = c("Dark" = "Dark2", "Paired" = "Paired",
                                    "Rainbow" = "Set1", "Grey" = "Greys"))
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
    "Analysis Tools",
    wordcloud_panel
  )
)
