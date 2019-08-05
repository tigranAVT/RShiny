library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("mooWheel", "Select A Song", geniusLyrics$Title),
      selectInput("years", label = "Select Years", choices = unique(geniusLyrics$Year), multiple = T, selected = "2019"),
      numericInput("wordCount", label = "Number of Top Words to Show", value = 10, min = 5, max = 12, step = 1)
    ),
    mainPanel(
      plotOutput("wheel"),
      plotOutput("topN"),
      plotOutput("common"),
      # common words in between years analysis textOutput("")
      plotlyOutput("emotions"),
      # what emoetions prevail textOutput("")
      plotlyOutput("polarityInChart"),
      # ste tikoyi analysna textOutput("")
    ),
  )
)
