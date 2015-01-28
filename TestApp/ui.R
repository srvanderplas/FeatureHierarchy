library(shiny)
seed <- runif(1, 1000, 1000000)
shinyUI(fluidPage(
  titlePanel("Testing Feature Hierarchy Lineups"),
  fluidRow(
    column(
      3, 
      wellPanel(
        singleton(tags$head(tags$style(type="text/css", "#lineAnswer div div,#groupAnswer div div { display: inline-table; width: 15%; margin-right:2%;} div.radio { display: inline-table; width: 15%; margin-right:2%;}"))),
        checkboxGroupInput("lineAnswer", "Linear Trend Plot Number(s)?", choices=1:20, selected=NULL, inline=F),
        radioButtons("lineDifficulty", "Linear Trend: Difficulty", choices=c("very easy", "easy", "medium", "hard", "impossible"), selected = "medium"),
        checkboxGroupInput("groupAnswer", "Clustered Plot Number(s)?", choices=1:20, selected=NULL, inline=F),
        radioButtons("groupDifficulty", "Cluster Difficulty", choices=c("very easy", "easy", "medium", "hard", "impossible"), selected = "medium"),
        checkboxGroupInput("problem", "", choices=c("This lineup is problematic"), inline=T),
        actionButton("counter", "Submit Answer", icon=icon("check-square"))
      ) 
    ),
    column(5,
           div(align="center", imageOutput("plot", width='60%'))
    )
#     , 
#     column(4,
#            downloadButton("dataset", "Download Data"),
#            checkboxInput("showAnswer", "Show Answer", value=TRUE),
#            uiOutput("answer")
#     )
  )
))

