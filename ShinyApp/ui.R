library(shiny)
seed <- runif(1, 1000, 1000000)
shinyUI(fluidPage(
  titlePanel("Mixture Models for Feature Hierarchy Lineups"),
  fluidRow(
    column(
      3, 
      wellPanel(
        actionButton("newdata", "New Data"),
        numericInput("seed", "Seed Value", value=round(runif(1, 1000, 1000000))),
        fluidRow(column(6, numericInput("N", "Number of Points", value=50, min=25, max=75, step=5)),
                 column(6, numericInput("K", "Number of groups", value=3, min=2, max=6, step=1))), 
        br(),
        fluidRow(column(6, checkboxGroupInput("aes", "Aesthetics", c("Color", "Shape"), selected=NULL)),
                 column(6, checkboxGroupInput("plotopts", "Plot Options", c("Reg. Line", "Error Bands", "Ellipses"), 
                                              selected=NULL))), 
        br(),        
        sliderInput(inputId="lambda", label="Group Strength", 
                    min=0, max=1, value=1, step=.01, round=F),
        sliderInput(inputId="nulllambda", label="Group Strength (Null Plots)", 
                    min=0, max=1, value=.5, step=.01, round=F),
        conditionalPanel( 
          condition = "input.p=='2'", 
          sliderInput(inputId="lambda2", label="Group Strength (Model 2)", 
                      min=0, max=1, value=0, step=.01, round=F)
        ),
        br(),
        h4("Data Generation Options"),
        sliderInput(inputId="sd", label="Std Dev.",
                    min=.2, max=2, value=.3, step=0.1, round=F)
        ,
        br(),
        sliderInput(inputId="q", label="Within Cluster Std. Dev.",
                    min=.2, max=.8, value=.3, step=0.05, round=F),
        br(),
        sliderInput(inputId="nullrange", "Null Slope range", 
                    min=0, max=1, value=c(.2, .8), step=.05)
        
      ) 
    ),
    column(5,
           div(align="center", plotOutput("plot", width='100%', height='550px'))
    ), 
    column(4,
           downloadButton("dataset", "Download Data"),
           checkboxInput("showAnswer", "Show Answer", value=TRUE),
           uiOutput("answer")
    )
  )
))

