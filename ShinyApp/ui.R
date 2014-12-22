library(shiny)

shinyUI(fluidPage(
  titlePanel("Mixture Models for Feature Hierarchy Lineups"),
  fluidRow(
    column(
      4,
      wellPanel(
        actionButton("newdata", "New Data"),     
        numericInput("N", "Number of Points", value=50, min=30, max=100, step=5), 
        numericInput("K", "Number of groups", value=3, min=2, max=10, step=1),
        br(),
        fluidRow(
          column(4, radioButtons("p", "# Target Plots", choices=c("1", "2"), selected=1)),
          column(4, checkboxGroupInput("aes", "Aesthetics", 
                                       c("Color", "Shape"), selected=NULL)),
          column(4, checkboxGroupInput("plotopts", "Plot Options", 
                                       c("Regression Line", "Data Ellipses"), selected=NULL))
        ),
        br(),        
        sliderInput(inputId="lambda", label="Proportion Group Model", 
                    min=0, max=1, value=.5, step=.05, round=F),
        br(),
        conditionalPanel(
          condition = "input.p=='2'", 
          sliderInput(inputId="lambda2", label="Proportion Group Model (2nd Target)", 
                      min=0, max=1, value=1, step=.05, round=F),
          br()
        ),
        sliderInput(inputId="nulllambda", label="Null Plots: Group Proportion", 
                    min=0, max=1, value=1, step=.05, round=F)
        
      )
    ),
    column(8,
           plotOutput("plot",height='800px'),
             column(2, checkboxInput("showAnswer", "Show Answer")),
             column(2, uiOutput("answer"))
    )
  )
))

