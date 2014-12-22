library(shiny)

shinyUI(fluidPage(
  titlePanel("Mixture Models for Feature Hierarchy Lineups"),
  fluidRow(
    column(
      4,
      wellPanel(
        actionButton("newdata", "New Data"),     
        numericInput("N", "Number of Points", value=50, min=30, max=100, step=5), 
        numericInput("K", "Number of groups", value=3, min=3, max=5, step=1),
        br(),
        fluidRow(
          column(4, radioButtons("p", "# Target Plots", choices=c("1", "2"), selected=1)),
          column(4, checkboxGroupInput("aes", "Aesthetics", 
                                       c("Color", "Shape"), selected=NULL)),
          column(4, checkboxGroupInput("plotopts", "Plot Options", 
                                       c("Regression Line", "Data Ellipses"), selected=NULL))
        ),
        br(),        
        sliderInput(inputId="lambda", label="Group Strength", 
                    min=0.05, max=1, value=1, step=.01, round=F),
        br(),
        conditionalPanel(
          condition = "input.p=='2'", 
          sliderInput(inputId="lambda2", label="Group Strength (Model 2)", 
                      min=0.05, max=1, value=1, step=.01, round=F),
          br()
        ),
        sliderInput(inputId="nulllambda", label="Group Strength (Null Plots)", 
                    min=0.05, max=1, value=.5, step=.01, round=F)
        ,
        br(),
        sliderInput(inputId="sd", label="Line Std Dev.",
                    min=.5, max=2, value=1.5, step=0.25, round=F)
        #,
#         br(),
#         sliderInput(inputId="nullsep", label="Null Cluster Separation",
#                     min=.05, max=1, value=.05, step=0.05, round=F)
        
      )
    ),
    column(8,
           plotOutput("plot",height='800px'),
             column(2, checkboxInput("showAnswer", "Show Answer")),
             column(2, uiOutput("answer"))
    )
  )
))

