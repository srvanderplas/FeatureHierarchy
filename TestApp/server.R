library(shiny)
library(ggplot2)
library(plyr)
library(nullabor)
library(ggthemes)
library(Cairo)
options(shiny.usecairo=T)

load("Lineups.rda")
plots <- data.frame(expand.grid(j=1:9, i=1:nrow(answers)))
plots$filename <- sprintf("set_%s_plot%s.png", plots$i, plots$j)

shinyServer(function(input, output, session){
  
  plots <- data.frame(expand.grid(j=1:9, i=1:nrow(answers)))
  plots$filename <- sprintf("set_%s_plot%s.png", plots$i, plots$j)
  
  set.seed(as.numeric(Sys.time()))
  plotseq <- sample(1:nrow(plots), nrow(plots), replace=F)
  plots <- plots[plotseq,]

  observe({
    if(input$counter>0){
      isolate({
        if(!is.null(input$lineAnswer) & !is.null(input$lineDifficulty) & 
             !is.null(input$groupAnswer) & !is.null(input$groupDifficulty)){
          tmp <- rbind(
            data.frame(
              set=plots[input$counter+1, 2], 
              plot=plots[input$counter+1, 1],
              target.type="line",
              target.ans = as.numeric(input$lineAnswer),
              target.diff = input$lineDifficulty
            ),
            data.frame(
              set=plots[input$counter+1, 2], 
              plot=plots[input$counter+1, 1],
              target.type="group",
              target.ans = as.numeric(input$groupAnswer),
              target.diff = input$groupDifficulty
            )
          )
          
          data <- read.csv("./res.csv", stringsAsFactors=F)
          data <- rbind(data, tmp)
          write.csv(data, "./res.csv", row.names=F)
        }
      })
      
      updateCheckboxGroupInput(session, "lineAnswer", selected = NULL)
      updateRadioButtons(session, "lineDifficulty", selected = 3)
      updateCheckboxGroupInput(session, "groupAnswer", selected = NULL)
      updateRadioButtons(session, "groupDifficulty", selected = 3)
    }
  })
  
  output$plot <- renderImage({
    filename <- normalizePath(file.path('../Images/Lineups', 
                                        plots$filename[input$counter+1]))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
})
