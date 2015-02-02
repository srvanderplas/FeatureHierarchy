library(shiny)
library(ggplot2)
library(plyr)
library(nullabor)
library(ggthemes)
library(Cairo)
options(shiny.usecairo=T)

load("Lineups.rda")
plots <- data.frame(expand.grid(j=1:9, i=1:nrow(answers)))
plots$filename <- sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.png", plots$i, plots$j, data.parms$K[plots$i], data.parms$sd[plots$i], data.parms$q[plots$i])
plots$shortfilename <- sprintf("Images/Lineups/set-%d-plot-%d.png", plots$i, plots$j)
testdata.old<- read.csv("./res.csv", stringsAsFactors=F)
testdata.old$filename <- sprintf("set_%s_plot%s.png", testdata.old$set, testdata.old$plot)

plots$freqtested <- unlist(lapply(1:nrow(plots), function(i) sum(testdata.old$filename%in%plots$shortfilename[i])))
plots$prob <- 1/(1+plots$freqtested)
plots$prob <- plots$prob/sum(plots$prob)

shinyServer(function(input, output, session){
  
  plots <- data.frame(expand.grid(j=1:9, i=1:nrow(answers)))
  plots$filename <- sprintf("set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.png", plots$i, plots$j, data.parms$K[plots$i], data.parms$sd[plots$i], data.parms$q[plots$i])
  
  
  set.seed(as.numeric(Sys.time()))
  plotseq <- sample(1:nrow(plots), nrow(plots), replace=F, prob=plots$prob)
  plots <- plots[plotseq,]

  observe({
    if(input$counter>0){
      isolate({
        if(!is.null(input$lineAnswer) & !is.null(input$groupAnswer)){
          tmp <- rbind(
            data.frame(
              set=plots[input$counter+1, 2], 
              plot=plots[input$counter+1, 1],
              target.type="line",
              target.ans = ifelse(is.null(input$lineAnswer), NA, 
                                  as.numeric(input$lineAnswer)),
              target.diff = ifelse(is.null(input$lineDifficulty), NA, 
                                   input$lineDifficulty), 
              problem = !is.null(input$problem)
            ),
            data.frame(
              set=plots[input$counter+1, 2], 
              plot=plots[input$counter+1, 1],
              target.type="group",
              target.ans = ifelse(is.null(input$groupAnswer), NA, 
                                  as.numeric(input$groupAnswer)),
              target.diff = ifelse(is.null(input$groupDifficulty), NA, 
                                   input$groupDifficulty), 
              problem = !is.null(input$problem)
            )
          )
          
          testdata<- read.csv("./res.csv", stringsAsFactors=F)
          testdata$target.type <- as.character(testdata$target.type)
          testdata$target.diff <- as.character(testdata$target.diff)
          testdata<- rbind.fill(testdata, tmp)
          write.csv(testdata, "./res.csv", row.names=F)
        }
      })
      
      updateCheckboxGroupInput(session, inputId="lineAnswer", "Linear Trend Plot Number(s)?", choices=1:20, selected = NULL)
      updateCheckboxGroupInput(session, inputId="groupAnswer", "Clustered Plot Number(s)?", choices=1:20, selected=NULL)
      updateRadioButtons(session, inputId="lineDifficulty", selected = "medium")
      updateRadioButtons(session, inputId="groupDifficulty", selected = "medium")
      updateCheckboxGroupInput(session, inputId="problem", choices=c("This lineup is problematic"))
    }
  })
  
  output$plot <- renderImage({
    filename <- normalizePath(file.path('../Images/Lineups', 
                                        plots$filename[input$counter+1]))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
})
