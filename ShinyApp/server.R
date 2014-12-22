library(shiny)
library(ggplot2)
library(plyr)
library(nullabor)

# functions for generating data
source("/home/susan/Documents/Rprojects/FeatureHierarchy/Code/MixtureLineups.R")

# functions for generating data
source("/home/susan/Documents/Rprojects/FeatureHierarchy/Code/theme_lineup.R")



# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("/home/susan/Documents/Rprojects/FeatureHierarchy/Data/color-perceptual-kernel.csv")
shapetm <- read.csv("/home/susan/Documents/Rprojects/FeatureHierarchy/Data/shape-perceptual-kernel.csv")

shinyServer(function(input, output, session){
  
  # Calculate palettes
  color.pal <- reactive({
    best.combo(input$K, colors, colortm)
  })
  
  shape.pal <- reactive({
    best.combo(input$K, shapes, shapetm)
  })
  
  pos <- reactive({
    if(!is.na(input$newdata)){
      sample(1:20, size=as.numeric(input$p))
    }
  })
  
  
  dframe <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=input$lambda, N=input$N, K=input$K)
    }
  })
  
  dframe2 <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=input$lambda2, N=input$N, K=input$K)
    }
  })
  
  nulldata <- reactive({
    if(!is.na(input$newdata)){
      rdply(19, function(.sample) mixture.sim(lambda=input$nulllambda, N=input$N, K=input$K))
    }
  })
  
  data <- reactive({
    if(input$p=='2'){
      tmp <- lineup(true=dframe(), pos=pos()[1], n=20, samples=nulldata())
      tmp <- rbind.fill(subset(tmp, .sample!=pos()[2]), cbind(.sample=pos()[2], dframe2))
    } else {
      tmp <- lineup(true=dframe(), pos=pos()[1], n=20, samples=nulldata())
    }
    tmp
  })
  
  output$plot <- renderPlot({
    dd <- data()
    colorp <- color.pal()
    shapep <- shape.pal()
    
    plot <- ggplot(data=dd, aes(x=x, y=y)) + theme_lineup() + facet_wrap(~.sample) + coord_fixed(ratio=1)
    
    # Set Aesthetics
    if(length(input$aes)==0){
      plot <- plot + geom_point(size=3, shape=1) + 
        scale_shape_discrete(solid=F)
    } else if(length(input$aes)==1){
      if("Color"%in%input$aes){
        plot <- plot + geom_point(aes(color=factor(group)), size=3, shape=1) + 
          scale_color_manual(values=colorp)
      } else {
        plot <- plot + geom_point(aes(shape=factor(group)), size=3) + 
          scale_shape_manual(values=shapep)
      }
    } else {
      plot <- plot + geom_point(aes(color=factor(group), shape=factor(group)), size=3) + 
        scale_color_manual(values=colorp) + 
        scale_shape_manual(values=shapep)
    }
    
    # Set other geoms/aids
    if("Regression Line"%in%input$plotopts){
      plot <- plot + geom_smooth(method="lm", color="black", alpha=.25)
    }
    
    if("Data Ellipses"%in%input$plotopts){
      if("Color"%in%input$aes){
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(colour=factor(group)), fill="transparent")
      } else if("Shape"%in%input$aes){
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), colour="black", fill="transparent")
      } else {
        plot <- plot + stat_ellipse(geom="polygon", level=.9, colour="black", fill="transparent")
      }
    }
      
    print(plot)
  })
  
  output$answer <- renderUI({
    if(input$showAnswer){
      if(input$p=="2"){
        tab.out <- 
          tags$table(
            tags$tr(
              tags$td(paste0("Target 1: ", pos()[1])),
              tags$td(paste0("Target 2: ", pos()[2]))          
            )
          )
      } else {
        tab.out <- 
          tags$table(
            tags$tr(
              tags$td(paste0("Target: ", pos()[1]))
            )
          )
      }
    } else {
      if(input$p=="2"){
        tab.out <- 
          tags$table(
            tags$tr(
              tags$td("Target 1: ?"),
              tags$td("Target 2: ?")
            )          
          )
      } else {
        tab.out <- 
          tags$table(
            tags$tr(
              tags$td("Target: ?")
            )
          )
      }
    }
    tab.out
  })
})
