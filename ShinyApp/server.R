library(shiny)
library(ggplot2)
library(plyr)
library(nullabor)
library(ggthemes)
library(Cairo)
options(shiny.usecairo=T)

#folderStr <- "/home/susan/Documents/Rprojects/FeatureHierarchy"
#folderStr <- "/Users/heike/papers/2015-FeatureHierarchy"
folderStr <- "../"

# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv(sprintf("%s/Data/color-perceptual-kernel.csv", folderStr))
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv(sprintf("%s/Data/shape-perceptual-kernel.csv", folderStr))
# shapetm[9:10,] <- 0
# shapetm[, 9:10] <- 0
shapetm[9,] <- 0
shapetm[,9] <- 0
shapetm[10,] <- 0
shapetm[,10] <- 0

# functions for generating data
source(sprintf("%s/Code/MixtureLineups.R", folderStr))

# functions for generating data
source(sprintf("%s/Code/theme_lineup.R", folderStr))



shinyServer(function(input, output, session){
  
  observe({
    input$newdata
    tmp <- round(runif(50, 1000, 1000000)[50])
    set.seed(tmp)
    updateNumericInput(session, "seed", value=tmp)
  })
  
  # Calculate palettes
  color.pal <- reactive({
    best.combo(input$K, colors, colortm)
  })
  
  shape.pal <- reactive({
    best.combo(input$K, shapes, shapetm)
  })
  
  pos <- reactive({
    if(!is.na(input$newdata)){
      sample(1:20, size=2)
    }
  })
  
  
  dframe <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=input$lambda, N=input$N, K=input$K, q=input$q, sd=input$sd)
    }
  })
  
  dframe2 <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=input$lambda2, N=input$N, K=input$K, q=input$q, sd=input$sd)
    }
  })
  
  nulldata <- reactive({
    if(!is.na(input$newdata)){
      rdply(19, function(.sample) 
        mixture.sim(lambda=input$nulllambda, 
                    N=input$N, 
                    K=input$K, 
                    q=input$q, 
                    sd=input$sd, 
                    slope=runif(1, input$nullrange[1], input$nullrange[2])
                    ))
    }
  })
  
  data <- reactive({
    tmp <- lineup(true=dframe(), pos=pos()[1], n=20, samples=nulldata())
    tmp <- rbind.fill(subset(tmp, .sample!=pos()[2]), cbind(.sample=pos()[2], dframe2()))
    tmp
  })
  
  output$plot <- renderPlot({
    dd <- data()
    colorp <- color.pal()
    shapep <- shape.pal()
    
    plot <- gen.plot(dd, input$aes, input$plotopts, color.pal, shape.pal)
#     plot <- ggplot(data=dd, aes(x=x, y=y)) + theme_lineup() + facet_wrap(~.sample) + coord_fixed(ratio=1)
#     
#     # Set Aesthetics
#     if(length(input$aes)==0){
#       plot <- plot + geom_point(size=3, shape=1) + 
#         scale_shape_discrete(solid=F)
#     } else if(length(input$aes)==1){
#       if("Color"%in%input$aes){
#         plot <- plot + geom_point(aes(color=factor(group)), size=3, shape=1) + 
#           scale_color_manual(values=colorp)
#       } else {
#         plot <- plot + geom_point(aes(shape=factor(group)), size=3) + 
#           scale_shape_manual(values=shapep)
#       }
#     } else {
#       plot <- plot + geom_point(aes(color=factor(group), shape=factor(group)), size=3) + 
#         scale_color_manual(values=colorp) + 
#         scale_shape_manual(values=shapep)
#     }
#     
#     # Set other geoms/aids
#     if("Reg. Line"%in%input$plotopts){
#       plot <- plot + geom_smooth(method="lm", color="black", se=F, fullrange=T)
#     } 
#     if("Error Bands"%in%input$plotopts){
#       plot <- plot + geom_ribbon(stat="smooth", method="lm", fill="black", color="transparent", alpha=.3, fullrange=T)
#     }
#     
#     if("Ellipses"%in%input$plotopts){
#       if("Color"%in%input$aes){
#         plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(colour=factor(group)), fill="transparent")
#       } else if("Shape"%in%input$aes){
#         plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), colour="black", fill="transparent")
#       } else {
#         plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), colour="black", fill="transparent")
#       }
#     }
#       
    plot
  })
  
  output$answer <- renderUI({
    if(input$showAnswer){
      tab.out <- 
        tags$table(
          tags$tr(
            tags$td(paste0("Target 1: ", pos()[1], " "))
          ),
          tags$tr(
            tags$td(paste0("Target 2: ", pos()[2], " "))          
          )
        )
      tab.out <- tagList(tab.out,br(),dataTableOutput("stats"))
    } else {
      tab.out <- 
        tags$table(
          tags$tr(
            tags$td("Target 1: ?")
          ),
          tags$tr(
            tags$td("Target 2: ?")
          )          
        )
    }
    tab.out
  })
  
  stats <- reactive({
    
    dd <- data()
    
#     dd <- ddply(dd, .(.sample), function(df){ 
#       tmp <- df
#       tmp$kmean.cluster <- kmeans(tmp[,c("x", "y")], input$K, iter.max=100)$cluster
#       return(tmp)
#       }
#     )
    
    st <- ddply(dd, .(.sample), summarize, 
                linear.model.r2 = round(summary(lm(y~x))$r.squared, 4),
                aes.group.r2 = round(summary(lm(y~factor(group)))$r.squared, 4))
    st    
  })
  
  output$stats <- renderDataTable(stats(), options=list(pageLength=10))
  
  output$dataset <- downloadHandler(
    filename = function(){
      paste('data-', Sys.time(), '.csv', sep='')
    }, 
    content=function(file){
      st <- isolate(stats())
      dd <- isolate(data())
      qdat <- data.frame(ans.1 = isolate(pos()[1]),
                         ans.2 = isolate(ifelse(input$p=="2", pos()[2], NA)),
                         seed = input$seed)
      qdat <- cbind(qdat, data.frame(lapply(input, function(i) paste(ifelse(length(i)==0, " ", i), collapse=","))))
      names(qdat) <- gsub("plotopts1", "reg.line", names(qdat))
      names(qdat) <- gsub("plotopts2", "data.ellipse", names(qdat))
      names(qdat) <- gsub("aes1", "Color", names(qdat))
      names(qdat) <- gsub("aes2", "Shape", names(qdat))
      names(qdat) <- gsub("^p$", "num.targets", names(qdat))
      qdat <- qdat[,!names(qdat)%in%c("showAnswer", "newdata", "aes", "plotopts")]
      tmp <- merge(dd, st)
      tmp <- merge(tmp, qdat)
      tmp <- tmp[,!grepl("(ss\\.net)|(shiny\\.server)", names(tmp))]
      write.csv(tmp, file, row.names=FALSE)
    }
  )
})
