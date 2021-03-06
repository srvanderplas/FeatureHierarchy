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
  
  computeN <- reactive({
    input$N*input$K
  })
  
  dframe <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=1, N=computeN(), K=input$K, sd.cluster=input$sd.cluster, sd.trend=input$sd.trend)
    }
  })
  
  dframe2 <- reactive({
    if(!is.na(input$newdata)){
      mixture.sim(lambda=0, N=computeN(), K=input$K, sd.cluster=input$sd.cluster, sd.trend=input$sd.trend)
    }
  })
  
  nulldata <- reactive({
    if(!is.na(input$newdata)){
      rdply(19, function(.sample) 
        mixture.sim(lambda=.5, 
                    N=computeN(),  
                    K=input$K, 
                    sd.cluster=input$sd.cluster, 
                    sd.trend=input$sd.trend
                    ))
    }
  })
  
  data <- reactive({
    tmp <- lineup(true=dframe(), pos=pos()[1], n=20, samples=nulldata())
    tmp <- rbind.fill(subset(tmp, .sample!=pos()[2]), cbind(.sample=pos()[2], dframe2()))
    tmp
  })
  
#   output$plot <- renderPlot({
#     dd <- data()
#     colorp <- color.pal()
#     shapep <- shape.pal()
#     plot <- gen.plot(dd, input$aes, input$plotopts, colorp, shapep)
#     plot
#   })
  
  output$plot1 <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    dd <- data()
    colorp <- color.pal()
    shapep <- shape.pal()
    plot <- gen.plot(dd, input$aes, input$plotopts, colorp, shapep)
    ggsave(plot, filename=outfile, width=6, height=6, dpi=100)
    
    # Return a list
    list(src = outfile,
         alt = "Sample Lineup")
  }, deleteFile = TRUE)

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
    
    st <- ddply(dd, .(.sample),
                function(df){
                  # we need to distinguish between two different models - 
                  # one is a straight regression line, 
                  # not considering the groups, 
                  # the other is the clustering index 
                  # not considering the line
                  reg <- lm(y~x, data=df)
                  clust <- lm(y~factor(group) + 0, data=df)
                  res <- summary(aov(clust))
                  data.frame(.sample=unique(df$.sample), 
                             Fline = round(summary(reg)$r.squared, 2), 
                             Fgroup=round(res[[1]]$`F value`[1], 2))
                } )
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
