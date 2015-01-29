source("./Code/MixtureLineups.R")
source("./Code/theme_lineup.R")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(nullabor)

set.seed(518290387)

# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("./Data/color-perceptual-kernel.csv")
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("./Data/shape-perceptual-kernel.csv")
# shapetm[9:10,] <- 0
# shapetm[, 9:10] <- 0
shapetm[9,] <- 0
shapetm[,9] <- 0
shapetm[10,] <- 0
shapetm[,10] <- 0

# Lineup Design
data.parms <- expand.grid(N=c(45, 75),
                          K=c(3, 5),
                          sd=c(.35, .45, .55),
                          q=c(.25, .35, .45),
                          rep=1:3)
data.parms[data.parms$K==5]$q = data.parms[data.parms$K==5]$q - .05

plot.parms <- expand.grid(
  color = c(0,1),
  shape = c(0,1),
  reg = c(0,1),
  err = c(0,1),
  ell = c(0,1)
)[c(
  1, # control
  2, 3, # color, shape
  4, 18, # color + shape, color + ellipse
  5, 13, # trend, trend + error
  6, 14 # color + trend, color + trend + error
  ),]

get.aes <- function(r){
  c("Color", "Shape")[which(as.logical(r[1:2]))]
}

get.stats <- function(r){
  c("Reg. Line", "Error Bands", "Ellipses")[which(as.logical(r[3:5]))]
}

data <- ldply(1:nrow(data.parms), function(i) {data.frame(set=i, gen.data(as.list(data.parms[i,])))})

data.stats <- ddply(data, .(set, .sample), 
                    function(df){
                      tmp <- summary(aov(lm(y~x+factor(group) + 0, data=df)))
                      res <- tmp[[1]]$`Mean Sq`
                      return(data.frame(.set=unique(df$set), .sample=unique(df$.sample), Fline = round(res[1]/res[3], 2), Fgroup=round(res[2]/res[3], 2), lineplot=unique(df$target2), groupplot=unique(df$target1)))
                    } )

answers <- ddply(data.stats, .(set), summarize, lineplot=unique(lineplot), groupplot=unique(groupplot))

d_ply(data, .(set), function(df){
  i <- unique(df$set)
  for(j in 1:nrow(plot.parms)){
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,]), ), 
           filename = sprintf("Images/Lineups/set_%d_plot%d.png", i, j), 
           width=6, height=6, units="in", dpi=100)
  }
})

save(data, data.stats, data.parms, plot.parms, answers, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)
