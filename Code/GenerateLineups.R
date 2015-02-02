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
data.parms <- expand.grid(K=c(3, 5),
                          sd=c(.3, .4, .5, .6),
                          q=c(.2, .25, .3, .35, .4), 
                          rep=1:3)
data.parms$N <- 15*data.parms$K

plot.parms <- expand.grid(
  color = c(0,1),
  shape = c(0,1),
  reg = c(0,1),
  err = c(0,1),
  ell = c(0,1)
)[c(
  1, # control
  2, # color
  3, # shape
  4, # color + shape
  18, # color + ellipse
  20, # color + shape + ellipse
  5, # trend
  13, # trend + error
  6, # color + trend
  30 # color + ellipse + trend + error
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
                      r2 <- summary(lm(y~x, data=df))
                      tmp <- summary(aov(lm(y~x+factor(group) + 0, data=df)))
                      res <- tmp[[1]]$`Mean Sq`
                      data.frame(.sample=unique(df$.sample), 
                                 LineRSq = r2$r.squared, 
                                 Fgroup = round(res[2]/res[3], 2), 
                                 lineplot=unique(df$target2), 
                                 groupplot=unique(df$target1))
                      } )

answers <- ddply(data.stats, .(set), summarize, lineplot=unique(lineplot), groupplot=unique(groupplot))

d_ply(data, .(set), function(df){
  i <- unique(df$set)
  for(j in 1:nrow(plot.parms)){
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,]), ), 
           filename = sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.png", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), 
           width=6, height=6, units="in", dpi=150)
  }
})

save(data, data.stats, data.parms, plot.parms, answers, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)
