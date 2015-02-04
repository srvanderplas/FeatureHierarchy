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
                          sd=round(c(.25, .35, .45), 2),
                          q=round(c(.2, .25, .3, .35), 2), 
                          rep=1:10)
data.parms$q[data.parms$K==5] <- data.parms$q[data.parms$K==5]-.05
data.parms$q <- round(data.parms$q, 2)
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

data.subplot.stats <- ddply(data, .(set, .sample), 
                            function(df){
                              r2 <- summary(lm(y~x, data=df))
                              data.frame(.sample=unique(df$.sample), 
                                         LineSig = r2$r.squared, 
                                         ClusterSig = cluster(df), 
                                         lineplot=unique(df$target2), 
                                         groupplot=unique(df$target1))
                            } )


data.stats <- ddply(data.subplot.stats, .(set), summarize, 
                    line=LineSig[.sample==lineplot], 
                    cluster=ClusterSig[.sample==groupplot], 
                    null.line = max(LineSig[.sample!=lineplot]), 
                    null.cluster=max(ClusterSig[.sample!=lineplot]))

data.stats <- cbind(data.parms, data.stats)
names(data.stats)[2:3] <- c("sd.trend", "sd.cluster")

load("./Data/SimulationResults.Rdata")
res$sd.cluster <- round(res$sd.cluster, 2)
res$sd.trend <- round(res$sd.trend, 2)
sim.quantile <- function(x){
  df <- subset(res, sd.trend==x$sd.trend & sd.cluster==x$sd.cluster & K==x$K)
  if(nrow(df)==0){
    warning(sprintf("Parameter Set (K=%s, SD_T=%.2f, SD_C=%.2f) not found", x$K, x$sd.trend, x$sd.cluster))
    return(data.frame(line=NA, cluster=NA, null.line=NA, null.cluster=NA))
  } 
  data.frame(
    line=sum(x$line>=res$line)/length(res$line),
    cluster=sum(x$cluster>=res$cluster)/length(res$cluster),
    null.line=sum(x$null.line>=res$null.line)/length(res$null.line),
    null.cluster=sum(x$null.cluster>=res$null.cluster)/length(res$null.cluster)
    )
}

tmp <- ddply(data.stats, .(K, sd.trend, sd.cluster, rep), sim.quantile)


answers <- ddply(data.stats, .(set), summarize, lineplot=unique(lineplot), groupplot=unique(groupplot))

d_ply(data, .(set), function(df){
  i <- unique(df$set)
  for(j in 1:nrow(plot.parms)){
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,]), ), 
           filename = sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.pdf", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), 
           width=6, height=6, units="in", dpi=150)
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,]), ), 
           filename = sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.png", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), 
           width=6, height=6, units="in", dpi=150)
  }
})

save(data, data.stats, data.parms, plot.parms, answers, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)
