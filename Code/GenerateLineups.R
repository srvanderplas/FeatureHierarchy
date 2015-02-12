source("./Code/MixtureLineups.R")
source("./Code/theme_lineup.R")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(nullabor)
library(doMC)
registerDoMC(12)
library(digest)


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

# function to create a list of chosen aesthetics
get.aes <- function(r){
  c("Color", "Shape")[which(as.logical(r[1:2]))]
}

# function to create a list of chosen statistics
get.stats <- function(r){
  c("Reg. Line", "Error Bands", "Ellipses")[which(as.logical(r[3:5]))]
}
# 
# load("./Data/SimulationResults.Rdata")
# sim.quantile <- function(x){
#   df <- subset(simulation.results, sd.trend==x$sd.trend & sd.cluster==x$sd.cluster & K==x$K & N ==x$N)
#   if(nrow(df)==0){
#     warning(sprintf("Parameter Set (K=%s, SD_T=%.2f, SD_C=%.2f) not found", x$K, x$sd.trend, x$sd.cluster))
#     return(data.frame(line=NA, cluster=NA, null.line=NA, null.cluster=NA))
#   } 
#   data.frame(
#     line=sum(x$line>=df$line)/length(df$line),
#     cluster=sum(x$cluster>=df$cluster)/length(df$cluster),
#     null.line=sum(x$null.line>=df$null.line)/length(df$null.line),
#     null.cluster=sum(x$null.cluster>=df$null.cluster)/length(df$null.cluster)
#     )
# }
# 
# 
# # Lineup Design
# data.parms.full <- expand.grid(K=c(3, 5),
#                           sd.trend=round(c(.25, .35, .45), 2),
#                           sd.cluster=1:3)
# data.parms.full$sd.cluster[data.parms.full$K==5] <- c(.2, .25, .3)[data.parms.full$sd.cluster[data.parms.full$K==5]]
# data.parms.full$sd.cluster[data.parms.full$K==3] <- c(.25, .3, .35)[data.parms.full$sd.cluster[data.parms.full$K==3]]
# data.parms.full$sd.cluster <- round(data.parms.full$sd.cluster, 2)
# data.parms.full$N <- 15*data.parms.full$K
# 
# plot.parms <- expand.grid(
#   color = c(0,1),
#   shape = c(0,1),
#   reg = c(0,1),
#   err = c(0,1),
#   ell = c(0,1)
# )[c(
#   1, # control
#   2, # color
#   3, # shape
#   4, # color + shape
#   18, # color + ellipse
#   20, # color + shape + ellipse
#   5, # trend
#   13, # trend + error
#   6, # color + trend
#   30 # color + ellipse + trend + error
# ),]
# 
# set.seed(518290387)
# 
# res <- llply(1:nrow(data.parms.full), function(i){
#   z <- eval.data.quantiles(i, data.parms.full[i,])
#   return(z)
# }, .parallel=T)
# 
# 
# data <- data.frame()
# data.parms <- data.frame()
# data.stats <- data.frame()
# data.subplot.stats <- data.frame()
# data.quantiles <- data.frame()
# data.ntries <- NULL
# 
# for(i in 1:length(res)){
#   data <- rbind.fill(data, res[[i]]$data)
#   data.parms <- rbind.fill(data.parms, data.frame(set=res[[i]]$data.stats$set, data.parms.full[rep(i, nrow(res[[i]]$data.stats)),]))
#   data.stats <- rbind.fill(data.stats, res[[i]]$data.stats)
#   data.subplot.stats <- rbind.fill(data.subplot.stats, res[[i]]$data.subplot.stats)
#   data.quantiles <- rbind.fill(data.quantiles, res[[i]]$quantile.eval)
#   data.ntries <- c(data.ntries, res[[i]]$ntries)
# }
# 
# save(plot.parms, data, data.parms, data.stats, data.subplot.stats, data.quantiles, file="./Data/Lineups.Rdata")

load("./Data/Lineups.Rdata")

plot.names <- c("plain","color", "shape", "colorShape", "colorEllipse", "colorShapeEllipse", "trend", "trendError", "colorTrend", "colorEllipseTrendError")

picture.details <- ddply(data, .(set), function(df){
  i <- unique(df$set)
  z <- data.frame()
  for(j in 1:nrow(plot.parms)){
    z <- rbind(z, save.pics(df, datastats=data.stats[i,], plotparms=plot.parms[j,], plotname=plot.names[j]))
  }
  return(z)
}, .parallel=T)

write.csv(picture.details, "./Images/Lineups/picture-details.csv", row.names=FALSE)
