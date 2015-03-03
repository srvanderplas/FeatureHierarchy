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
library(Cairo)

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
 
# #----- Set Up Data Generation (Actual Plots) ----
# data.parms.full <- expand.grid(sd.trend=round(c(.15, .25, .35), 2),
#                                sd.cluster=c(.15, .25, .35),
#                                K=3, 
#                                N=45)
# 
# data.parms.full$l1 <- 0
# data.parms.full$l2 <- rep(0:8, times=1)
# plot.parms <- expand.grid(
#   color = c(0,1),
#   shape = c(0,1),
#   reg = c(0,1),
#   err = c(0,1),
#   ell = c(0,1)
# )[c(
#   1, # control
#   2, # color
#   # 3, # shape
#   # 4, # color + shape
#   # 18, # color + ellipse
#   # 20, # color + shape + ellipse
#   5, # trend
#   # 13, # trend + error
#   6#, # color + trend
#   # 30 # color + ellipse + trend + error
# ),]
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
# set.seed(518290387)
# 
# res <- llply(1:nrow(data.parms.full), function(i){
#   z <- eval.data.quantiles(i, data.parms.full[i,])
#   return(z)
# })
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
# data.stats <- merge(data.stats, data.parms.full)
# 
# save(plot.parms, data, data.parms, data.stats, data.subplot.stats, data.quantiles, file="./Data/Lineups.Rdata")

#----- Set Up Data Generation (Trial Plots) ----
set.seed(32509803)
test.data.parms <- data.frame(K=rep(3, 20),
                              type=rep(c("trend", "cluster"), each=10),
                              sd.trend=rep(c(.15, .25), each=10),
                              sd.cluster=rep(c(.25, .15), each=10))
test.data.parms$l1 <- rep(1, 20)
test.data.parms$l2 <- rep(c(1,2), each=10)
test.data.parms$N <- 15*test.data.parms$K
test.data.parms$set <- 1:nrow(test.data.parms)

test.data <- ldply(1:nrow(test.data.parms), function(i){ data.frame(set=i, gen.test.data(test.data.parms[i,]))}, .parallel=T)
test.data <- merge(test.data, test.data.parms[,c("set", "type")], all.x=T, all.y=T)
test.data.subplot.stats <- 
  ddply(test.data, .(set, .sample), 
        function(df){
          reg <- lm(y~x, data=df)
          data.frame(.sample=unique(df$.sample), 
                     LineSig = summary(reg)$r.squared, 
                     ClusterSig = cluster(df), 
                     target1=unique(df$target1), 
                     type=unique(df$type))
        })
test.stats <- 
  ddply(test.data.subplot.stats, .(set), summarize, 
    set=unique(set),
    type = unique(type),
    target1 = unique(target1),
    target.sig = ifelse(unique(type)=="trend", LineSig[.sample==unique(target1)], ClusterSig[.sample==unique(target1)]),
    null.sig = ifelse(unique(type)=="trend", max(LineSig[.sample!=unique(target1)]), max(ClusterSig[.sample!=unique(target1)])),
    K = subset(test.data.parms, test.data.parms$set==unique(set))$K,
    sd.trend = subset(test.data.parms, test.data.parms$set==unique(set))$sd.trend,
    sd.cluster = subset(test.data.parms, test.data.parms$set==unique(set))$sd.cluster,
    l1 = subset(test.data.parms, test.data.parms$set==unique(set))$l1,
    l2 = subset(test.data.parms, test.data.parms$set==unique(set))$l2
  )
test.stats$N <- test.stats$K*15

tmp <- melt(test.stats, id.vars=c(1:3, 6:11), variable.name="sub.type", value.name="significance")
qplot(data=tmp, x=significance, color=sub.type, geom="density") + facet_wrap(~type)

save(test.data.parms, test.data, test.data.subplot.stats, test.stats, file="./Data/TestLineups.Rdata")


----- Plot Generation (Actual Plots) ----
load("./Data/Lineups.Rdata")

plot.names <- c("plain","color", "shape", "colorShape", "colorEllipse", "colorShapeEllipse", "trend", "trendError", "colorTrend", "colorEllipseTrendError")

plot.opts <- data.frame(expand.grid(i=unique(data$set), j=c(1, 2, 7, 9)))

picture.details <- ddply(plot.opts, .(i,j), function(idx){
  i <- idx[1,1]
  j <- idx[1,2]
  save.pics(subset(data, set==i), datastats=data.stats[i,], plotparms=plot.parms[j,], plotname=plot.names[j])
}, .parallel=T)

write.csv(picture.details[,-c(1:2)], "./Images/Lineups/picture-details.csv", row.names=FALSE)

files <- paste0("Images/Lineups/svgs/", list.files("./Images/Lineups/svgs"))
del.files <- !(files%in%picture.details$pic_name)
file.remove(gsub("svg", "pdf", files)[del.files])
file.remove(files[del.files])

#----- Plot Generation (Trial Plots) ----
load("./Data/TestLineups.Rdata")

test.picture.details <- ldply(unique(test.data$set), function(i){
  save.pics(df=subset(test.data, set==i), datastats=test.stats[i,], 
            plotparms=data.frame(color=0, shape=0, reg=0, err=0, ell=0), plotname="plain", testplot=T)
}, .parallel=T)

write.csv(test.picture.details, "./Images/Lineups/picture-details-trial.csv", row.names=FALSE)

#----- Set Up Data Generation (Example Plots) ----
ex.pars <- data.frame(K=rep(3, 2), type=c("trend", "cluster"), sd.trend=c(.15, .25), sd.cluster=c(.25, .15))
ex.pars$N <- 15*ex.pars$K

ex.data <- ldply(1:nrow(ex.pars), function(i){ data.frame(set=i, gen.test.data(ex.pars[i,], N=5))}, .parallel=T)
ex.stats <- ex.pars
ex.stats$lineplot <- 0
ex.stats$groupplot <- 0
ex.stats$target.sig <- 0
ex.stats$target.plot <- c(unique(subset(ex.data, set==1)$target1), unique(subset(ex.data, set==2)$target1))

#----- Plot Generation (Example Plots) ----
for(i in 1:nrow(ex.pars)){
  dataname <- sprintf("example-set-%d", i)
  realfname <- sprintf("example-set-%d", i)
  fname <- realfname
  plotobj <- gen.plot(subset(ex.data, set==i), aes=NULL, stats=NULL)
  write.csv(subset(ex.data, set==i), sprintf("./Images/Lineups/Data/example-data-%d.csv", i), row.names=FALSE)
  interactive_lineup(plotobj,
                     fname=fname, 
                     script="http://www.hofroe.net/examples/lineup/fhaction.js", 
                     toggle="select", width=6, height=1.5, ex=T)
}