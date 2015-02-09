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
                          q=1:3, 
                          rep=1:100)
data.parms$q[data.parms$K==5] <- c(.2, .25, .3)[data.parms$q[data.parms$K==5]]
data.parms$q[data.parms$K==3] <- c(.25, .3, .35)[data.parms$q[data.parms$K==3]]
data.parms$q <- round(data.parms$q, 2)
data.parms$N <- 15*data.parms$K
data.parms$set <- 1:nrow(data.parms)

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
                              reg <- lm(y~x, data=df)
                              data.frame(.sample=unique(df$.sample), 
                                         LineSig = summary(reg)$r.squared, 
                                         ClusterSig = cluster(df), 
                                         lineplot=unique(df$target1), 
                                         groupplot=unique(df$target2))
                            } )


data.stats <- ddply(data.subplot.stats, .(set), summarize, 
                    line=LineSig[.sample==lineplot], 
                    cluster=ClusterSig[.sample==groupplot], 
                    null.line = max(LineSig[.sample!=lineplot & .sample!=groupplot]), 
                    null.cluster=max(ClusterSig[.sample!=groupplot & .sample!=lineplot]))

data.stats <- cbind(data.parms, data.stats)
names(data.stats)[2:3] <- c("sd.trend", "sd.cluster")

load("./Data/SimulationResults.Rdata")
sim.quantile <- function(x){
  df <- subset(res, sd.trend==x$sd.trend & sd.cluster==x$sd.cluster & K==x$K & N ==x$N)
  if(nrow(df)==0){
    warning(sprintf("Parameter Set (K=%s, SD_T=%.2f, SD_C=%.2f) not found", x$K, x$sd.trend, x$sd.cluster))
    return(data.frame(line=NA, cluster=NA, null.line=NA, null.cluster=NA))
  } 
  data.frame(
    line=sum(x$line>=df$line)/length(df$line),
    cluster=sum(x$cluster>=df$cluster)/length(df$cluster),
    null.line=sum(x$null.line>=df$null.line)/length(df$null.line),
    null.cluster=sum(x$null.cluster>=df$null.cluster)/length(df$null.cluster)
    )
}

# Calculate quantiles of datasets compared to simulated quantiles
tmp <- ddply(data.stats, .(K, sd.trend, sd.cluster, rep, N), sim.quantile)

# # Ensure uniform distribution for quantiles
# tmp2 <- melt(tmp, id.vars=1:5, variable.name="dist", value.name="quantile")
# tmp2$par <- paste(tmp2$K, round(tmp2$sd.trend, 2), round(tmp2$sd.cluster, 2), tmp2$N, sep=", ")
# 
# qplot(data=tmp2, x=quantile, y=..scaled.., ylab="Scaled Density", xlab="Quantile of Simulated Distribution", stat="density", geom="line", color=dist, size=I(2)) + facet_wrap(~par)
# 
# qplot(data=subset(tmp2, K==3 & sd.trend==.35), x=quantile, ylab="Count", xlab="Quantile of Simulated Distribution", stat="bin", geom="histogram", fill=dist) + facet_wrap(~dist)
# 
# qplot(data=tmp2, x=quantile, y=..scaled.., ylab="Scaled Density", xlab="Quantile of Simulated Distribution", stat="density", group=interaction(sd.trend, sd.cluster, K, dist), geom="line", color=dist, linetype=factor(K)) + facet_grid(sd.cluster~sd.trend+dist, labeller=label_both)

# Require all quantiles to be between (.2, .8)
tmp.sub <- subset(tmp, rowSums(tmp[,6:9]>.2 & tmp[,6:9]<.8)==4)

# Find first data set with each parameter values and acceptable quantiles
chosen.data.sets <- 
  ddply(tmp.sub, .(K, sd.trend, sd.cluster, N), function(df){
    par.row <- subset(df, rep==min(df$rep))
    return(c(set=subset(data.parms, N==par.row$N & K==par.row$K & sd==par.row$sd.trend & q==par.row$sd.cluster & rep==par.row$rep)$set))
})

# Subset all data-generating sets
data <- subset(data, set%in%chosen.data.sets$set)
data.stats <- subset(data.stats, set%in%chosen.data.sets$set)
data.parms <- subset(data.parms, set%in%chosen.data.sets$set)
data.subplot.stats <- subset(data.subplot.stats, set%in%chosen.data.sets$set)
data.stats <- merge(data.stats, unique(data.subplot.stats[,c("set", "lineplot", "groupplot")]))




answers <- ddply(data.stats, .(set), summarize, lineplot=unique(lineplot), groupplot=unique(groupplot))

d_ply(data, .(set), function(df){
  i <- unique(df$set)
  for(j in 1:nrow(plot.parms)){
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,])), 
           filename = sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.pdf", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), 
           width=6, height=6, units="in", dpi=150)
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,]), ), 
           filename = sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.png", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), 
           width=6, height=6, units="in", dpi=150)
    interactive_lineup("gen.plot", df, filename=sprintf("Images/Lineups/set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f.svg", i, j, data.parms$K[i], data.parms$sd[i], data.parms$q[i]), script="http://www.hofroe.net/examples/lineup/fhaction.js", aes=get.aes(plot.parms[j,]), stats=get.stats(plot.parms[j,]))
  }
})

save(data, data.stats, data.parms, plot.parms, answers, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)
