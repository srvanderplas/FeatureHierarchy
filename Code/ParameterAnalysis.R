# Stats-analysis
# Compare statistics for each lineup data set

setwd("./Code")
source("MixtureLineups.R")
source("theme_lineup.R")

setwd("../Images/Turk16/")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(nullabor)
library(grid)
library(digest)
library(Cairo)

# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("../../Data/color-perceptual-kernel.csv")
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("../../Data/shape-perceptual-kernel.csv")
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

# answers <- read.csv("picture-details.csv")
# # answers$pic_name <- gsub("Images/Lineups/", "", gsub("svg", "pdf", answers$pic_name))
# answers2 <- answers
# 
# for(i in 1:nrow(answers)){
#   df <- read.csv(sprintf("./Data/%s.csv", answers$data_name[i]), stringsAsFactors=FALSE)
#   
#   stats <- ddply(df, .(.sample, target1, target2), eval.df)
#   
#   write.csv(stats, file=sprintf("./Stats/%s.csv", answers$data_name[i]))
#   
#   plottype <- gsub("turk16-", "", answers$test_param[i])
#   
#   plot.parms <- c(grepl("[Cc]olor", plottype), grepl("[Ss]hape", plottype), grepl("[Tt]rend", plottype), grepl("Error", plottype), grepl("Ellipse", plottype))
#   
#   plotans <- sprintf("%d, %d, %d", unique(df$target1), unique(df$target2), which.min(stats$gini))
#   answers2[i,"obs_plot_location"] <- plotans
# #   plot <- gen.plot(df, aes=get.aes(plot.parms), stats=get.stats(plot.parms)) + 
# #     ggtitle(sprintf("Data Targets: %s, File Targets: %s", plotans, answers$obs_plot_location[i]))
# #   print(plot)
# }
# 
# # write.csv(answers2, "data-picture-details-gini.csv", row.names=FALSE)

answers2 <- read.csv("data-picture-details-gini.csv", stringsAsFactors = F)
res <- ldply(unique(answers2$data_name), function(x){
  return(cbind(data_name = x, read.csv(paste0("./Stats/", x, ".csv"))))
})

library(stringr)
res$target <- paste0(c("", "trend")[(res$.sample==res$target1)+1], c("", "cluster")[(res$.sample==res$target2) + 1])
res$target[res$target==""] <- "decoy"

res$set <- str_extract(res$data_name, pattern="set-\\d{1,2}") %>% str_replace(pattern="set-", "") %>% as.numeric()
res$k <- str_extract(res$data_name, pattern="k-\\d") %>% str_replace(pattern="k-", "") %>% as.numeric()
res$sd.line <- str_extract(res$data_name, pattern="sdline-0.\\d{1,2}") %>% str_replace(pattern="sdline-", "") %>% as.numeric()
res$sd.group <- str_extract(res$data_name, pattern="sdgroup-0.\\d{1,2}") %>% str_replace(pattern="sdgroup-", "") %>% as.numeric()

res <- ddply(res, .(k, sd.line, sd.group), transform, rep = set-min(set)+1)
res$sdLine <- paste("sigma[T] :",res$sd.line)
res$sdGroup <- paste("sigma[C] :", res$sd.group)
res$K <- paste("K: ", res$k)

ggplot(data=res) + 
  geom_jitter(aes(x=line, y=sd.line, color=target)) + 
  facet_grid(target ~ K+sdGroup, labeller=label_parsed)

ggplot(data=res) + 
  geom_point(aes(x=line, y=factor(set), color=target, size=target), shape=1) + 
  facet_grid(K + sdGroup + sdLine~., space="free", scale="free", labeller=label_parsed) +
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  scale_size_manual(values=c(4,3,4)) + 
  theme_bw()

ggplot(data=res) + 
  geom_jitter(aes(x=group, y=sd.group, color=target)) + 
  facet_grid(target ~ K+sdLine, labeller=label_parsed)


ggplot(data=res) + 
  geom_point(aes(x=group, y=factor(set), color=target, size=target), shape=1) + 
  facet_grid(K+sdLine+sdGroup~., space="free", scale="free", labeller=label_parsed) +
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  scale_size_manual(values=c(4,3,4)) + 
  theme_bw()




ggplot(data=res) + 
  geom_segment(aes(x=group, y=rep-.3, xend=group, yend=rep+.3, color=target, size=target)) + 
  facet_grid(sdGroup+sdLine~K, space="free", scale="free", labeller=label_parsed) +
  scale_size_manual(values=c(1.5, 1, 1.5)) + 
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  theme_bw() + 
  scale_y_continuous(breaks=c(1, 2, 3))

ggplot(data=res) + 
  geom_segment(aes(x=line, y=rep-.3, xend=line, yend=rep+.3, color=target, size=target)) + 
  facet_grid(sdLine+sdGroup~K, space="free", scale="free", labeller=label_parsed) +
  scale_size_manual(values=c(1.5, 1, 1.5)) + 
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  theme_bw() + 
  scale_y_continuous(breaks=c(1, 2, 3))

ggplot(data=res) + 
  geom_segment(aes(x=group, y=rep-.3, xend=group, yend=rep+.3, color=target, size=target), alpha=.5) + 
  facet_grid(sdGroup~K+sdLine, labeller=label_parsed) +
  scale_size_manual(values=c(1.5, 1, 1.5)) + 
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  theme_bw() + 
  scale_y_continuous(breaks=c(1, 2, 3))

# When sigma_C is much lower than sigma_T (i.e. .15 - .2 more) the cluster result begins to merge with the rest of the decoys...

ggplot(data=res) + 
  geom_segment(aes(x=line, y=rep-.3, xend=line, yend=rep+.3, color=target, size=target), alpha=.5) + 
  facet_grid(sdLine~K+sdGroup, labeller=label_parsed) +
  scale_size_manual(values=c(1.5, 1, 1.5)) + 
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  theme_bw() + 
  scale_y_continuous(breaks=c(1, 2, 3))

# When sigma_T and sigma_C are close to 0.3 for K=3, there is little separation. 
# This occurs at sigma_T = 0.25, sigma_C=0.2, 0.25, 0.3 for K=5


# summarise for each lineup:
stats <- ddply(res, .(data_name, target), summarize, 
               line=max(line),
               group=max(group))
stats.line <- dcast(stats, data_name~target, value.var="line")
names(stats.line)[3] <- "decoy.trend"
stats.group <- dcast(stats, data_name~target, value.var="group")
names(stats.group)[3] <- "decoy.cluster"
dstats <- merge(stats.line[,c("data_name", "trend", "decoy.trend")], 
                stats.group[,c("data_name", "cluster", "decoy.cluster")], by="data_name")

load("../../Data/modeldata.Rdata")
# get values from modeldata to match
dmodel <- ddply(modeldata, .(data_name, plottype), summarize,
                cluster.correct=sum(cluster.correct), trend.correct=sum(trend.correct),
                evals=length(data_name))

dstats <- merge(dstats, subset(dmodel, plottype=="plain"), by="data_name")
qplot(trend-decoy.trend,trend.correct/evals, data=dstats) + theme_bw() + geom_smooth()
qplot(cluster-decoy.cluster,cluster.correct/evals, data=dstats) + theme_bw() + geom_smooth()

dstats$cd <- dstats$cluster - dstats$decoy.cluster
cl.glm <- glm(cbind(cluster.correct, evals-cluster.correct)~cd, data=dstats, family=binomial(link="cloglog"))
summary(cl.glm)

grid <- data.frame(expand.grid(
  cd = seq(-0.015, 0.1, by=0.005)
))
grid$pred <- stats::predict(cl.glm, newdata=grid, type="response")

qplot(cluster-decoy.cluster,cluster.correct/evals, data=dstats) + theme_bw() + geom_line(aes(x=cd, y=pred), colour="red", data=grid)


# Simulation-redo
# Copied from TestLineupDistributions.R, with modified parameters.
setwd("../../")
source("Code/MixtureLineups.R")
library(nullabor)
library(compiler)
library(doMC)
registerDoMC(8)
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)

# Simulation parameters
data.parms <- expand.grid(sd.trend=round(c(.25, .35, .45), 2),
                               sd.cluster=1:3,
                               K=c(3, 5))

data.parms$sd.cluster[data.parms$K==5] <- c(.2, .25, .3)[data.parms$sd.cluster[data.parms$K==5]]
data.parms$sd.cluster[data.parms$K==3] <- c(.25, .3, .35)[data.parms$sd.cluster[data.parms$K==3]]
data.parms$sd.cluster <- round(data.parms$sd.cluster, 2)
data.parms$N <- data.parms$K*15


tmp <- function(M=1000, N=45, K=3, sT=0.3, sC=0.3) {
  do.call("rbind", lapply(1:M, function(i) {
    input.pars <- list(N=N, K=K, sd.trend=sT, sd.cluster=sC)
    dat <- gen.data(input.pars)
    res <- ddply(dat, .(.sample), eval.df)
    res$trend.target <- unique(dat$target1)
    res$cluster.target <- unique(dat$target2)
    res$rep <- i
    data.frame(res, input.pars)
  }))
}
nulldist<- cmpfun(tmp)

simulation.results <- ldply(1:nrow(data.parms), function(i) with(data.parms[i,], nulldist(M=1000, N=N, K=K, sT=sd.trend, sC=sd.cluster)), .parallel=T)
names(simulation.results) <- c("sample", "R2", "C2", "Gini", "trend.target", "cluster.target", "rep", "N", "K", "sd.trend", "sd.cluster")
simulation.results$sd.trend <- round(simulation.results$sd.trend, 2)
simulation.results$sd.cluster <- round(simulation.results$sd.cluster, 2)
save(simulation.results, file = "./Data/Turk16ParameterSimulation.Rdata")

load("./Data/Turk16ParameterSimulation.Rdata")

simulation.results$sdLine <- paste("sigma[T] :",simulation.results$sd.trend)
simulation.results$sdGroup <- paste("sigma[C] :", simulation.results$sd.cluster)
simulation.results$target <- paste0(c("", "trend")[(simulation.results$sample==simulation.results$trend.target)+1], c("", "cluster")[(simulation.results$sample==simulation.results$cluster.target) + 1])
simulation.results$target[simulation.results$target==""] <- "decoy"

simulation.results <- ddply(simulation.results, .(K, sd.trend, sd.cluster, rep), transform, trend.r2 = R2[sample==trend.target], cluster.c2=C2[sample==cluster.target])
simulation.results <- ddply(simulation.results, .(K, sd.trend, sd.cluster), transform, trend.rank=floor(order(trend.r2)/20), cluster.rank=floor(order(cluster.c2)/20))

simulation.summary <- ddply(simulation.results, .(K, sd.trend, sd.cluster, sdLine, sdGroup, rep, target), summarise, min.r2 = min(R2), max.r2 = max(R2), min.c2 = min(C2), max.c2 = max(C2))

ggplot() + 
  geom_segment(aes(x=max.r2, xend=min.r2, y=rep, yend=rep, color=target, size=target), data=subset(simulation.summary, target=="decoy"), alpha=.25) + 
  geom_point(aes(x=min.r2, y=rep, color=target, size=target), shape=1, alpha=.25, data=subset(simulation.summary, target%in%c("cluster", "trend"))) + 
  facet_grid(sdLine~K+sdGroup, labeller=label_parsed) +
  scale_size_manual(values=c(.75, .25, 1)) + 
  scale_colour_manual(values=c("red", "grey70", "blue")) +
  theme_bw() + 
  ggtitle("Change in R^2 with parameter values")

ggplot() + 
  geom_segment(aes(x=max.c2, xend=min.c2, y=rep, yend=rep, color=target, size=target), data=subset(simulation.summary, target=="decoy"), alpha=.25) + 
  geom_point(aes(x=min.c2, y=rep, color=target, size=target), shape=1, alpha=.25, data=subset(simulation.summary, target%in%c("cluster", "trend"))) + 
  facet_grid(sdGroup~K+sdLine, labeller=label_parsed, scales="free") +
  scale_size_manual(values=c(1, .25, .75)) + 
  scale_colour_manual(values=c("red", "grey70", "blue")) +
  theme_bw() + 
  ggtitle("Change in C^2 with parameter values")

ggplot(data=simulation.results) + 
  geom_point(aes(x=C2, y=rep, color=target, size=target, shape=target), shape=1, alpha=.5) + 
  facet_grid(sdGroup~K+sdLine, labeller=label_parsed) +
  scale_size_manual(values=c(1.5, 1, 1.5)) + 
  scale_colour_manual(values=c("red", "grey50", "blue")) +
  theme_bw() + 
  ggtitle("Change in C^2 with parameter values")


# I think that we have a case of some fairly bad random samples... combined with the fact that values were REALLY compressed for K=5, particularly in the sigma_T=0.25, sigma_C=0.3 condiditon, where blue dots are actually higher than red dots for C^2. (The analogous condition for K=3 shows some signs of this starting to occur, though...)

# The bigger problem for us is that for R^2, red and blue were on opposite sides of the distribution - this isn't true for C^2, which could mean that we made clustering a bit harder by default. Of course, our results show that if that's the case, we're really good at clustering.
# It could also be that we're balancing out - "this plot has no linear trend" vs. "strong linear trend", compared with "strong clustering in a trend plot" vs. "strong clustering in a cluster plot". 

