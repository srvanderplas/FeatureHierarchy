source("Code/MixtureLineups.R")
library(compiler)
library(doMC)
registerDoMC(12)
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)

# Simulation parameters
data.parms <- expand.grid(K=c(3, 5),
                          sd=round(seq(.2, .5, by=.05), 2),
                          q=round(seq(.1, .4, by=.05), 2))
data.parms$N <- data.parms$K*15

tmp <- function(M=2500, N=45, K=3, sT=0.3, sC=0.3) {
  nulls <- data.frame(t(replicate(M, {
    lp <- data.frame(t(replicate(18, {
      mix = mixture.sim(lambda=0.5, K=K, N=N, q=sC, sd=sT)
      reg <- lm(y~x, data=mix)
      
      c(fline=summary(reg)$r.squared, fgroup=cluster(mix))
    })))
    c(fline=max(lp$fline), fgroup=max(lp$fgroup))
  })))
  
  trends <- replicate(M, {
    mix = mixture.sim(lambda=0, K=K, N=N, q=sC, sd=sT)
    reg <- lm(y~x, data=mix)
    c(fline=summary(reg)$r.squared)
  })
  
  clusters <- replicate(M, {
    mix = mixture.sim(lambda=1, K=K, N=N, q=sC, sd=sT)
    clust <- lm(y~factor(group) + 0, data=mix)
    res <- summary(aov(clust))
    c(fgroup=cluster(mix))
  })
  
  data.frame(N=N, K=K, sd.cluster=sC, sd.trend=sT, null.line = nulls$fline, null.cluster = nulls$fgroup, line=trends, cluster=clusters)
}
nulldist<- cmpfun(tmp)

res <- ldply(1:nrow(data.parms), function(i) with(data.parms[i,], nulldist(M=1000, N=N, K=K, sT=sd, sC=q)), .parallel=TRUE)
res$sd.trend <- round(res$sd.trend, 2)
res$sd.cluster <- round(res$sd.cluster, 2)

save(res, file = "./Data/SimulationResults.Rdata")
load("./Data/SimulationResults.Rdata")

longres <- melt(res, id.vars=1:4, variable.name="type", value.name = "value")
longres$dist <- c("Data", "Max(18 Null Plots)")[1+grepl("null", longres$type)]
longres$type <- gsub("null.", "", longres$type, fixed=T)

qplot(data=subset(longres, sd.trend==.2), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_T=.2", xlab="Statistic Value") + facet_grid(sd.cluster~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Cluster Results (SD_T=0.2).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.trend==.3), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_T=.3", xlab="Statistic Value") + facet_grid(sd.cluster~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Cluster Results (SD_T=0.3).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.trend==.4), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_T=.4", xlab="Statistic Value") + facet_grid(sd.cluster~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Cluster Results (SD_T=0.4).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.trend==.5), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_T=.5", xlab="Statistic Value") + facet_grid(sd.cluster~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Cluster Results (SD_T=0.5).pdf", width=6, height=6, units="in")


qplot(data=subset(longres, sd.cluster==.1), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_C=.1", xlab="Statistic Value") + facet_grid(sd.trend~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Line Results (SD_C=0.1).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.cluster==.2), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_C=.2", xlab="Statistic Value") + facet_grid(sd.trend~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Line Results (SD_C=0.2).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.cluster==.3), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_C=.3", xlab="Statistic Value") + facet_grid(sd.trend~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Line Results (SD_C=0.3).pdf", width=6, height=6, units="in")

qplot(data=subset(longres, sd.cluster==.4), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_C=.4", xlab="Statistic Value") + facet_grid(sd.trend~type+K, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
ggsave("Images/Line Results (SD_C=0.4).pdf", width=6, height=6, units="in")



dataset.criteria <- ddply(longres, .(type, dist, sd.trend, sd.cluster, K), summarize, LB = quantile(value, .25), mean=mean(value), UB = quantile(value, .75))

save(dataset.criteria, file="./Data/SimulationDatasetCriteria.Rdata")

dataset.criteria$ParameterSet <- with(dataset.criteria, sprintf("sdT%.2f-sdC%.2f", sd.trend, sd.cluster))
dataset.criteria$ParameterSet[dataset.criteria$type=="cluster"] <- with(dataset.criteria[dataset.criteria$type=="cluster",], sprintf("sdC%.2f-sdT%.2f", sd.cluster, sd.trend))


qplot(data=dataset.criteria, x=LB, xend=UB, y=ParameterSet, yend=ParameterSet, color=dist, geom="segment") + geom_point(aes(x=LB, y=ParameterSet, color=dist))  + geom_point(aes(x=UB, y=ParameterSet, color=dist)) + facet_wrap(~type+K, scales="free") 
