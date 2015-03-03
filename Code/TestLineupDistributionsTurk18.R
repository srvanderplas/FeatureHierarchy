source("Code/MixtureLineups.R")
library(nullabor)
library(compiler)
library(doMC)
registerDoMC(6)
library(plyr)
library(reshape2)
library(ggplot2)
library(grid)

# Simulation parameters
data.parms <- expand.grid(K=c(3, 5),
                          sd.trend=round(seq(.1, .4, by=.05), 2),
                          sd.cluster=round(seq(.1, .4, by=.05), 2))
data.parms$N <- data.parms$K*15


tmp <- function(M=1000, N=45, K=3, sT=0.3, sC=0.3) {
  data.frame(t(replicate(M, {
    input.pars <- list(N=N, K=K, sd.trend=sT, sd.cluster=sC)
    c(unlist(input.pars), eval.data(gen.data(input.pars)))
  })))
}
nulldist<- cmpfun(tmp)

simulation.results <- ldply(1:nrow(data.parms), function(i) with(data.parms[i,], nulldist(M=1000, N=N, K=K, sT=sd.trend, sC=sd.cluster)), .parallel=T)
names(simulation.results)[3:4] <- c("sd.trend", "sd.cluster")
simulation.results$sd.trend <- round(simulation.results$sd.trend, 2)
simulation.results$sd.cluster <- round(simulation.results$sd.cluster, 2)
save(simulation.results, file = "./Data/SimulationResultsTurk18.Rdata")

load("./Data/SimulationResultsTurk18.Rdata")

longres <- melt(simulation.results, id.vars=1:4, variable.name="type", value.name = "value")
longres$dist <- c("Data", "Max(18 Nulls)")[1+grepl("null", longres$type)]
longres$type <- gsub("null.", "", longres$type, fixed=T)

qplot(data=subset(longres, sd.trend==.2), x=value, y=..scaled.., stat="density", color=dist, linetype=factor(K), geom="line", main="Simulation Results: Trials where SD_T=.2", xlab="Statistic Value") + facet_grid(sd.cluster~type, scales="free", labeller=label_both) + scale_color_discrete("Distribution") + scale_linetype_discrete("K")
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

save(dataset.criteria, file="./Data/SimulationDatasetCriteriaTurk18.Rdata")

dataset.criteria$ParameterSet <- with(dataset.criteria, sprintf("sdT%.2f-sdC%.2f", sd.trend, sd.cluster))
dataset.criteria$ParameterSet[dataset.criteria$type=="cluster"] <- with(dataset.criteria[dataset.criteria$type=="cluster",], sprintf("sdC%.2f-sdT%.2f", sd.cluster, sd.trend))


qplot(data=dataset.criteria, x=LB, xend=UB, y=ParameterSet, yend=ParameterSet, color=dist, geom="segment") + geom_point(aes(x=LB, y=ParameterSet, color=dist))  + geom_point(aes(x=UB, y=ParameterSet, color=dist)) + facet_wrap(~type+K, scales="free") 
