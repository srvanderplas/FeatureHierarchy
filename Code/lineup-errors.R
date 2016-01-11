details <- read.csv("../../Data/data-picture-details-gini.csv")
load("../../Data/Lineups.Rdata")


sd.trend <- function(x, y) {
  summary(lm(y~x))$sigma
}

sd.cluster <- function(x, y, group) {
  dframe <- data.frame(x, y, group)
  dframe = dframe %>% group_by(group) %>% mutate(
    dx = (x-mean(x))^2,
    dy = (y-mean(y))^2,
    d = sqrt(dx + dy)
  )
  sd(dframe$d)
}



stats <- data %>%  group_by(set, .sample) %>% summarize(
  trend.target = target1[1],
  cluster.target = target2[1], 
  sd.trend = sd.trend(x, y),
  sd.cluster = sd.cluster(x, y, group)
)

stats.lps <- stats %>% group_by(set) %>% summarize(
#  trend.target = trend.target[1],
#  cluster.target = cluster.target[1], 
  trend.null.est = min(sd.trend[.sample!=trend.target]),
  cluster.null.est = min(sd.cluster[.sample!=cluster.target]),
  sd.trend.est = sd.trend[.sample==trend.target],
  sd.cluster.est = sd.cluster[.sample==cluster.target]
)

data.stats <- merge(data.stats, stats.lps, by="set")
save(data, data.parms, data.quantiles, data.stats, data.subplot.stats, plot.parms, file="../../Data/Lineups.Rdata")

####################
# diagnostics
library(ggplot2)
stats$type <- "null"
stats$type[stats$.sample==stats$trend.target] <- "line target"
stats$type[stats$.sample==stats$cluster.target] <- "cluster target"
qplot(sd.trend, sd.cluster, data=stats, colour=type, shape=type) +
  theme_bw() + facet_wrap( facets=~set, labeller="label_both") + 
  scale_colour_manual(values=c("darkred", "steelblue", "grey50")) +
  scale_shape_manual(values=c(16,17, 1))


qplot(sd.trend, sd.trend.est, data=data.stats)
qplot(sd.cluster, sd.cluster.est, data=data.stats)
