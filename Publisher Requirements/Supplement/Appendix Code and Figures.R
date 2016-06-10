# --- Packages -----------------------------------------------------------------
library(stringr) # strings
library(lubridate) # dates/times
library(reshape2) # manipulating data
library(dplyr) # cleaning/summarizing data
library(magrittr) # pipes

library(ggplot2) # plots
library(grid) # manipulate ggplot2/grid graphics
suppressMessages(library(gridExtra)) # combine ggplot2 plots
library(RColorBrewer) # Color schemes

library(nullabor) # Lineup package
library(digest) # hash text
library(Cairo) # graphics device - unicode characters

library(compiler) # Make simulations run faster

library(lme4) # mixed models

library(xtable) # Output tables to LaTeX

suppressMessages(library(multcomp)) # Multiple comparisons

library(vinference) # install from heike/vinference github

# --- Important Functions ------------------------------------------------------
# Identify the best combination of a set of colors/shapes based on a 
# provided distance matrix
best.combo <- function(ngroups = 3, palette, dist.matrix){
  # check distance matrix
  if (nrow(dist.matrix) != length(palette) | 
      ncol(dist.matrix) != length(palette)) {
    stop(paste0("The distance matrix does not match the size of the palette. ",
                "It should be ", length(palette), "x", length(palette), "."))
  }
  if ( (sum(dist.matrix < 0) > 0) ) {
    stop("Distance matrix cannot have negative entries.")
  }
  
  require(combinat)
  clist <- t(combn(1:length(palette), ngroups))
  pairwise.combos <- t(combn(1:ngroups, 2))
  res <- rowSums(apply(pairwise.combos, 1, function(i){
    diag(as.matrix(dist.matrix[clist[,i[1]], clist[,i[2]]]))
  }))
  
  return(palette[clist[which.max(res),]])
}

# Simulate clustered data: N points, in K clusters, with 
# sd.cluster within-cluster standard deviation
sim.clusters <- function(K, N, sd.cluster=.3){  
  xc <- sample(1:K, replace = F)
  yc <- sample(1:K, replace = F)
  xc <- jitter(xc, amount = .2)
  yc <- jitter(yc, amount = .2)
  while (cor(xc,yc) < .25 | cor(xc,yc) > .75) {
    xc <- sample(1:K, replace = F)
    yc <- sample(1:K, replace = F)
    xc <- jitter(xc, amount = .2)
    yc <- jitter(yc, amount = .2)
  }
  
  yc <- scale(yc)
  xc <- scale(xc)
  
  groups <- sample(K, N, replace = TRUE, 
                   prob = abs(rnorm(K, mean = 1/K, sd = 0.5 / K ^ 2)))
  
  yerr <- rnorm(N, sd = sd.cluster)
  xerr <- rnorm(N, sd = sd.cluster)
  
  cluster.data <- data.frame(x = xc[groups] + xerr, 
                             y = yc[groups] + yerr, 
                             group = groups)
  return(cluster.data)
}

# Simulate linear data: N points with (sd.trend)^2 error variance
sim.line <- function(K = 3, N = 45, sd.trend = .3){
  # Simulate data from line
  line.data <- data.frame(x = jitter(seq(-1, 1, length.out = N)), y = 0)
  line.data$y <- line.data$x + rnorm(N, 0, sd.trend)
  
  return(line.data)
}

# Simulate data which is a mixture between clusters and lines
# Calls sim.clusters and sim.line
mixture.sim <- function(lambda, K, N, sd.trend=.3, sd.cluster=.3){
  cluster.data <- sim.clusters(K = K, N = N, sd.cluster = sd.cluster)
  cluster.data[,c("x", "y")] <- scale(cluster.data[,c("x", "y")])
  line.data <- sim.line(K = K, N = N, sd.trend = sd.trend)
  line.data[,c("x", "y")] <- scale(line.data[,c("x", "y")])
  
  ll <- rbinom(n = N, size = 1, prob = lambda)  # one model or the other
  mix.data <- data.frame(
    x = ll*cluster.data$x + (1 - ll)*line.data$x,  
    y = ll*cluster.data$y + (1 - ll)*line.data$y,
    group = as.numeric(cluster.data$group)  
  )
  
  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  mix.data$group <- cutree(hclust(dist(mix.data[,c("x", "y")])), k = K) 
  # grouping by the best K clusters
  
  return(mix.data)
}

# Generate data for an entire lineup
gen.data <- function(input){
  # Position of target plots
  pos <- sample(1:20, size = 2)
  
  # Trend target data
  trenddata <- mixture.sim(lambda = 0, 
                           N = input$N, 
                           K = input$K, 
                           sd.trend = input$sd.trend, 
                           sd.cluster = input$sd.cluster)
  # Cluster target data
  clusterdata <- mixture.sim(lambda = 1, 
                             N = input$N, 
                             K = input$K, 
                             sd.trend = input$sd.trend, 
                             sd.cluster = input$sd.cluster)
  
  # Null data
  # Need 19 data sets so that nullabor works; then replace the data at 
  # cluster target position with cluster target data
  nulldata <- rdply(19, function(.sample) 
    mixture.sim(lambda = .5, 
                N = input$N, 
                K = input$K, 
                sd.cluster = input$sd.cluster, 
                sd.trend = input$sd.trend
    ))
  
  data <- lineup(true = trenddata, pos = pos[1], n = 20, samples = nulldata)
  data <- rbind.fill(
    subset(data, .sample != pos[2]), 
    cbind(.sample = pos[2], clusterdata))
  
  data$target1 <- pos[1]
  data$target2 <- pos[2]
  
  data
}

# Calculate cluster sum of squares
cluster <- function(dframe) {
  # we assume to have x, y, and a group variable
  xmean <- mean(dframe$x)
  ymean <- mean(dframe$y)
  dframe$dist <- with(dframe, (x - xmean) ^ 2 + (y - ymean) ^ 2)
  SSTotal <- sum(dframe$dist)
  dframe <- dframe %>% 
    group_by(group) %>% 
    mutate(xgroup = mean(x), ygroup = mean(y))
  dframe$gdist <- with(dframe, (x - xgroup) ^ 2 + (y - ygroup) ^ 2)
  SSGroup <- sum(dframe$gdist)
  (SSTotal - SSGroup)/SSTotal
}

# Calculate gini statistic
gini <- function(y, unbiased = TRUE, na.rm = FALSE){
  if ( (!is.numeric(y))) {
    warning("'y' is not numeric; returning NA")
    return(NA)
  }
  if (!na.rm && any(na.ind <- is.na(y)))
    stop("'x' contain NAs")
  if (na.rm)
    y <- y[!na.ind]
  x <- as.numeric(table(y))/sum(y)
  1 - sum(x ^ 2)
}

# Calculate trend/group/gini statistics for any given dataset
eval.df <- function(df){
  data.frame(
    line = summary(lm(y~x, data = df))$r.squared, 
    group = cluster(df),
    gini = gini(df$group)
  )
}

# Calculate statistics for each lineup
eval.data <- function(df){
  
  nulls <- subset(df, .sample != target1 & .sample != target2)
  groups <- subset(df, .sample == target2)
  lines <- subset(df, .sample == target1)
  
  nl <- ddply(nulls, .(.sample), eval.df)
  
  cl <- eval.df(groups)
  ll <- eval.df(lines)
  
  c(null.line = max(nl$line), 
    null.cluster = max(nl$group), 
    null.gini = min(nl$gini),
    line = ll$line, 
    cluster = cl$group,
    gini = cl$gini)
}

theme_lineup <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(legend.position = "none", 
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = unit(c(1,1,0,0), "line")
    )}

# --- Supplement A -------------------------------------------------------------
# Simulations of the parameter space

# Read simulation data
if (file.exists("SimulationDatasetCriteriaTurk16.csv")) {
  dataset.criteria <- read.csv("SimulationDatasetCriteriaTurk16.csv", stringsAsFactors = F)
} else {
  # Generate simulation results. This will take a while. 
  library(nullabor)
  library(compiler)
  library(doMC)
  registerDoMC(6)
  library(magrittr)
  library(dplyr)
  library(plyr) # required for parallel computation - dplyr functionality is missing
  
  # Simulation parameters
  data.parms <- expand.grid(K=c(3, 5),
                            sd.trend=round(seq(.2, .5, by=.05), 2),
                            sd.cluster=round(seq(.1, .4, by=.05), 2))
  data.parms$N <- data.parms$K*15
  
  
  tmp <- function(M=1000, N=45, K=3, sT=0.3, sC=0.3) {
    data.frame(t(replicate(M, {
      input.pars <- list(N=N, K=K, sd.trend=sT, sd.cluster=sC)
      c(unlist(input.pars), eval.data(gen.data(input.pars)))
    })))
  }
  nulldist <- cmpfun(tmp)
  
  # Use plyr because it is parallelizable
  simulation.results <- plyr::ldply(
    1:nrow(data.parms), 
    function(i) {
      with(data.parms[i,], 
           nulldist(M=1000, N=N, K=K, sT=sd.trend, sC=sd.cluster))
    }, 
    .parallel=T)
  names(simulation.results)[3:4] <- c("sd.trend", "sd.cluster")
  simulation.results$sd.trend <- round(simulation.results$sd.trend, 2)
  simulation.results$sd.cluster <- round(simulation.results$sd.cluster, 2)
  
  longres <- melt(simulation.results, id.vars=1:4, variable.name="type", value.name = "value")
  longres$dist <- c("Data", "Max(18 Nulls)")[1+grepl("null", longres$type)]
  longres$type <- gsub("null.", "", longres$type, fixed=T)
  
  # dataset.criteria <- ddply(longres, .(type, dist, sd.trend, sd.cluster, K), summarize, LB = quantile(value, .25), mean=mean(value), UB = quantile(value, .75))
  dataset.criteria <- longres %>% group_by(type, dist, sd.trend, sd.cluster, K) %>%
    summarize(
      LB = quantile(value, .25), 
      mean=mean(value), 
      UB = quantile(value, .75)
    )
  
  write.csv(dataset.criteria, file = "SimulationDatasetCriteriaTurk16.csv", row.names = F)
}

dataset.criteria$ParameterSet <- with(
  dataset.criteria,
  sprintf("sdT%.2f-sdC%.2f", sd.trend, sd.cluster))
dataset.criteria$ParameterSet[dataset.criteria$type == "cluster"] <- with(
  dataset.criteria[dataset.criteria$type == "cluster",],
  sprintf("sdC%.2f-sdT%.2f", sd.cluster, sd.trend))

dataset.criteria$lsc <- paste("sigma[C]: ", round(dataset.criteria$sd.cluster, 2))
dataset.criteria$lst <- paste("sigma[T]: ", round(dataset.criteria$sd.trend, 2))
dataset.criteria$lK <- paste("K: ", dataset.criteria$K)

### Figure 1 A ###
qplot(data = subset(dataset.criteria, type == "line"), x = LB, xend = UB, y = sd.cluster, yend = sd.cluster, color = dist, geom = "segment") +
  geom_point(aes(x = LB, y = sd.cluster, color = dist))  +
  geom_point(aes(x = UB, y = sd.cluster, color = dist)) +
  facet_grid(lst~lK, scales = "free", labeller = "label_parsed")  + theme_bw() +
  scale_color_brewer("Distribution",palette = "Set1") + theme(legend.position = "bottom") + xlab("Interquartile intervals of Max (18) null distribution (blue) \nand target distribution (red) of linearity measured in R squared.") + ylab(expression("Cluster variability":sigma[C]))

### Figure 1 B ###
qplot(data = subset(dataset.criteria, type == "cluster"), x = LB, xend = UB, y = sd.trend, yend = sd.trend, color = dist, geom = "segment") +
  geom_point(aes(x = LB, y = sd.trend, color = dist))  +
  geom_point(aes(x = UB, y = sd.trend, color = dist)) +
  facet_grid(lsc~lK, scales = "free", labeller = "label_parsed")  + theme_bw() +
  scale_color_brewer("Distribution", palette = "Set1") + theme(legend.position = "bottom") + xlab("Interquartile intervals of Max (18) null distribution (blue) \nand target distribution (red) of amount of clustering.") + ylab(expression( "Variability along the trend":sigma[T]))

# --- Supplement B -------------------------------------------------------------

### Figure 2 ###
# Warning - this takes a LONG time to run
set.seed(20140501)
K <- 10
res <- data.frame(rep = rep(1:10, each = K + 1))
res <- res %>% group_by(rep) %>% do(
  dVsim(x = 0:K, K = K, m = 20, scenario = 3, target = 1:2, N = 100000)
  )
res <- res %>% dplyr::ungroup() %>% dplyr::group_by(x)
means <- res %>%
  dplyr::summarise(avg = mean(scenario3))

qplot(x, scenario3, data = res, shape = I(21), alpha = I(0.8)) +
theme_bw() +
ylab("estimated probability") + xlab("Number of times (out of 10) one of the targets is picked") +
  geom_segment(aes(x = x - 0.3, xend = x + 0.3, y = avg, yend = avg), data = means) +
  geom_point(aes(x = x, y = binom), colour = "steelblue", pch = 4, size = 4) +
  scale_x_continuous(breaks = 0:K, labels = 0:K)

# --- Supplement C - Models ----------------------------------------------------

### Prepare modeldata data frame ###
modeldata <- read.csv("modeldata.csv", stringsAsFactors = F) %>%
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         k = factor(k, levels = c(3, 5)),
         plottype = factor(plottype, levels = c("plain", "trend", "color", 
                                                "shape", "colorShape", "colorEllipse", 
                                                "colorTrend",  "trendError", 
                                                "colorShapeEllipse", 
                                                "colorEllipseTrendError"))
  )

totaltime <- modeldata %>% group_by(individualID) %>% summarize(
  total.experiment.time = max(end_time) - min(start_time)
)

### Fit ALL the models!
modeldata$one.correct <- 1 - modeldata$neither.correct

gol <- glmer(
  one.correct ~ sdline_new + sdgroup_new + k_new +
    trial.num + plottype + (1 | data_name) + (1 | individualID),
  data = modeldata, family = binomial(),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 1e5)
  )
)

gol.fixef <- data.frame(confint(gol, method = "Wald"))[-(c(1:2,4:7)),] # exclude sigmas
names(gol.fixef) <- c("LB", "UB")
gol.fixef$OR <- fixef(gol)[-(2:5)]
gol.fixef[1,1:3] <- 0

type_compare <- glht(gol, mcp(plottype = "Tukey"))
gol.fixef$letters <- cld(type_compare)$mcletters$Letters

gol.fixef$label <- gsub("plottype", "", names(fixef(gol)))[-(2:5)]
gol.fixef$label <- gol.fixef$label %>%
  str_replace("color", "Color + ") %>%
  str_replace("[sS]hape", "Shape + ") %>%
  str_replace("[tT]rend", "Trend + ") %>%
  str_replace("Ellipse", "Ellipse + ") %>%
  str_replace("Error", "Error + ") %>%
  str_replace("\\(Intercept\\)", "Plain + ") %>%
  str_replace("( \\+ )$", "") %>%
  reorder(gol.fixef$OR)

sig <- anova(gol)

gol.2 <- update(gol, . ~ . + gini.min)
# large gini (i.e. homogeneity) leads to significant better probability
# of picking one of the targets

gol.2b <- update(gol.2, . ~ . + gini.min:plottype,
                 control = glmerControl(
                   optimizer = "bobyqa",
                   optCtrl = list(maxfun = 1e5)
                 ))
# individual designs are significantly different

modeldata$one.ellipse <- modeldata$no.ellipse == 1

gol.3 <- update(gol, . ~ . + one.ellipse)
# not significant, nor is the number of missing ellipses,
# nor the absence of at least one ellipse

gol.3b <- update(gol.3, . ~ . + one.ellipse:plottype)

gol.4 <- update(gol, . ~ . + max.range) # not significant

gol.4b <- update(gol, . ~ . + max.range:plottype) # not significant

gol.5 <- update(gol, . ~ . + log(as.numeric(trial.time)),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
                ))
# there is a (highly) significant effect of (log) response time on accuracy.
# The size of the effect is on average -0.405486 for each unit increase in
# (log) response time.

gol.5b <- update(gol, . ~ . + poly(log(as.numeric(trial.time)),2),
                 control = glmerControl(
                   optimizer = "bobyqa",
                   optCtrl = list(maxfun = 1e5)
                 ))

faceoff$one.ellipse <- faceoff$no.ellipse == 1

gvl.4 <-  glmer(cluster.correct ~ sdline_new + sdgroup_new +
                  k_new + plottype + (1 | data_name) + (1 | individualID),
                data = faceoff,
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa"))

### Table 2 ###
gvl.fixef <- data.frame(confint(gvl.4, method = "Wald"))[-(1:2),] # exclude sigmas
names(gvl.fixef) <- c("LB", "UB")
gvl.fixef$OR <- fixef(gvl.4)
gvl.fixef <- rbind(gvl.fixef, c(0,0,0))
row.names(gvl.fixef)[14] <- "plottypePlain"

gvl.all <- gvl.fixef
gvl.fixef <- gvl.fixef[c(14,5:13),]

type_compare <- glht(gvl.4, mcp(plottype = "Tukey"))
gvl.fixef$letters <- cld(type_compare)$mcletters$Letters

gvl.5 <-  update(gvl.4, . ~ . + first.trial + log(as.numeric(trial.time)),
                 control = glmerControl(optimizer = "bobyqa"))

gvl.6 <-  update(gvl.4, . ~ . + factor(conf_level),
                 control = glmerControl(optimizer = "bobyqa"))

gvl.7 <-  update(gvl.4, . ~ . + one.ellipse,
                 control = glmerControl(optimizer = "bobyqa"))

aov7 <- anova(gvl.4, gvl.7)

gvl.7b <-  update(gvl.7, . ~ . + one.ellipse:plottype,
                  control = glmerControl(optimizer = "bobyqa"))

aov7b <- anova(gvl.7, gvl.7b)

gvl.8 <-  update(gvl.4, . ~ . + gini.min,
                 control = glmerControl(optimizer = "bobyqa"))
# not significant, not even a little bit

gvl.9 <-  update(gvl.4, .~.+factor(no.ellipse),
                 control = glmerControl(optimizer = "bobyqa"))
# only 1 missing ellipse has significant negative effect! beautiful!

gols.aov.2 <- anova(gol, gol.2)
gols.aov.2b <- anova(gol.2, gol.2b)

gols.aov.5 <- anova(gol, gol.5)
gols.aov.5b <- anova(gol.5, gol.5b)

# need predictions from gol.2b
df2b <- data.frame(expand.grid(
  plottype = levels(modeldata$plottype),
  sdline_new = 0.25,
  sdgroup_new = 0.25,
  k_new = 3,
  trial.num = 5,
  gini.min = seq(0.5,1, by = 0.025)
))

# Model for response times
modeldata$start <- ymd_hms(modeldata$start_time)
modeldata$end <- ymd_hms(modeldata$end_time)
modeldata$time <- as.numeric(with(modeldata, end-start))
modeldata$k_new <- factor(modeldata$k_new)

time <- lmer(log(time) ~ first.trial + plottype +
               simpleoutcome + k_new + sdline_new + sdgroup_new +
               (1 | individualID) + (1 | dataset),
             data = modeldata)
#anova(time, time2)
# hugely significant dataset effect? I wouldn't have guessed that. It might be an effect of the large number of evaluations

modeldata$one.ellipse <- modeldata$no.ellipse == 1

time.2 <- update(time, . ~ . + trial.num) # highly significant
time.3 <- update(time.2, . ~ . + gini.min) # not significant
time.4 <- update(time.2, . ~ . + one.ellipse, data = modeldata) # not significant

# Model for participant confidence

conf <- lmer(data = modeldata, conf_level ~ first.trial + trial.num +
               log(as.numeric(trial.time)) + plottype + simpleoutcome +
               sdline_new + sdgroup_new + k_new +
               (1 | dataset) + (1 | ip_address))

pconf <- cftest(conf)
table <- data.frame(summary(conf)$coefficients)
# lmer reports t values and df. I would rather report the normal approximated values, because degrees of freedom are at least 50 and mostly at 10,000.
conf.2 <- update(conf, . ~ . + gini.min) # nope
conf.3 <- update(conf, . ~ . + one.ellipse) # nope
conf.4 <- update(conf, . ~ . - log(as.numeric(trial.time)) + poly(log(as.numeric(trial.time)), 2)) # yes, quadratic term is highly significant


# Face-off model comparisons:
aov <- anova(gvl.4, gvl.5)
aov2 <- anova(gvl.4, gvl.6)
aov.7 <- anova(gvl.4, gvl.7)
aov.7b <- anova(gvl.7, gvl.7b)
aov.8 <- anova(gvl.4, gvl.8) # gini
aov.9 <- anova(gvl.4, gvl.9) # factor(no.ellipse)

# --- Supplement C - Figures ---------------------------------------------------
### Figure 3 ###
cols <- brewer.pal(9, "Set1")
df2b <- merge(df2b, unique(modeldata[,c("plottype", "label")]), by = "plottype")

df2b$pred <- predict(gol.2, newdata = df2b, re.form = ~0, type = "response")
qplot(gini.min, pred, colour = label, shape = label,  data = df2b, size = I(2.5)) +
  theme_bw() +
  geom_line(aes(group = label)) +
  ylab("Predicted probablity for identifying one of the two targets") +
  xlab("Gini impurity (0 means no diversity, 1 means complete diversity)") +
  scale_colour_manual("", values = c(cols, "black")) +
  scale_shape_manual("", values = rep(1:6, 2))

### Figure 4 ###
qplot(time, data = modeldata, binwidth = .1) + theme_bw() +
  ylab("# Evaluations") + xlab("Log Response time (in seconds)") +
  scale_x_log10() +
  geom_vline(xintercept = median(modeldata$time), colour = "grey50")

### Table 3 ###
table <- data.frame(summary(time.2)$coefficients)
table$pvals <- cftest(time.2)$test$pvalues

### Table 4 ###
table <- table[,-c(3,5)]
table$pvalues <- pconf$test$pvalues

### Figure 5 ###
modeldata$simpleoutcome <- factor(modeldata$simpleoutcome, levels = c("trend", "both", "cluster", "neither"))
modeldata$label <- factor(modeldata$label, levels = gvl.fixef$label[order(gvl.fixef$OR)])
modeldata$st <- factor(sprintf("sigma[T]==%.2f", modeldata$sdline_new))
modeldata$sc <- factor(sprintf("sigma[C]==%.2f", modeldata$sdgroup_new))
modeldata$sc <- factor(modeldata$sc, levels = rev(levels(modeldata$sc)))
ggplot(data = subset(modeldata, k_new == 3)) +
  geom_bar(aes(x = label, fill = simpleoutcome), position = "fill") +
  facet_grid(st+sc~.,
             labeller = label_parsed) +
  scale_fill_manual("Outcome", values = c("darkorange", "grey40", "steelblue", "grey90")) + ylab("") + theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 330, hjust = 0, vjust = 1),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0))

### Figure 6 ###
ggplot(data = subset(modeldata, k_new == 5)) +
  geom_bar(aes(x = label, fill = simpleoutcome), position = "fill") +
  facet_grid(st+sc~.,
             labeller = label_parsed) +
  scale_fill_manual("Outcome", values = c("darkorange", "grey40", "steelblue", "grey90")) + ylab("") + theme_bw() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 330, hjust = 0, vjust = 1),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0))

### Figure 7 ###
df7 <- data.frame(expand.grid(
  plottype = levels(modeldata$plottype),
  sdline_new = 0.275,
  sdgroup_new = 0.25,
  k_new = 5,
  one.ellipse = c(TRUE, FALSE)
))

df7plus <- data.frame(expand.grid(
  data_name = unique(faceoff$data_name),
  plottype = levels(modeldata$plottype),
  sdline_new = 0.275,
  sdgroup_new = 0.25,
  k_new = 5,
  one.ellipse = c(TRUE, FALSE)
))
df7$pred4 <- predict(gvl.4, newdata = df7, re.form = ~0, type = "response")
df7$pred7 <- predict(gvl.7, newdata = df7, re.form = ~0, type = "response")
df7$pred7b <- predict(gvl.7b, newdata = df7, re.form = ~0, type = "response")
df7 <- merge(df7, unique(modeldata[,c("plottype", "label")]), by = "plottype")

rf7 <- data.frame(ranef(gvl.7b)$data_name)
rf7$data_name <- row.names(rf7)
names(rf7)[1] <- "rdata"
df7plus$pred7b <- predict(gvl.7b, newdata = df7plus, re.form = ~0, type = "link")
df7plus <- merge(df7plus, rf7, by = "data_name")
df7plus$predlink <- with(df7plus, pred7b + rdata)
df7plus$predprob <- with(df7plus, exp(predlink)/(1 + exp(predlink)) )
df7plus <- merge(df7plus, unique(modeldata[,c("plottype", "label")]), by = "plottype")

df7$plottype <- reorder(df7$plottype, df7$pred4)
qplot(pred4, label, data = df7, shape = I(3)) +
  geom_point(aes(x = pred7b, colour = one.ellipse, shape = one.ellipse), size = 3) +
  theme_bw() + ylab("") +
  theme(legend.position = "bottom") +
  scale_colour_manual("One ellipse missing", values = c("steelblue", "darkorange")) +
  scale_shape("One ellipse missing") +
  xlab("Probability to pick cluster target\n(given one of the targets was picked)")


