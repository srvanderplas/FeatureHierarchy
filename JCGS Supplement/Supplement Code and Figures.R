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
source("MixtureLineups.R") # Functions to simulate data, save plots, calculate statistics, etc.
source("theme_lineup.R") # Lineup theme for ggplot2

# --- Supplement A -------------------------------------------------------------
# Simulations of the parameter space

# Load simulation data
load("SimulationDatasetCriteriaTurk16.Rdata")

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
load("modeldata.Rdata")

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

