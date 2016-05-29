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

library(wordcloud) # plot wordclouds
library(tm) # text mining

# --- Important Functions ------------------------------------------------------
source("MixtureLineups.R") # Functions to simulate data, save plots, calculate statistics, etc.
source("theme_lineup.R") # Lineup theme for ggplot2

# --- Plots from Introduction --------------------------------------------------
### Figure 1 ###
x1 <- rnorm(25, mean = 4, sd = 0.5)
x2 <- rnorm(25, mean = 0, sd = 0.5)
y1 <- rnorm(25, mean = 1, sd = 0.5)
y2 <- rnorm(25, mean = 2, sd = 0.5)

qplot(c(x1,x2), c(y1,y2)) +
  theme_bw() + xlab("x") + ylab("y") +
  theme(axis.title = element_blank(), axis.text = element_blank())

x <- rnorm(60)
y <- rnorm(60)
group <- rep(1:4, length = 60)
qplot(x,y,
      color = factor(group %/% 2),
      shape = factor(group %/% 2)) +
  theme_bw() +
  xlab("x") +
  ylab("y") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

x1 <- runif(50, -.75, 1.25)
y1 <- x1 ^ 2 - x1 + rnorm(50, sd = 0.1)
x2 <- runif(25, -.75, 1.25)
y2 <- x2 + rnorm(25, sd = 0.1)

qplot(c(x1,x2),c(y1,y2)) +
  theme_bw() +
  xlab("x") +
  ylab("y")  +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
res <- data.frame(sd.trend = rep(c(0.1, 0.2, 0.3, 0.4), each = 45))
res <- res %>%
  group_by(sd.trend) %>%
  do(sim.line(N = 45, sd.trend = as.numeric(.[1,1])))

### Figure 2 ###
res$label <- paste("sigma[T] :",res$sd.trend)
qplot(x,y, data = res, pch = I(1)) +
  facet_grid(facets = .~label, labeller = "label_parsed") +
  theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(axis.title = element_blank()) +
  scale_x_continuous(breaks = c(-.75, 0, .75)) +
  scale_y_continuous(breaks = c(-1.5, -.75, 0, .75, 1.5))

### Figure 3 ###
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("color-perceptual-kernel.csv")
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("shape-perceptual-kernel.csv")
shapetm[9,] <- shapetm[,9] <- 0
shapetm[10,] <- shapetm[,10] <- 0


color3pal <- best.combo(3, colors, colortm)
color5pal <- best.combo(5, colors, colortm)
shape3pal <- best.combo(3, shapes, shapetm)
shape5pal <- best.combo(5, shapes, shapetm)

res <- data.frame(sd.cluster = rep(c(0.15, 0.20, 0.25, 0.30), each = 45))
res <- res %>%
  group_by(sd.cluster) %>%
  do(
    data.frame({
      set.seed(325098573);
      sim.clusters(K = 3, N = 45, sd.cluster = as.numeric(.[1,1]))
    })
  )

res$K <- 3
res$color <- color3pal[res$group]
res$shape <- shape3pal[res$group]

res2 <- data.frame(sd.cluster = rep(c(0.15, 0.20, 0.25, 0.30), each = 75))
res2 <- res2 %>%
  group_by(sd.cluster) %>%
  do({
    set.seed(325098573);
    sim.clusters(K = 5, N = 75, sd.cluster = as.numeric(.[1,1]))
    })

res2$K <- 5
res2$color <- color5pal[res2$group]
res2$shape <- shape5pal[res2$group]
res <- rbind(res, res2)
suppressMessages(library(ggplot2))
res$label <- paste("sigma[C] :",res$sd.cluster)
res$Klabel <- paste("K :", res$K)

ggplot(aes(x = x, y = y, color = color, shape = shape), data = res) +
  geom_point() +
  facet_grid(facets = Klabel~label, labeller = "label_parsed") + theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position = "none") +
  scale_shape_identity() +
  scale_color_identity() +
  theme(aspect.ratio = 1) +
  theme(axis.title = element_blank())

### Figure 4 ###
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

res <- data.frame(lambda = rep(c(0, .25, .5, .75, 1), each = 45))
res <- res %>%
  group_by(lambda) %>%
  do(
    data.frame({
      set.seed(325098573);
      mixture.sim(as.numeric(.[1,1]), K = 3, N = 45,
                  sd.trend = .25, sd.cluster = .25)
    })
  )

res$K <- 3
res$color <- color3pal[res$group]
res$shape <- shape3pal[res$group]

res2 <- data.frame(lambda = rep(c(0, .25, .5, .75, 1), each = 45))
res2 <- res2 %>%
  group_by(lambda) %>%
  do(
    data.frame({
      set.seed(325098573);
      mixture.sim(as.numeric(.[1,1]), K = 5, N = 45,
                  sd.trend = .25, sd.cluster = .2)
    })
  )

res2$K <- 5
res2$color <- color5pal[res2$group]
res2$shape <- shape5pal[res2$group]
res <- rbind(res, res2)
res$label <- paste("lambda :", res$lambda)
res$Klabel <- paste("K :", res$K)
ggplot(aes(x = x, y = y, color = color, shape = shape), data = res) +
  geom_point() +
  facet_grid(facets = Klabel~label, labeller = "label_parsed") +
  theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position = "none") +
  scale_shape_identity() +
  scale_color_identity() +
  theme(aspect.ratio = 1) +
  theme(axis.title = element_blank())


### Figure 5 ###
sT = 0.25
sC = 0.20
N = 45
K = 3
M = 1000

if (file.exists("SmallSimulation.Rdata")) {
  load("SmallSimulation.Rdata")
} else {
  # Compute simulation - this will take a while.
  nulldist <- function(N, sT = 0.25, sC = 0.2) {
    nulls <- data.frame(t(replicate(N, {
      lp <- data.frame(t(replicate(18, {
        mix = mixture.sim(lambda = 0.5, K = 3, N = 45,
                          sd.cluster = sC, sd.trend = sT)
        reg <- lm(y~x, data = mix)

        c(fline = summary(reg)$r.squared, fgroup = cluster(mix))
      })))
      c(fline = max(lp$fline), fgroup = max(lp$fgroup))
    })))

    trends <- replicate(10, {
      mix = mixture.sim(lambda = 0, K = 3, N = 45, sd.cluster = sC, sd.trend = sT)
      reg <- lm(y~x, data = mix)
      c(fline = summary(reg)$r.squared)
    })

    clusters <- replicate(10, {
      mix = mixture.sim(lambda = 1, K = 3, N = 45, sd.cluster = sC, sd.trend = sT)
      clust <- lm(y~factor(group) + 0, data = mix)
      res <- summary(aov(clust))
      c(fgroup = cluster(mix))
    })

    list(nulls = nulls, trends = trends, clusters = clusters)
  }

  res <- nulldist(N = N, sC = sC, sT = sT)

  tmp <- function(M = 1000, N = 45, K = 3, sT = 0.3, sC = 0.25) {
    data.frame(
      t(
        replicate(
          M,
          {
            input.pars <- list(N = N, K = K, sd.trend = sT, sd.cluster = sC)
            c(unlist(input.pars), eval.data(gen.data(input.pars)))
          }
        )
      )
    )
  }

  nulldist <- cmpfun(tmp)

  res <- nulldist(M = M, N = 45, K = 3, sT = sT, sC = sC)

  save(res, file = "SmallSimulation.Rdata")
}

longres <- melt(res, id.vars = 1:4, variable.name = "type", value.name = "value")
longres$dist <- c("Data", "Most Extreme of\n18 Null Dists")[1 + grepl("null", longres$type)]
longres$type <- gsub("null.", "", longres$type, fixed = T)
longres$Statistic <- longres$type
longres$Statistic[longres$type == "cluster"] <- "Cluster Measure"
longres$Statistic[longres$type == "line"] <- "R squared"
longres$Statistic[longres$type == "gini"] <- "Gini Impurity"
longres$Statistic <- factor(longres$Statistic, levels = c("R squared", "Cluster Measure", "Gini Impurity"))

ggplot(data = subset(longres, Statistic != "Gini Impurity")) +
  geom_area(aes(x = value, y = ..density.., color = dist, fill = dist),
            stat = "density", position = "identity",
            alpha = 0.5) +
  xlab("Simulated Distribution of Test Statistic") +
  ylab("Density") +
  facet_grid(. ~ Statistic,  labeller = label_both) +
  scale_color_manual("Distribution", values = c("black",  "gray")) +
  scale_fill_manual("Distribution", values = c("transparent", "gray")) +
  theme_bw() +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))

### Figure 6 - pre-generated pictures

### Figure 7 ###
colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
            "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
qplot(x = 1:10, y = 0, color = colors, size = I(5)) +
  scale_color_identity() +
  theme_lineup()
shapes <- c(1, 0, 3, 4, 8, 5, 2, 6,-0x25C1, -0x25B7)

qplot(x = 1:10, y = 0, shape = shapes, size = I(5)) +
  scale_shape_identity() +
  theme_lineup()

### Prepare modeldata data frame ###
load("modeldata.Rdata")

totaltime <- modeldata %>% group_by(individualID) %>% summarize(
  total.experiment.time = max(end_time) - min(start_time)
)

# --- Plots about participants and initial model data --------------------------

### Figure 8 ###
tmp <- users %>%
  group_by(age) %>%
  summarize(count = n()) %>%
  mutate(
    label = str_replace(as.character(age), "-", "-\\\n")
  )

ggplot(data = tmp) +
  geom_bar(aes(x = age, y = count), stat = "identity",
           fill = "grey70", color = "grey40") +
  geom_text(aes(x = age, y = count, label = label,
                vjust = as.numeric(count > 100) * 1.5 - .25),
            hjust = .5,  size = 3) +
  theme_bw() +
  scale_y_continuous("# Participants", limits = c(0, 350)) +
  xlab("Age of Participants") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), text = element_text(size = 10)) +
  ggtitle("Participant Age Distribution")

tmp <- users %>%
  group_by(education) %>%
  summarize(count = n()) %>%
  mutate(
    label = as.character(education) %>%
      str_replace_all(c("High School or less" = "<= High\\\nSchool",
                    "Some college" = "Some\\\ncollege",
                    "Bachelor's degree" = "Bachelor",
                    "Grad school or higher" = "Graduate"))
  )

ggplot(data = tmp) +
  geom_bar(aes(x = education, y = count), stat = "identity",
           fill = "grey70", color = "grey40") +
  geom_text(aes(x = education, y = count, label = label,
                hjust = as.numeric(count > 100)*1.5 - .25),
            vjust = .5,  size = 3, angle = 90) +
  theme_bw() +
  scale_y_continuous("# Participants") +
  xlab("Age of Participants") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), text = element_text(size = 10)) +
  ggtitle("Participant Education Levels")

### Figure 9 ###

user.data <- modeldata %>% group_by(individualID) %>%
  summarize(answers = length(individualID),
            cluster = sum(cluster.correct),
            trend = sum(trend.correct))
clusters <- as.data.frame(table(user.data$cluster))
trends <- as.data.frame(table(user.data$trend))
names(clusters) <- c("x", "Cluster")
clusters$Trend <- trends$Freq
clm <- melt(clusters, measure.var = 2:3)

ggplot() +
  geom_point(aes(x, value, colour = variable, shape = variable), size = 3, data = clm) +
  theme_bw() +
  scale_colour_brewer("Target", palette = "Set1") +
  scale_shape_discrete("Target") +
  ylab("# of participants") +
  xlab("# of target identifications (out of ten)") +
  geom_line(aes(x, value, colour = variable, group = variable), data = clm) +
  ggtitle("Target Identification by Participants") +
  theme(text = element_text(size = 10))

# --- Models -------------------------------------------------------------------

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

gol.2 <- update(gol, .~.+gini.min)
# large gini (i.e. homogeneity) leads to significant better probability
# of picking one of the targets

gol.2b <- update(gol.2, .~.+gini.min:plottype,
                 control = glmerControl(
                   optimizer = "bobyqa",
                   optCtrl = list(maxfun = 1e5)
                ))
# individual designs are significantly different

modeldata$one.ellipse <- modeldata$no.ellipse == 1

gol.3 <- update(gol, .~.+one.ellipse)
# not significant, nor is the number of missing ellipses,
# nor the absence of at least one ellipse

gol.3b <- update(gol.3, .~.+one.ellipse:plottype)

gol.4 <- update(gol, .~.+max.range) # not significant

gol.4b <- update(gol, .~.+max.range:plottype) # not significant

gol.5 <- update(gol, .~.+log(as.numeric(trial.time)),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 2e5)
                ))
# there is a (highly) significant effect of (log) response time on accuracy.
# The size of the effect is on average -0.405486 for each unit increase in
# (log) response time.

gol.5b <- update(gol, .~. + poly(log(as.numeric(trial.time)),2),
                control = glmerControl(
                  optimizer = "bobyqa",
                  optCtrl = list(maxfun = 1e5)
                ))

gvl.4 <-  glmer(cluster.correct~sdline_new + sdgroup_new +
                  k_new + plottype + (1 | data_name) + (1 | individualID),
                data = faceoff,
                family = binomial(),
                control = glmerControl(optimizer = "bobyqa"))

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

# --- Face off model pictures --------------------------------------------------

### Figure 10 ###
md <- data.frame(plottype = levels(modeldata$plottype),
                 tendency = c("none", "trend", "cluster", "cluster",
                              "cluster", "cluster", "conflict",
                              "trend", "cluster", "conflict"))

modeldata$label <- modeldata$plottype
modeldata$label <- modeldata$plottype %>%
  str_replace("color", "Color + ") %>%
  str_replace("[sS]hape", "Shape + ") %>%
  str_replace("[tT]rend", "Trend + ") %>%
  str_replace("Ellipse", "Ellipse + ") %>%
  str_replace("Error", "Error + ") %>%
  str_replace("plain", "Plain") %>%
  str_replace("( \\+ )$", "") %>%
  str_replace_all(c("Color" = "C", "Shape" = "S", "Ellipse" = "L",
                    "Trend" = "T", "Error" = "E")) %>%
  str_replace_all(" \\+ ", "+")

faceoff <- subset(modeldata, trend.correct | cluster.correct)

fac.order <- levels(with(faceoff, reorder(label,label,function(x) -length(x))))
modeldata$label <- factor(modeldata$label, levels = fac.order)

totals <- modeldata %>%
  group_by(label, plottype) %>%
  summarize(eval = length(label), type = "Total")

correct.totals <- faceoff %>%
  group_by(label, plottype) %>%
  summarize(eval = length(label), type = "Correct")

sub.totals <- modeldata %>%
  group_by(label, plottype, simpleoutcome) %>%
  summarize(eval = length(label))
sub.totals <- merge(sub.totals, md, by = "plottype")

totals <- rbind(totals, correct.totals)
totals <- merge(totals, md, by = "plottype")
totals$tendency <- factor(totals$tendency,
                          levels = c("none", "trend", "cluster", "conflict"))
totals$label <- factor(totals$label, levels = fac.order, ordered = T)

md <- merge(md, subset(totals, type == "Correct")[,c("plottype", "label", "eval")],
            by = "plottype")
md <- md[order(md$tendency, md$eval),]

totals$label <- factor(totals$label, levels = md$label)

totals$nlabel = as.numeric(totals$label)
sub.totals$nlabel = as.numeric(factor(sub.totals$label, levels = md$label))
sub.totals$simpleoutcome <- factor(
  sub.totals$simpleoutcome,
  levels = c("cluster", "both", "trend", "neither"))
sub.totals$tendency <- factor(sub.totals$tendency,
                              levels = c("none", "trend", "cluster", "conflict"))
ggplot() +
  geom_bar(aes(x = label, fill = simpleoutcome, weight = eval), data = sub.totals,
           colour = "grey40") +
  facet_grid(.~tendency, space = "free", scales = "free") +
  xlab("") + ylab("# of target evaluations") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 1), axis.title.x = element_blank(), legend.position = "bottom", text = element_text(size = 10)) +
  scale_fill_manual("Outcome", values = c("grey20", "grey30", "grey20", "grey90"),
                    guide = guide_legend(reverse = TRUE))


### Figure 11 ###

faceoff$one.ellipse <- faceoff$no.ellipse == 1

gvl.fixef$label <- gsub("plottype", "", row.names(gvl.fixef))
gvl.fixef$label <- gvl.fixef$label %>%
  str_replace("color", "Color + ") %>%
  str_replace("[sS]hape", "Shape + ") %>%
  str_replace("[tT]rend", "Trend + ") %>%
  str_replace("Ellipse", "Ellipse + ") %>%
  str_replace("Error", "Error + ") %>%
  str_replace("\\(Intercept\\)", "Plain + ") %>%
  str_replace("( \\+ )$", "") %>%
  reorder(gvl.fixef$OR)

ggplot(data = gvl.fixef) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_pointrange(aes(x = label, y = OR, ymin = LB, ymax = UB), size = .25) +
  coord_flip() +
  theme_bw() +
  ggtitle("Odds of selecting Cluster over Trend Target") +
  xlab("") +
  geom_text(aes(y = -1, x = label, label = letters)) +
  scale_y_continuous(
    "Odds (on log scale) with 95% Wald Intervals\n(Reference level: Plain plot)",
    breaks = c(-1, log(c(1/c(2, 1.5), c(1,1.5, 2 ))), 1), labels = c("  <--Trend\n      Target", "1/2", "1/1.5" ,"1", "1.5", "2", "Cluster-->  \nTarget      "), limits = c(-1,1))

### Figure 12 ### - Wordclouds
lexicaldata <- modeldata
lexicaldata$choice_reason <- tolower(lexicaldata$choice_reason)
lexicaldata$choice_reason <- gsub("^_", "", lexicaldata$choice_reason)

words <- lexicaldata %>% group_by(plottype, simpleoutcome) %>%
  summarize(list = paste(choice_reason, collapse = " ")) %>% filter(simpleoutcome != "both", plottype %in% c("plain", "color", "trend", "colorEllipse"))

quantities <- c("all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only" )
stops <- setdiff(stopwords("english"), quantities)

for (i in 1:nrow(words)) {
  words.corpus <- Corpus(DataframeSource(data.frame(words$list[i])))
  words.corpus <- tm_map(words.corpus, removePunctuation)
  words.corpus <- tm_map(words.corpus, function(x) removeWords(x, stops))

  corpus.temp <- tm_map(words.corpus, stemDocument, language = "english")
  # stemCompletion doesn't seem to work, I am essentially following along that function
  stems <- unlist(strsplit(corpus.temp[[1]]$content, split = " "))
  complete <- unlist(strsplit(words.corpus[[1]]$content, split = " "))
  dframe <- data.frame(stems, complete)
  dframe <- dframe %>% group_by(stems) %>% mutate(mode = names(sort(table(complete), decreasing = TRUE))[1])
  corpus.final <- corpus.temp
  corpus.final[[1]]$content <- paste(dframe$mode, collapse = " ")

  tdm <- TermDocumentMatrix(corpus.final)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(word = names(v),freq = v)
  pal <- brewer.pal(9, "BuGn")
  pal <- pal[-(1:2)]
  wordcloud(d$word,d$freq, scale = c(8,.3),min.freq = 2,max.words = 100, random.order = T, rot.per = .15, colors = "black", vfont = c("sans serif","plain"))
}
