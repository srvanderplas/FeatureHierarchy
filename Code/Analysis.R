library(stringr)
library(plyr)
library(reshape2)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(doMC)
registerDoMC(8)

lineups <- read.csv("./Images/Turk16/data-picture-details.csv", stringsAsFactors=FALSE)
lineups$pic_id_old <- lineups$pic_id
lineups$pic_id <- 1:nrow(lineups)

userdata <- read.csv("./Data/turk16_results.csv", stringsAsFactors=FALSE)
userdata$response.id <- 1:nrow(userdata)
# table(userdata$ip_address, userdata$nick_name)

tmp <- merge(userdata[!is.na(userdata$pic_id),], lineups[,c("pic_id", "sample_size", "test_param", "param_value", "p_value", "obs_plot_location")], all.x=T, all.y=F)
tmp$k <- as.numeric(substr(tmp$param_value, 3, 3))
tmp$sd.line <- as.numeric(substr(tmp$param_value, 12, 15))
tmp$sd.cluster <- as.numeric(substr(tmp$param_value, 25, 28))

correct.ans <- function(x,y){
  x1 <- as.numeric(str_trim(unlist(str_split(x, ","))))
  answers <- str_trim(unlist(str_split(y, ",")))
  lineplot <- as.numeric(answers[1])
  groupplot <- as.numeric(answers[2])
  c(line.correct=lineplot%in%x1, group.correct=groupplot%in%x1, n.answers=length(x1), both.correct = lineplot%in%x1 & groupplot%in%x1, neither=!(lineplot%in%x1 | groupplot%in%x1))
}

useranswers <- ddply(tmp, .(response.id), function(df) correct.ans(df$response_no, df$obs_plot_location))
useranswers <- merge(useranswers, tmp)
useranswers$plottype <- gsub("turk16-", "", useranswers$test_param)
useranswers$plottype <- factor(useranswers$plottype, levels=c("plain", "trend", "color", "shape", "colorShape", "colorEllipse", "colorTrend",  "trendError", "colorShapeEllipse", "colorEllipseTrendError"))
useranswers$sd.cluster <- factor(useranswers$sd.cluster)
useranswers$sd.line <- factor(useranswers$sd.line)
useranswers$k <- factor(useranswers$k)
useranswers <- ddply(useranswers, .(param_value, test_param), transform, param_idx=as.numeric(factor(pic_id)))

# # User answer tendencies...
# user.stats <- dcast(subset(useranswers, !grepl(",", response_no)), nick_name~response_no, fun.aggregate=length)
# subset(user.stats, rowSums(user.stats[,3:22]>0)<4& rowSums(user.stats[,3:22]>6))
# 
# 
# useranswers.long <- melt(useranswers, id.vars=c(1, 4, 7:25), value.vars=c("line.correct", "group.correct", "both.correct", "neither"), value.name="correct", variable.name="answer.type")
# useranswers.long$answer.type <- gsub(".correct", "", useranswers.long$answer.type)
# qplot(x=factor(answer.type), fill=factor(answer.type), weight=correct, geom="histogram", data=useranswers.long, position="dodge") + 
#   facet_grid(k+sd.line~sd.cluster, labeller=label_both)
# 
# 
# fixed.ef.model <- glm(correct~plottype+sd.cluster*sd.line+k, data=subset(useranswers.long, answer.type=="group"), family = binomial(link="logit"))
# summary(fixed.ef.model)
# 
# participant.scores <- ddply(useranswers, .(nick_name, ip_address, sd.cluster), summarize, line.correct=mean(line.correct), group.correct=mean(group.correct))
# participant.scores <- melt(participant.scores, id.vars=1:3, variable.name="type", value.name="percent.correct")
# participant.scores$type <- gsub(".correct", "", participant.scores$type)
# qplot(data=participant.scores, x=type, y=percent.correct, color=type, geom="violin") + facet_wrap(~sd.cluster)
# 
# 
# dataset.answers <- ddply(useranswers, .(param_value, param_idx), summarize, mean.line.correct=mean(line.correct), mean.group.correct=mean(group.correct))
# plot.answers <- ddply(useranswers, .(pic_id, test_param, param_value, p_value, plottype, param_idx), summarize, line.correct=mean(line.correct), group.correct=mean(group.correct))
# plot.answers <- merge(plot.answers, dataset.answers)
# plot.answers$plottype.fac <- as.character(as.numeric(factor(plot.answers$plottype))-1)
# 
# qplot(data=plot.answers, x=line.correct, y=group.correct, color=factor(param_idx), shape=plottype.fac, geom="point", size=I(10)) + facet_wrap(~param_value) + scale_shape_manual(guide="legend", values=as.character(0:9), labels=levels(plot.answers$plottype)) 
# 
# qplot(data=plot.answers, x=line.correct-mean.line.correct, y=group.correct-mean.group.correct, color=factor(param_idx), shape=plottype.fac, geom="point", size=I(10)) + facet_wrap(~param_value) + scale_shape_manual(guide="legend", values=as.character(0:9), labels=levels(plot.answers$plottype)) 
# 
# head(plot.answers)
# 
# plain.color <- subset(plot.answers, plottype%in%c("plain", "color"))
# qplot(data=plain.color, x=line.correct-mean.line.correct, y=group.correct-mean.group.correct, color=factor(param_idx), shape=plottype.fac, geom="point", size=I(10)) + facet_wrap(~param_value) + scale_shape_manual(guide="legend", values=c("x", "c"), labels=c("plain", "color"))

modeldata <- useranswers[,c(1, 4, 7:25, 2, 3, 5, 6)]
modeldata$outcome <- paste(c("", "line")[1+as.numeric(modeldata$line.correct==1)], c("", "group")[1+as.numeric(modeldata$group.correct==1)], c("", "neither")[1+as.numeric(modeldata$neither==1)], sep="")
modeldata$outcome[modeldata$both.correct==1] <- "both"

modeldata <- merge(modeldata, lineups[,c("pic_id", "data_name", "param_value")], all.x=T, all.y=T)
modeldata$dataset <- factor(str_extract(modeldata$data_name, "set-\\d{1,3}") %>% str_replace("set-", "") %>% as.numeric)
modeldata$individualID <- factor(sprintf("%s-%s", modeldata$ip_address, modeldata$nick_name))

library(lme4)

line.model <- glmer(line.correct~ plottype + (1|individualID) + (1|dataset), data=modeldata, family = binomial(link="logit"))

group.model <- glmer(group.correct~plottype + (1|individualID) + (1|dataset), data=modeldata, family = binomial(link="logit"))

library(multcomp)

N <- 1000

## Line model - simulations and CIs
line.model.sim <- simulate(line.model, nsim=N)
line.res <- llply(line.model.sim, function(x) line.model <- glmer(data=modeldata, line.correct~plottype+(1|individualID)+(1|dataset), family = binomial(link="logit")), .parallel=TRUE)

save(line.res, "./Data/lineModelSim.RData")

line.ranefs <- ldply(line.res, function(x) data.frame(individual.sd=attr(VarCorr(x)$individualID, which="stddev"), dataset.sd=attr(VarCorr(x)$dataset, which="stddev")))
line.ranef.indivCI <- ldply(line.ranefs[,2], function(x) quantile(x, probs=c(0.025, 0.975)))
line.ranef.datasetCI <- ldply(line.ranefs[,3], function(x) quantile(x, probs=c(0.025, 0.975)))

line.sigma2 <- ldply(line.res, function(x) sigma(x))
line.sigmaCI <- quantile(line.sigma2[,2], probs=c(0.025, 0.975))

line.fixefs <- ldply(line.res, fixef)

## Group model - simulations and CIs
group.model.sim <- simulate(group.model, nsim=N)
group.res <- llply(group.model.sim, function(x) group.model <- glmer(data=modeldata, group.correct~plottype+(1|individualID)+(1|dataset), family = binomial(link="logit")), .parallel=TRUE)

save(group.res, "./Data/groupModelSim.RData")

group.ranefs <- ldply(group.res, function(x) data.frame(individual.sd=attr(VarCorr(x)$individualID, which="stddev"), dataset.sd=attr(VarCorr(x)$dataset, which="stddev")))
group.ranef.indivCI <- ldply(group.ranefs[,2], function(x) quantile(x, probs=c(0.025, 0.975)))
group.ranef.datasetCI <- ldply(group.ranefs[,3], function(x) quantile(x, probs=c(0.025, 0.975)))

group.sigma2 <- ldply(group.res, function(x) sigma(x))
group.sigmaCI <- quantile(group.sigma2[,2], probs=c(0.025, 0.975))

group.fixefs <- ldply(group.res, fixef)
