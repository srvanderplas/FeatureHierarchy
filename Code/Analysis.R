library(stringr)
library(plyr)
library(reshape2)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(doMC)
registerDoMC(8)
library(lubridate)
library(digest)

lineups <- read.csv("./Images/Turk16/data-picture-details-gini.csv", stringsAsFactors=FALSE)
lineups$pic_id_old <- lineups$pic_id
lineups$pic_id <- 1:nrow(lineups)



userdata <- read.csv("./Data/turk16_results.csv", stringsAsFactors=FALSE)

users <- read.csv("./Images/Turk16/turk16_users.csv", stringsAsFactors=F, header=F)
names(users) <- c("nick_name", "age", "gender", "education", "ip_address")

userlist <- unique(c(userdata$nick_name, users$nick_name))
iplist <- unique(c(userdata$ip_address, users$ip_address))

# table(userdata$ip_address, userdata$nick_name)
userdata$nick_name2 <- userdata$nick_name
userdata$ip_address2 <- userdata$ip_address
userdata$ip_address <- as.numeric(factor(userdata$ip_address2, levels=iplist))
userdata$nick_name <- as.numeric(factor(userdata$nick_name2, levels=userlist))

users$nick_name2 <- users$nick_name
users$ip_address2 <- users$ip_address
users$nick_name <- as.numeric(factor(users$nick_name2, levels=userlist))
users$ip_address <- as.numeric(factor(users$ip_address2, levels=iplist))

write.csv(userdata[,-which(names(userdata)%in%c("ip_address2", "nick_name2"))], "./Data/turk16_results_anon.csv", row.names=F)
write.csv(users[,-which(names(users)%in%c("ip_address2", "nick_name2"))], "./Data/turk16_users_anon.csv", row.names=F)


userdata <- read.csv("./Data/turk16_results.csv", stringsAsFactors=FALSE)

users <- read.csv("./Images/Turk16/turk16_users.csv", stringsAsFactors=F, header=F)
names(users) <- c("nick_name", "age", "gender", "education", "ip_address")
userdata$response.id <- 1:nrow(userdata)

users$age <- factor(users$age, levels=0:10, labels=c("NA", "<18", "18-25", "26-30", "31-35", "36-40", "41-45", "45-50", "51-55", "56-60", "61+"))
users$gender <- factor(users$gender, levels=0:2, labels=c("NA", "Male", "Female"))
users$education <- factor(users$education, levels=0:5, labels=c("NA", "High School or less", "Some college", "Bachelor's degree", "Some graduate school", "Graduate degree"))




tmp <- merge(userdata[!is.na(userdata$pic_id),], lineups[,c("pic_id", "sample_size", "test_param", "param_value", "p_value", "obs_plot_location")], all.x=T, all.y=F)
tmp$k <- as.numeric(substr(tmp$param_value, 3, 3))
tmp$sd.line <- as.numeric(substr(tmp$param_value, 12, 15))
tmp$sd.cluster <- as.numeric(substr(tmp$param_value, 25, 28))

correct.ans <- function(x,y){
  x1 <- as.numeric(str_trim(unlist(str_split(x, ","))))
  answers <- str_trim(unlist(str_split(y, ",")))
  lineplot <- as.numeric(answers[1])
  groupplot <- as.numeric(answers[2])
  giniplot <- ifelse(groupplot==as.numeric(answers[3]) | lineplot==as.numeric(answers[3]), NA, as.numeric(answers[3]))
  c(n.answers=length(x1), line.correct=lineplot%in%x1, group.correct=groupplot%in%x1, both.correct = lineplot%in%x1 & groupplot%in%x1, neither.correct=!(lineplot%in%x1 | groupplot%in%x1 | giniplot%in%x1), gini.correct=giniplot%in%x1)
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

modeldata <- useranswers[,c(1, 2, 8:26, 3:7)]
modeldata$outcome <- paste(c("", "line")[1+as.numeric(modeldata$line.correct==1)], c("", "group")[1+as.numeric(modeldata$group.correct==1)], sep="")
modeldata$outcome[modeldata$neither.correct==1] <- "neither"
modeldata$outcome[modeldata$both.correct==1] <- "both"
modeldata$outcome[modeldata$outcome==""] <- "gini"
modeldata$outcome <- factor(modeldata$outcome, levels=c("neither", "gini", "line", "group", "both"))

modeldata$outcome.gini <- paste(c("", "line")[1+as.numeric(modeldata$line.correct==1)], c("", "group")[1+as.numeric(modeldata$group.correct==1)], c("", "gini")[1+as.numeric(modeldata$gini.correct==1)], sep="")
modeldata$outcome.gini[modeldata$outcome.gini==""] <- "neither"


modeldata <- merge(modeldata, lineups[,c("pic_id", "data_name", "param_value")], all.x=T, all.y=T)
modeldata$dataset <- factor(str_extract(modeldata$data_name, "set-\\d{1,3}") %>% str_replace("set-", "") %>% as.numeric)
modeldata$individualID <- factor(sprintf("%s-%s", modeldata$ip_address, modeldata$nick_name))
modeldata$k <- factor(modeldata$k, levels=c(3, 5))
modeldata$parameter.value <- factor(gsub("set-\\d{1,3}-", "", modeldata$data_name))
modeldata$start_time <- ymd_hms(modeldata$start_time)
modeldata$end_time <- ymd_hms(modeldata$end_time)
modeldata$trial.time <- with(modeldata, end_time-start_time)


ggplot(data=modeldata, 
       aes(x=factor(conf_level), 
           group=factor(c("both", "neither", "group", "group", "line", "line", "neither")[as.numeric(factor(outcome))], 
                        levels=c("both", "group", "line", "neither")), 
           color=factor(c("both", "neither", "group", "group", "line", "line", "neither")[as.numeric(factor(outcome))], 
                        levels=c("both", "group", "line", "neither")))) + 
  geom_line(stat="bin", aes(y=..count..)) + 
  scale_color_discrete("Target\nIdentified")

ggplot(data=modeldata, aes(x=outcome, y=as.numeric(trial.time), fill=outcome)) + 
  geom_boxplot() + 
  scale_y_continuous(limits=c(0, 300)) + 
  coord_flip() + 
  scale_fill_discrete(guide="none")

ggplot(data=modeldata, aes(x=factor(conf_level), y=as.numeric(trial.time))) + 
  geom_jitter(aes(color=factor(conf_level)), shape=1, alpha=.5) + 
  geom_boxplot(fill='transparent') + 
  scale_y_continuous("Trial Time (seconds)", limits=c(0, 300)) + 
  coord_flip() + 
  scale_color_brewer(guide="none", palette="Set1") + 
  xlab("Participant Confidence") + 
  theme_bw()

ggplot(data=modeldata, aes(y=conf_level, x=as.numeric(trial.time)))  + geom_smooth(method="loess") + scale_x_continuous(limits=c(0, 300))

time.accuracy <- modeldata %>% group_by(plottype, dataset) %>% summarise(mean.trial.time = mean(trial.time), accuracy = 1-mean(neither.correct)) %>% arrange(mean.trial.time) %>% melt(id.var=c("plottype", "dataset", "mean.trial.time")) 

qplot(data=time.accuracy, x=as.numeric(mean.trial.time), y=value, color=plottype) + geom_smooth()+ xlim(c(20, 60)) + xlab("Mean Trial Time (by Plot)") + ylab("Accuracy") + facet_wrap(~plottype)


time.data <- melt(modeldata[,c("conf_level", "k", "sd.line", "sd.cluster", "line.diff", "cluster.diff", "plottype", "line.correct", "group.correct", "both.correct", "neither.correct", "gini.correct", "outcome", "dataset", "individualID", "trial.time")], id.var="trial.time", variable.name="variable", value.name="level")

qplot(data=time.data, x=as.numeric(trial.time), group=level, geom="density") + facet_wrap(~variable, scales="free_y") + scale_x_continuous(limits=c(0, 600))


library(lme4)

line.model <- glmer(line.correct~ plottype + 
#                       k + sd.line + sd.cluster + 
#                       sd.line:sd.cluster + 
                      (1|individualID) + 
                      (1|dataset), 
                    data=modeldata, family = binomial(link="logit"), 
                    control=glmerControl(optimizer="bobyqa"))

group.model <- glmer(group.correct~plottype + 
                       (1|individualID) + 
                       (1|dataset), 
                     data=modeldata, 
                     family = binomial(link="logit"), 
                     control=glmerControl(optimizer="bobyqa"))

# # Model Simulations
# library(multcomp)
# 
# N <- 500
# 
# ## Line model - simulations and CIs
# line.model.sim <- t(simulate(line.model, nsim=N))
# line.model.sim <- melt(line.model.sim)
# names(line.model.sim) <- c("i", "rowID", "line.correct")
# line.model.sim$i <- as.numeric(gsub("sim_", "", line.model.sim$i))
# # each iteration takes ~ 1.5 minutes
# line.res <- dlply(line.model.sim, .(i), function(x) model <- glmer(data=modeldata, x$line.correct[x$rowID]~plottype+(1|individualID)+(1|dataset), family = binomial(link="logit")), .parallel=TRUE)
# 
# save(line.res, "./Data/lineModelSim.RData")
# 
# line.ranefs <- ldply(line.res, function(x) data.frame(individual.sd=attr(VarCorr(x)$individualID, which="stddev"), dataset.sd=attr(VarCorr(x)$dataset, which="stddev")))
# line.ranef.indivCI <- ldply(line.ranefs[,2], function(x) quantile(x, probs=c(0.025, 0.975)))
# line.ranef.datasetCI <- ldply(line.ranefs[,3], function(x) quantile(x, probs=c(0.025, 0.975)))
# 
# line.sigma2 <- ldply(line.res, function(x) sigma(x))
# line.sigmaCI <- quantile(line.sigma2[,2], probs=c(0.025, 0.975))
# 
# line.fixefs <- ldply(line.res, fixef)
# 
# ## Group model - simulations and CIs
# group.model.sim <- simulate(group.model, nsim=N)
# group.res <- llply(group.model.sim, function(x) model <- glmer(data=modeldata, group.correct~plottype+(1|individualID)+(1|dataset), family = binomial(link="logit")), .parallel=TRUE)
# 
# save(group.res, "./Data/groupModelSim.RData")
# 
# group.ranefs <- ldply(group.res, function(x) data.frame(individual.sd=attr(VarCorr(x)$individualID, which="stddev"), dataset.sd=attr(VarCorr(x)$dataset, which="stddev")))
# group.ranef.indivCI <- ldply(group.ranefs[,2], function(x) quantile(x, probs=c(0.025, 0.975)))
# group.ranef.datasetCI <- ldply(group.ranefs[,3], function(x) quantile(x, probs=c(0.025, 0.975)))
# 
# group.sigma2 <- ldply(group.res, function(x) sigma(x))
# group.sigmaCI <- quantile(group.sigma2[,2], probs=c(0.025, 0.975))
# 
# group.fixefs <- ldply(group.res, fixef)
