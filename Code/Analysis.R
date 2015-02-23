library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)

lineups <- read.csv("./Images/Lineups/picture-details.csv", stringsAsFactors=FALSE)
lineups$pic_id_old <- lineups$pic_id
lineups$pic_id <- 1:nrow(lineups)

userdata <- read.csv("./Data/turk16_results.csv", stringsAsFactors=FALSE)
userdata$response.id <- 1:nrow(userdata)
table(userdata$ip_address, userdata$nick_name)

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

useranswers.long <- melt(useranswers, id.vars=c(1, 4:22), value.vars=c("line.correct", "group.correct", "both.correct", "neither"), value.name="correct", variable.name="answer.type")
useranswers.long$answer.type <- gsub(".correct", "", useranswers.long$answer.type)
qplot(x=factor(correct), fill=factor(answer.type), geom="histogram", data=useranswers.long, position="dodge") + 
  facet_grid(k+sd.line~sd.cluster, labeller=label_both)

useranswers.long$plottype <- factor(useranswers.long$plottype, levels=c("plain", "trend", "color", "shape", "colorShape", "colorEllipse", "colorTrend",  "trendError", "colorShapeEllipse", "colorEllipseTrendError"))
useranswers.long$sd.cluster <- factor(useranswers.long$sd.cluster)
useranswers.long$sd.line <- factor(useranswers.long$sd.line)
useranswers.long$k <- factor(useranswers.long$k)

fixed.ef.model <- glm(correct~plottype+sd.cluster*sd.line+k, data=useranswers.long, family = binomial(link="logit"))
summary(fixed.ef.model)

participant.scores <- ddply(useranswers, .(nick_name, ip_address, sd.cluster), summarize, line.correct=mean(line.correct), group.correct=mean(group.correct))
participant.scores <- melt(participant.scores, id.vars=1:3, variable.name="type", value.name="percent.correct")
participant.scores$type <- gsub(".correct", "", participant.scores$type)
qplot(data=participant.scores, x=type, y=percent.correct, color=type, geom="violin") + facet_wrap(~sd.cluster)
