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
  c(line.correct=lineplot%in%x1, group.correct=groupplot%in%x1, n.answers=length(x1))
}

useranswers <- ddply(tmp, .(response.id), function(df) correct.ans(df$response_no, df$obs_plot_location))
useranswers <- merge(useranswers, tmp)

useranswers.long <- melt(useranswers, id.vars=c(1, 4:21), value.vars=c("line.correct", "group.correct"), value.name="correct", variable.name="answer.type")
useranswers.long$answer.type <- gsub(".correct", "", useranswers.long$answer.type)
qplot(x=factor(correct), fill=factor(answer.type), geom="histogram", data=useranswers.long, position="dodge") + 
  facet_grid(k+sd.line~sd.cluster, labeller=label_both)

