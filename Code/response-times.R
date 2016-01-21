load("../../Data/modeldata.Rdata")

library(lubridate)
modeldata$start <- ymd_hms(modeldata$start_time)
modeldata$end <- ymd_hms(modeldata$end_time)
modeldata$time <- as.numeric(with(modeldata, end-start))
modeldata$k_new <- factor(modeldata$k_new)

library(lme4)
time <- lmer(log(time)~first.trial+plottype + simpleoutcome+k_new+sdline_new+sdgroup_new+(1|individualID)+(1|dataset), data=modeldata)
time2 <- update(time, .~.-(1|dataset), data=modeldata)

anova(time, time2) # hugely significant dataset effect? I wouldn't have guessed that. It might be an effect of the large number of evaluations

library(xtable)
library(multcomp)

table <- data.frame(summary(time)$coefficients)

table$pvals <- cftest(time)$test$pvalues
xtable(table)

type_comp <- glht(time, linfct=mcp(plottype = "Tukey"))
ptypes <- table[3:11,]
ptypes <- rbind(c(0,NA,NA,NA), ptypes)
row.names(ptypes)[1] <- "plottypeplain"

ptypes$letters <- cld(type_comp)$mcletters$Letters
ptypes$parameter <- gsub("plottype","",row.names(ptypes))
library(ggplot2)
ptypes$parameter <- with(ptypes, reorder(parameter, Estimate))

# idx <- grep("^b", ptypes$letters)
# ptypes$labels[idx] <- gsub("^b","  b", ptypes$letters)[idx]
# idx <- grep("^c", ptypes$letters)
# ptypes$labels[idx] <- gsub("^c","    c", ptypes$letters)[idx]
# idx <- grep("^d", ptypes$letters)
# ptypes$labels[idx] <- gsub("^d","      d", ptypes$letters)[idx]
# idx <- grep("^e", ptypes$letters)
# ptypes$labels[idx] <- gsub("^e","        e", ptypes$letters)[idx]
# idx <- grep("bd", ptypes$letters)
# ptypes$labels[idx] <- gsub("bd","  b  d", ptypes$letters)[idx]
qplot( Estimate, parameter, data=ptypes, size=I(2.5)) + theme_bw() + xlim(c(-0.05, 0.3)) +
  geom_text(aes(x= -0.05, y=parameter, label=letters), hjust=0) +
  geom_segment(aes(x=Estimate-1.96*`Std..Error`, xend=Estimate+1.96*`Std..Error`,
                   y=parameter, yend=parameter))

