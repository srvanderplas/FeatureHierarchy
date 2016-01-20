load("../../Data/modeldata.Rdata")

library(lubridate)
modeldata$start <- ymd_hms(modeldata$start_time)
modeldata$end <- ymd_hms(modeldata$end_time)
modeldata$time <- as.numeric(with(modeldata, end-start))

library(lme4)
time <- lmer(log(time)~first.trial+simpleoutcome+k_new+sdline_new+sdgroup_new+(1|individualID)+(1|dataset), data=modeldata)

library(xtable)
xtable(summary(time)$coefficients)

