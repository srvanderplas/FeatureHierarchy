load("../Data/modeldata.Rdata")
# make sure to only use k_new, sdgroup_new and sdline_new

library(lme4)
faceoff <- subset(modeldata, simpleoutcome != "neither")

gvl.model <-  glmer(cluster.correct~plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))
gvl.2 <-  glmer(cluster.correct~sdline_new + plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))
gvl.3 <-  glmer(cluster.correct~sdline_new + sdgroup_new  + plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))
gvl.4 <-  glmer(cluster.correct~sdline_new + sdgroup_new +k_new + plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))
# no improvement
gvl.4b <-  glmer(cluster.correct~factor(sdline_new) + factor(sdgroup_new) +k_new + plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))
# no improvement
gvl.4c <-  glmer(cluster.correct~factor(sdline_new) + k_new*factor(sdgroup_new) +k_new + plottype + (1|data_name) + (1|individualID), data=faceoff, family=binomial(), control=glmerControl(optimizer="bobyqa"))

anova(gvl.model, gvl.2, gvl.3, gvl.4)
anova(gvl.4, gvl.4b)

summary(gvl.4)
# keep gvl.4 for the moment

# what about an interaction between difficulty and plottype?
gvl.4d <- update(gvl.4, .~.+plottype:sdgroup_new+plottype:sdline_new) # not significant
gvl.4d <- update(gvl.4, .~.+plottype:sdgroup_new, control = glmerControl(
  optimizer="bobyqa",
  optCtrl = list(maxfun = 1e5)
)) 
gvl.4e <- update(gvl.4, .~.+plottype:sdline_new, control = glmerControl(
  optimizer="bobyqa",
  optCtrl = list(maxfun = 1e5)
)) 


gvl.fixef <- data.frame(confint(gvl.4, method="Wald"))[-(1:2),] # exclude sigmas
names(gvl.fixef) <- c("LB", "UB")
gvl.fixef$OR <- fixef(gvl.4)
gvl.fixef <- rbind(gvl.fixef, c(0,0,0))
row.names(gvl.fixef)[14] <- "plottypeplain"

gvl.all <- gvl.fixef

suppressMessages(require(multcomp))
type_compare <- glht(gvl.4, mcp(plottype="Tukey"))
gvl.fixef <- gvl.fixef[c(14,5:13),]
gvl.fixef$letters <- cld(type_compare)$mcletters$Letters
# not sure what to do about those warnings


gvl.fixef$label <- gsub("plottype", "", names(fixef(gvl.model)))
gvl.fixef$label <- gvl.fixef$label %>% 
  str_replace("color", "Color + ") %>% 
  str_replace("[sS]hape", "Shape + ") %>%
  str_replace("[tT]rend", "Trend + ") %>%
  str_replace("Ellipse", "Ellipse + ") %>%
  str_replace("Error", "Error + ") %>%
  str_replace("\\(Intercept\\)", "Plain + ") %>%
  str_replace("( \\+ )$", "") %>% 
  reorder(gvl.fixef$OR)

ggplot(data=gvl.fixef) + 
  geom_hline(yintercept=0, colour="gray70") + 
  geom_pointrange(aes(x=label, y=OR, ymin=LB, ymax=UB), size=.75) + 
  coord_flip() + 
  theme_bw() + 
  ggtitle("Odds of selecting Cluster over Trend Target") + 
  xlab("") +
  geom_text(aes(y=-1, x=label, label=letters)) + 
  scale_y_continuous(
    "Odds (on log scale) of selecting Cluster over Trend Target and 95% Wald Intervals\n(Reference level: Plain plot)", 
    breaks=c(-1, log(c(1/c(2, 1.75, 1.5,1.25), c(1,1.25,1.5, 1.75,2 ))), 1), labels=c("<--Trend\n  Target", "1/2  ","1/1.75", "1/1.5", "1/1.25","1","1.25","1.5", "1.75", "2", "Cluster-->\nTarget  "), limits=c(-1,1))