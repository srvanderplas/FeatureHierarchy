load(file="./Images/Lineups/Lineups.rda")
eval.data <- data.summary %>% 
  group_by(name, plot.idx, type, Type1, Type2, group, ngroups, n) %>%
  do(data.frame(ans1 = .$.sample[order(.$slope.r2, decreasing = T)[1]],
                ans2 = .$.sample[order(.$group.r2, decreasing = T)[1]],
                runnerup1 = .$.sample[order(.$slope.r2, decreasing = T)[2]],
                runnerup2 = .$.sample[order(.$group.r2, decreasing = T)[2]],
                target1 = unique(.$target1),
                target2 = unique(.$target2)))

qplot(data=data.summary, x=jitter(as.numeric(as.factor(group))), y=slope.r2, color=factor(target1==.sample), alpha=I(.5)) + 
  facet_grid(Type1~Type2, labeller=label_both, scales="free") + scale_colour_discrete(guide="none")
qplot(data=data.summary, x=jitter(as.numeric(as.factor(group))), y=group.r2, color=factor(target2==.sample), alpha=I(.5)) + 
  facet_grid(Type1~Type2, labeller=label_both, scales="free") + scale_colour_discrete(guide="none")
with(eval.data, table(ans1, target1, type))

qplot(data=eval.data, x=factor(ans1), y=factor(target1), geom="tile", stat="bin2d", fill=..count..)
ggplot(data=subset(eval.data, Type1!=""), aes(x=factor(ans1), y=factor(target1))) + 
  stat_bin2d(aes(fill=..count..), geom="tile", binwidth=c(1,1), position="identity") + 
  facet_wrap(~type) + 
  ggtitle("Answer Matching using R^2") + 
  xlab("R^2 predicted answer") + ylab("Target answer")
ggplot(data=subset(eval.data, Type2!=""), aes(x=factor(ans2), y=factor(target2))) + 
  stat_bin2d(aes(fill=..count..), geom="tile", binwidth=c(1,1), position="identity") + 
  facet_wrap(~type) + 
  ggtitle("Answer Matching using R^2") + 
  xlab("R^2 predicted answer") + ylab("Target answer")

ggplot(data=subset(eval.data, Type1!=""), aes(x=factor(ans1), y=factor(target1))) + 
  stat_bin2d(aes(fill=..count..), geom="tile", binwidth=c(1,1), position="identity") + 
  facet_wrap(~group) + 
  ggtitle("Answer Matching using R^2") + 
  xlab("R^2 predicted answer") + ylab("Target answer")
ggplot(data=subset(eval.data, Type2!=""), aes(x=factor(ans2), y=factor(target2))) + 
  stat_bin2d(aes(fill=..count..), geom="tile", binwidth=c(1,1), position="identity") + 
  facet_wrap(~group) + 
  ggtitle("Answer Matching using R^2") + 
  xlab("R^2 predicted answer") + ylab("Target answer")