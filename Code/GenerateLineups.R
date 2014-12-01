library(nullabor)
library(ggplot2)
library(plyr)

source("./Code/theme_lineup.R")
source("./Code/GenerateData.R")

set.seed(31415926)

# Linear trend alone - no outliers
N <- 10
seeds <- c(699181, 190785, 746705, 938857, 256246, 
           130991, 829750, 107019, 310685, 968407, 
           982098, 247471, 658815, 828038, 956567, 
           706934,  74904, 743373, 955470, 339582, 
           230370, 594429, 959280, 677468, 301141, 
           523820, 397938, 952596, 726989, 247199)
# seeds <- sample(0:1e6, N) + sample(0:1e3, N) + sample(0:100, N)
answers <- data.frame()

colors <- c("#882E72", "#D92120", "#4EB265", "#3F4EA1", 
            "#E7742F", "#1965B0", "#B1B343", "#4683C1", "#DFA53A")

names <- data.frame(filename = sample(paste("./Images/Lineups/", rep(1:3, each=N*2), "-", rep(1:(N*2), times=3), ".png", sep=""), 6*N), idx=1:(6*N), stringsAsFactors=FALSE)

permute.var <- c("x", "y")
for(i in 1:N){
  j <- 3*i-2
  set.seed(seeds[i])
  dframe <- linear.trend(n=30, sd.data=10)
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  filename <- names$filename[j]
  pv <- sample(permute.var, 1)
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y), size=3) + 
    facet_wrap(~.sample) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=NA, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Slope"))
  
  # Color alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+1]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(2,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Color"))
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+2]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,5,6)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="ShapeColor"))
}

# write.csv(answers, "./Images/Lineups/NoOutliersKey.csv", row.names=FALSE)

# Linear trend with Outliers
N <- 10
set.seed(271828)
seeds <- c(488462,  89012,  68718, 875098, 790630,
           995942, 307213, 635549, 380455, 971611,
            90250, 687530, 557990, 873571, 611075,
           100338, 214593, 365595, 609200, 944618,
           578770, 710917, 117886, 458621, 307274,
           749423, 231016, 321914, 222946, 627623)
# seeds <- sample(0:1e6, N) + sample(0:1e3, N) + sample(0:100, N)
# answers <- data.frame()

colors <- c("#882E72", "#D92120", "#4EB265", "#3F4EA1", 
            "#E7742F", "#1965B0", "#B1B343", "#4683C1", "#DFA53A")
permute.var <- c("x", "y")
for(i in 1:N){
  set.seed(seeds[i])
  j <- 28+3*i
  dframe <- linear.trend(n=50, n.outliers=4, sd=3)
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  filename <- names$filename[j]
  pv <- sample(permute.var, 1)
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y), size=3) + 
    facet_wrap(~.sample) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=NA, seed=seeds[i], idx=i, permute.var=pv, group="Outliers", type="Shape"))
  
  # Color alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+1]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,5,6)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="Outliers", type="Color"))
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+2]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=4, pos=pos.y)
  
  ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,5,6,1)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="Outliers", type="ShapeColor"))
}

write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)