library(nullabor)
library(ggplot2)
library(plyr)

source("./Code/theme_lineup.R")
source("./Code/GenerateData.R")

set.seed(31415926)

# Linear trend alone - no outliers
N <- 5
seeds <- c(699181, 190785, 746705, 938857, 256246, 
           130991, 829750, 107019, 310685, 968407, 
           982098, 247471, 658815, 828038, 956567, 
           706934,  74904, 743373, 955470, 339582, 
           230370, 594429, 959280, 677468, 301141, 
           523820, 397938, 952596, 726989, 247199,
           466865, 221146, 717741, 788710, 751081,
           549865, 866498, 941755, 930329, 718688)
# seeds <- sample(0:1e6, N) + sample(0:1e3, N) + sample(0:100, N)
answers <- data.frame()
plots <- rep(NA, 2*7*N)

# colors <- c("#882E72", "#D92120", "#4EB265", "#3F4EA1", 
#             "#E7742F", "#1965B0", "#B1B343", "#4683C1", "#DFA53A")
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

idx <- 1:(N*7*2)
names <- paste0("./Images/Lineups/", rep(1:N, each=7*2), "-", rep(1:(7*2), times=N), ".png")
names <- data.frame(filename = sample(names, 7*N*2), idx=1:(7*N*2), stringsAsFactors=FALSE)

permute.var <- c("x", "y")
for(i in 1:N){
  j <- 7*i-6
  set.seed(seeds[i])
  dframe <- linear.trend(n=30, sd.data=10)
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  filename <- names$filename[j]
  pv <- sample(permute.var, 1)
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j] <- 
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
  plots[j+1] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,7)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Color"))
  
  # Shape alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+2]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j+2] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(5,10)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Shape"))

  # Shape+Color (redundant) alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+3]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j+3] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(5,10)]) +
    scale_color_manual(values=colors[c(3,7)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="ColorShape"))
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+4]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  plots[j+4] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeColor"))
  
  # Slope vs. Shape
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+5]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  plots[j+5] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1, 3, 10)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeShape"))
  
  # Slope vs. Color+Shape (redundant)
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+6]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  
  plots[j+6] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group.k), color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1, 3, 10)]) +
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeColorShape"))
}

# write.csv(answers, "./Images/Lineups/NoOutliersKey.csv", row.names=FALSE)

# Linear trend with Outliers
N <- 5
set.seed(271828)
seeds <- c(488462,  89012,  68718, 875098, 790630,
           995942, 307213, 635549, 380455, 971611,
            90250, 687530, 557990, 873571, 611075,
           100338, 214593, 365595, 609200, 944618,
           578770, 710917, 117886, 458621, 307274,
           749423, 231016, 321914, 222946, 627623,
            88102, 962776, 699940, 353661, 976897)
# seeds <- sample(0:1e6, N) + sample(0:1e3, N) + sample(0:100, N)
# answers <- data.frame()

permute.var <- c("x", "y")
for(i in 1:N){
  set.seed(seeds[i])
  j <- (N*7)-6+7*i
  dframe <- linear.trend(n=50, n.outliers=4, sd=3)
  
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  filename <- names$filename[j]
  pv <- sample(permute.var, 1)
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j] <- 
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
  plots[j+1] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Color"))
  
  # Shape alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+2]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j+2] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1,3,10)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="Shape"))
  
  # Shape+Color (redundant) alone
  pos.x <- sample(1:20, 1)
  pv <- "group"
  filename <- names$filename[j+3]
  lineupdata <- lineup(null_permute(pv), dframe, pos=pos.x)
  plots[j+3] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1,3,10)]) +
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=NA, target2=pos.x, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="ColorShape"))
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+4]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  plots[j+4] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeColor"))
  
  # Slope vs. Shape
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+5]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  plots[j+5] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1, 3, 10)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeShape"))
  
  # Slope vs. Color+Shape (redundant)
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  pv <- sample(permute.var, 1)
  filename <- names$filename[j+6]
  lineupdata <- permute.groups2(lineup(null_permute(pv), dframe, pos=pos.x), 
                                ngroups=3, pos=pos.y)
  plots[j+6] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group.k), color=factor(group.k)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shapes[c(1, 3, 10)]) +
    scale_color_manual(values=colors[c(3,4,8)]) +
    theme_lineup()
  ggsave(filename, width=10, height=8, dpi=300, units="in")
  answers <- rbind(answers, data.frame(name=filename, target1=pos.x, target2=pos.y, seed=seeds[i], idx=i, permute.var=pv, group="NoOutliers", type="SlopeColorShape"))
}

save(plots, names, answers, filename="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)