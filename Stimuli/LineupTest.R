library(nullabor)
library(ggplot2)
library(plyr)

source("./Code/theme_lineup.R")
source("./Code/GenerateData.R")

dframe <- linear.trend()

# Slope alone
pos.x <- sample(1:20, 1)
lineupdata <- lineup(null_permute("y"), dframe, pos=pos.x)
ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()

# Color alone
pos.x <- sample(1:20, 1)
lineupdata <- lineup(null_permute("group"), dframe, pos=pos.x)
ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()

# Slope vs. Color
pos.x <- sample(1:20, 1)
pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
lineupdata <- permute.groups2(lineup(null_permute("x"), dframe, pos=pos.x), 
                              ngroups=3, pos=pos.y)

ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()


dframe <- linear.trend(n=50, n.outliers=3, sd=4)
# Slope alone
pos.x <- sample(1:20, 1)
lineupdata <- lineup(null_permute("y"), dframe, pos=pos.x)
ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()

# Color alone
pos.x <- sample(1:20, 1)
lineupdata <- lineup(null_permute("group"), dframe, pos=pos.x)
ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()

# Slope vs. Color
pos.x <- sample(1:20, 1)
pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
lineupdata <- permute.groups2(lineup(null_permute("y"), dframe, pos=pos.x), 
                              ngroups=4, pos=pos.y)

ggplot(data=lineupdata) + 
  geom_point(aes(x=x, y=y, color=factor(group.k)), size=3) + 
  facet_wrap(~.sample) + 
  theme_lineup()

