

lambda <- .05
sd.i <- .5
sd.g <- .1
corr <- .5
N <- 50
K <- 3

library(mvtnorm)
library(lme4)

gen.data <- function(lambda, sd=.5, corr=.5, N=50, K=3){
  tmp <- data.frame(rmvnorm(50, c(0,0), sigma=matrix(c(sd.i, corr*sd.i, corr*sd.i, sd.i), nrow=2, byrow=T)))
  names(tmp) <- c("x", "y")
  tmp <- tmp[order(tmp$x),]
  
  
  centers <- kmeans(tmp, centers = K)
  centers <- suppressWarnings(data.frame(centers$centers[centers$cluster,]+rmvnorm(K, sigma=matrix(c(sd.g, corr*sd.g, corr*sd.g, sd.g), nrow=2, byrow=T))[centers$cluster,], group=centers$cluster))
  names(centers) <- c("x", "y", "group")
  
  data <- lambda*centers[,1:2] + (1-lambda)*tmp[,1:2]
  data$group <- centers$group
#   qplot(data=data, x=x, y=y, color=factor(group))
  return(data)
}

library(lmerTest)
test.lambda <- function(lambda, sd=.5, corr=.5, N=50, K=3){
  data <- gen.data(lambda=lambda/5, sd=sd, corr=corr, N=N, K=K)
  m.full <- lm(y ~ x + factor(group), data=data)
  m.line <- lm(y ~ x , data=data)
  m.group <- lm(y ~ factor(group), data=data)
  m.null <- lm(y ~ 1, data=data)
  z <- rbind(
    data.frame(part=c("null", "line", "line.full"), anova(m.null, m.line, m.full)), 
    data.frame(part=c("null", "group", "group.full"), anova(m.null, m.group, m.full))[2:3,])
  
  
  names(z) <- c("part", "DFE", "SSE", "DF", "SS", "F.val", "p")
  z$lambda <- lambda
  return(z)
}
library(plyr)
res <- ldply(rep(seq(0, 1, .01), each=5), test.lambda, corr=.9)

qplot(data=subset(res, !is.na(F.val)), x=lambda, y=SSE) + facet_wrap(~part)
