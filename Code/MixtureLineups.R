
#' Function to find the subset of a palette with the largest total pairwise distance
#' @param ngroups size of subset
#' @param palette a vector of aesthetic options
#' @param dist.matrix a distance matrix corresponding to palette
best.combo <- function(ngroups=3, palette, dist.matrix){
  # check distance matrix
  if(nrow(dist.matrix)!=length(palette) | ncol(dist.matrix)!=length(palette)){
    stop(paste0("The distance matrix does not match the size of the palette. ",
                "It should be ", length(palette), "x", length(palette), "."))
  }
  if(sum(dist.matrix<0)>0){
    stop("Distance matrix cannot have negative entries.")
  }
  
  require(combinat)
  clist <- t(combn(1:length(palette), ngroups))
  pairwise.combos <- t(combn(1:ngroups, 2))
  res <- rowSums(apply(pairwise.combos, 1, function(i){diag(as.matrix(dist.matrix[clist[,i[1]], clist[,i[2]]]))}))
  
  return(palette[clist[which.max(res),]])
}


sim.clusters <- function(K, N){
  # Choose cluster centers
  ang <- seq(2*pi/K, 2*pi, length.out=K) + runif(1, 0, 2*pi)
  rad <- runif(K, 0, 1)
  centers <- data.frame(x=rad*cos(ang), y=rad*sin(ang), group=1:K)
  centers[,1:2] <- scale(centers[,1:2])
#   centers <- data.frame(x=runif(K, -1, 1), y=runif(K, -1, 1), group=1:K)   

  # Set up data set
  m1.data <- data.frame(group=rep(1:K, each=ceiling(N/K))[1:N], x=0, y=0) 
  
  # Add in x and y error
  m1.data$x <- centers$x[m1.data$group] + rnorm(N, 0, 1)
  m1.data$y <- centers$y[m1.data$group] + rnorm(N, 0, 1)
  
  return(m1.data)
}

sim.line <- function(K, N, a=1){
  # Simulate data from line
  m2.data <- data.frame(x=jitter(seq(-2, 2, length.out=N)), y=0)
  m2.data$y <- a*m2.data$x + rnorm(N, 0, 1)
  
  return(m2.data)
}

mixture.sim <- function(lambda, K, N){
  m1.data <- sim.clusters(K=K, N=N)
  m2.data <- sim.line(K=K, N=N)

  mix.data <- data.frame(
    x=lambda*m1.data$x + (1-lambda)*m2.data$x,
    y=lambda*m1.data$y + (1-lambda)*m2.data$y
    )  
  
  mix.data$x <- scale(mix.data$x, center=T)
  mix.data$y <- scale(mix.data$y, center=T)
  # Sample group according to lambda - 
  # If lambda = 1, don't permute. If lambda = 0, permute everything
  # For entries that will be permuted, transition to group +/-1 with equal probability 
  # (unless edge, then transition to self or either +/- 1 with equal prob).
  mix.data$group <- sapply(m1.data$group, function(i){
    trans.vec <- c(-1*(i>1), 1*(i<K))
    if(lambda>0 & lambda < 1){
      i + rbinom(1, 1, 1-lambda)*sample(trans.vec, size=1, replace=F)
    } else if(lambda==0){
      i
    } else{
      i + sample(trans.vec, size=1, replace=F)
    }
    
  })
  
  return(mix.data)
}

# make.plot.set <- function(seed=NA, N=60, K=4, lambda = 1, plotset="Group Label", quiet=!is.na(seed)){
#   require(plyr)
#   require(ggplot2)
#   require(nullabor)
#   
#   # Define colors and shapes
#   colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
#                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
#   shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)
#   
#   colortm <- read.csv("./Data/color-perceptual-kernel.csv")
#   shapetm <- read.csv("./Data/shape-perceptual-kernel.csv")
#   
#   color.pal <- sample(best.combo(K, colors, colortm))
#   shape.pal <- sample(best.combo(K, shapes, shapetm))
#   
#   # Check seed validity
#   if(!(is.numeric(seed) | is.na(seed))){
#     warning(paste0("Invalid Seed: type must be numeric (type is ",
#                    mode(seed),
#                    "). Proceeding with automatically generated seed, ",
#                    "and setting quiet=F."))
#     seed <- NA
#     quiet <- FALSE
#   }
#   
#   # Set seed if not set
#   if(is.na(seed)){
#     seed = sample(0:1e6, 1) + sample(0:1e3, 1) + sample(0:100, 1)
#   }
#   if(!quiet){
#     message(paste0("Setting Seed: ", seed))
#   }
#   set.seed(seed)
#   
#   # Create storage and indexing variables
#   plots <- lapply(1:7, function(i) ggplot() + geom_blank())
#   answers <- data.frame()
#   data <- data.frame()
#   j <- 1
#   
#   # Generate data (to be used in all plots)
#   dframe <- mixture.sim(lambda=lambda, N=N, K=K)
#   
#   # Slope alone
#   pos.x <- sample(1:20, 1)
#   nulldata <- rdply(19, function(.sample)  
#     mixture.sim(lambda=1, N=N, K=K)
#   )
#   lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
# #   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y), size=3, shape=1) + 
#     geom_smooth(aes(x=x, y=y), method="lm", se=F) + 
#     facet_wrap(~.sample) +
#     theme_lineup()
#   tmp <- data.frame(
#     name=paste0(plotset, "-", seed, "-", j), 
#     target1=pos.x, 
#     target2=NA, 
#     seed=seed, 
#     idx=j, 
#     plotset=plotset, 
#     type="Slope")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Color alone
#   pos.x <- sample(1:20, 1)
#   nulldata <- rdply(19, function(.sample)   
#     mixture.sim(lambda=0, N=N, K=K)
#   )
#   lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
#   
# #   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y, color=factor(group)), size=3, shape=1) + 
#     stat_ellipse(geom="polygon", level=.9, aes(x=x, y=y, colour=factor(group)), fill="transparent") +
#     facet_wrap(~.sample) + 
#     scale_color_manual(values=color.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=NA, 
#                     target2=pos.x, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="Color")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Shape alone
#   pos.x <- sample(1:20, 1)
#   nulldata <- rdply(19, function(.sample)   
#     mixture.sim(lambda=0, N=N, K=K)
#   )
#   lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
# #   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
#     stat_ellipse(geom="polygon", level=.9, aes(x=x, y=y, group=group), colour="black", fill="transparent") +
#     facet_wrap(~.sample) + 
#     scale_shape_manual(values=shape.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=NA, 
#                     target2=pos.x, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="Shape")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Shape+Color (redundant) alone
#   pos.x <- sample(1:20, 1)
#   nulldata <- rdply(19, function(.sample)   
#     mixture.sim(lambda=0, N=N, K=K)
#   )
#   lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
# #   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     stat_ellipse(geom="polygon", level=.9, aes(x=x, y=y, colour=factor(group)), fill="transparent") +
#     geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
#     facet_wrap(~.sample) + 
#     scale_shape_manual(values=shape.pal) +
#     scale_color_manual(values=color.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=NA, 
#                     target2=pos.x, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="ColorShape")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Slope vs. Color
#   pos.x <- sample(1:20, 1)
#   pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
#   nulldata <- rdply(18, function(.sample)   
#     mixture.sim(lambda=.5, N=N, K=K)
#   )
#   alt.target <- mixture.sim(lambda=1-lambda, N=N, K=K)
#   lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
#   lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
#                            cbind(.sample=pos.y, alt.target))
#   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y, color=factor(group)), size=3, shape=1) + 
#     scale_shape_discrete(solid=F) + 
#     facet_wrap(~.sample) + 
#     scale_color_manual(values=color.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=pos.x, 
#                     target2=pos.y, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="SlopeColor")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Slope vs. Shape
#   pos.x <- sample(1:20, 1)
#   pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
#   nulldata <- rdply(19, function(.sample)  
#     linear.group.trend(N=N, sd.groups=sd.groups, sd.data=sd.data, 
#                        K=K, null.line=T, null.group=T))
#   alt.target <- linear.group.trend(N=N, sd.groups=sd.groups, sd.data=sd.data, 
#                                    K=K, null.line=T)  
#   lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
#   lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
#                            cbind(.sample=pos.y, alt.target))
#   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
#     facet_wrap(~.sample) + 
#     scale_shape_manual(values=shape.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=pos.x, 
#                     target2=pos.y, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="SlopeShape")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   j <- j+1
#   
#   # Slope vs. Color+Shape (redundant)
#   pos.x <- sample(1:20, 1)
#   pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
#   nulldata <- rdply(19, function(.sample)  
#     linear.group.trend(N=N, sd.groups=sd.groups, sd.data=sd.data, 
#                        K=K, null.line=T, null.group=T))
#   alt.target <- linear.group.trend(N=N, sd.groups=sd.groups, sd.data=sd.data, 
#                                    K=K, null.line=T)  
#   lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
#   lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
#                            cbind(.sample=pos.y, alt.target))
#   
#   plots[[j]] <- 
#     ggplot(data=lineupdata) + 
#     geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
#     facet_wrap(~.sample) + 
#     scale_shape_manual(values=shape.pal) +
#     scale_color_manual(values=color.pal) +
#     theme_lineup()
#   tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
#                     target1=pos.x, 
#                     target2=pos.y, 
#                     seed=seed, 
#                     idx=j, 
#                     plotset=plotset, 
#                     type="SlopeColorShape")
#   lineupdata$name <- tmp$name
#   data <- rbind.fill(data, lineupdata)
#   answers <- rbind(answers, tmp)
#   
#   return(list(plots=plots, answers=answers, data=data))
# }