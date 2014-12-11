#' Function to generate data according to a linear trend, with a specified number of outliers
#' 
#' @title Generate data with a linear trend
#' 
#' @description
#' This function returns a data frame containing x, y, z, group, and outlier columns. 
#' It is designed for use with the \code{nullabor} package, to create lineups.
#' 
#' @details 
#' The returned data frame contains columns:
#' \itemize{
#'  \item \code{x}, a slightly non-uniform vector of values between -5 and 5
#'  \item \code{y}, which is linearly related to \code{x} (absolute slope between .5 and 4.5)
#'  \item \code{z}, which is linearly related to \code{x} and \code{y} 
#'        (the strength of this relationship varies) with additional random noise. 
#'        \code{z} is included to facilitate the second-level permutation clustering.
#'  \item \code{outlier}, which indicates whether the point is an outlier
#'  \item \code{group}, which indicates which group the point belongs to
#'  \item \code{a}, the "true" slope value
#'  \item \code{b}, the "true" intercept value
#'  \item \code{r2}, the fitted $R^2$ value
#' } 
#' 
#' @param n Number of points to generate
#' @param sd.data standard deviation of the data around the line
#' @param n.outliers number of outliers
#' @param ngroups number of groups to cut x values into (including a group for outliers, if any are generated)
#' @param group.strength sd of random noise used to cluster values
linear.trend <- function(n=30, sd.data=5, n.outliers=0, ngroups=2, group.strength=0.1){
  require(plyr)
  require(msm)
  if(n.outliers>.2*n){
    stop("There can be at most .2*n outliers.")
  }
  if(sd.data<=0){
    stop("sd.data must be positive.")
  }
  
  # generate slope - either positive or negative, between .5 and 4.5
  a <- sample(c(-1, 1), 1)*rtnorm(1, mean=2.5, lower=.5, upper=4.5)
  # generate intercept
  b <- rnorm(1)
  df <- data.frame(x=seq(-5, 5, length.out=n))
  df$y <- a*df$x+b+rnorm(n, sd=sd.data)
  df$outlier <- FALSE

  # cut groups so that outliers are their own group
  if((ngroups-as.numeric(n.outliers>0))>1){
    df$group <- cut(df$x+rnorm(n, sd=1), breaks=ngroups-as.numeric(n.outliers>0), labels=F)
  } else {
    df$group <- 1
  }
  
  
  # fuzz x so that points aren't evenly spaced
  df$x <- df$x+rnorm(n, sd=0.05)
  df$z <- rnorm(1, 1, .5)*df$x+rnorm(1, 1, .5)*df$y+rnorm(n, sd=group.strength)
  if(n.outliers>0){
    # select outlier from the first 1/5 of the dataset or the last 1/5 of the dataset
    idx <- c(1:n)
    cutoff <- sample(c("low", "high"), 1)
    outlier.vals <- idx[(cutoff=="low" & idx<=n/5) | (cutoff=="high" & idx>=4*n/5)]
    outlier <- sample(outlier.vals, n.outliers)
    replace.vals <- idx[(cutoff!="low" & idx<=n/5) | (cutoff!="high" & idx>=4*n/5)]
    replace <- sample(replace.vals, n.outliers)
    df$outlier[outlier] <- TRUE
    df$y[outlier] <- jitter(df$y[replace])
    df$group[outlier] <- ngroups
  }
  df$a <- a
  df$b <- b
  df$r2 <- summary(lm(df$y~df$x))$r.squared
  
  return(df)
}

#' Permute groups (for aesthetics) to test the effect of similarity vs. proximity
#' 
#' @description This function is used after the nullabor function lineup(null_permute, data). 
#' It uses hierarchical clustering to create a new group variable from permuted data. 
#' This group variable is used for displaying other aesthetics, like color or shape. 
#' In the target plot, there will be \code{ngroups} clusters which may slightly overlap. 
#' In the 
#' 
#' @param lineupdata output from lineup(null_permute, data)
#' @param ngroups number of clusters in the target plot
#' @param pos position of the second target plot
permute.groups2 <- function(lineupdata, ngroups=3, pos=sample(1:20, 1)){
  require(plyr)
  ddply(lineupdata, .(.sample), function(df){
#     dst <- dist(df[,c("x", "y", "z")])
    dst <- dist(df[,c("x", "y")])
    if(sum(df$.sample==pos)==0){
      df$group.k = cutree(hclust(dst, method="complete"), round(nrow(df)/(ngroups)))%%ngroups+1
    } else {
      df$group.k = cutree(hclust(dst, method="complete"), ngroups)
    }
    df
  })
}

#' Permute groups (for aesthetics) to test the effect of similarity vs. proximity
#' 
#' @description This function is used after the nullabor function lineup(null_permute, data). 
#' It uses the first iteration of k-means clustering to create a new group variable. 
#' This group variable is used for displaying other aesthetics, like color or shape. 
#' In the target plot, there will be \code{ngroups} clusters which may slightly overlap. 
#' 
#' @param df data frame with x, y columns
#' @param ngroups number of clusters in the target plot
#' @param pos position of the second target plot
permute.groups <- function(df, ngroups=3){
  require(plyr)
  # sample ngroup points, use 1st step of kmeans as "group" allocation with 3*ngroups groups
  kmcenter <- sample(1:nrow(df), ngroups*3)
  df$group <- apply(sapply(kmcenter, function(i) sqrt((df$x-df$x[i])^2 + (df$y-df$y[i])^2)), 1, which.min)
  df$xcenter <- df$x[kmcenter[df$group]]
  df$ycenter <- df$y[kmcenter[df$group]]
  df$group <- df$group %% ngroups + 1
  df
}

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

#' @title Generate data with groups arranged according to a linear trend 
#' 
#' @description
#' This function returns a data frame containing x, y, and group columns. 
#' It is designed for use with the \code{nullabor} package, to create lineups.
#' 
#' @details 
#' The returned data frame contains columns:
#' \itemize{
#'  \item \code{x}, a slightly non-uniform vector of values between -5 and 5
#'  \item \code{y}, which is linearly related to \code{x} (absolute slope between .5 and 4.5)
#'  \item \code{group}, which indicates which group the point belongs to
#'  \item \code{a}, the "true" slope value
#'  \item \code{b}, the "true" intercept value
#'  \item \code{r2}, the fitted $R^2$ value
#' } 
#' @param n Number of points to generate
#' @param sd.groups standard deviation of the group centers around the line
#' @param sd.data sd of random noise around group centers
#' @param ngroups number of groups to cut x values into (including a group for outliers, if any are generated)
#' @param null.line generate data without a line trend
#' @param null.group generate data without groups
linear.group.trend <- function(n=80, sd.groups=.1, sd.data=1, slope=1, ngroups=4, null.line=FALSE, null.group=FALSE){
  if(sd.data<=0 | sd.groups<0){
    stop("sd.data must be positive.")
  }
  # Determine how many points in each group
  id.group <- rep(1:ngroups, times=ceiling(n/ngroups))[1:n]
  
  # Set slope
  if(null.line){
    a <- 0
    b <- ngroups/2
  } else {
    a <- slope
    b <- 0
  }

  # Step 1: Generate group centers
  group.centers <- 1:ngroups - 1
  group.centers <- data.frame(xcenter=group.centers,
                              ycenter=a*(group.centers) + b,
                              group=group.centers)

  # Step 2: Permute the points in x
  # Step 3: Add general error variance in y
  if(null.group){
    xerr <- runif(n, -.5, .5)
    yerr <- rnorm(n, mean=0, sd=sd.data)
  } else {
    xerr <- rnorm(n, mean=0, sd=sd.groups)
    yerr <- rnorm(n, mean=0, sd=sd.data)
  }

  # Aggregate into a data frame (keep group centers)
  # Step 4: Scale x and y by k to get points in [0,1]-ish for x and proportionate points in y
  df <- data.frame(x=(group.centers$xcenter[id.group] + xerr)/ngroups, 
                   y=(group.centers$ycenter[id.group] + yerr)/ngroups, 
                   xcenter = (group.centers$xcenter[id.group])/ngroups,
                   ycenter = (group.centers$ycenter[id.group])/ngroups,
                   group=id.group)

  # Permutue group labels using k-means.
  if(null.group){
    df <- permute.groups(df, ngroups)
  }
  
  return(df)
}




#' Function to generate 7 plots which permute slope and the aesthetics 
#' color, shape, color+shape(redundant), or a combination of slope and 
#' the other aesthetics.
#' @param seed a seed to be used to generate the data which is shared by all 7 plots
#' @param n Number of points to generate
#' @param sd.groups standard deviation of the group centers around the line
#' @param sd.data sd of random noise around group centers
#' @param ngroups number of groups to cut x values into (including a group for outliers, if any are generated)
#' @param ... additional arguments to data.gen.function()
make.plot.set <- function(seed=NA, 
                          n=80, sd.groups=0.1, sd.data=1, ngroups=4,                        
                          group = "Group Label",
                          quiet=!is.na(seed),
                          ...){
  require(plyr)
  require(ggplot2)
  require(nullabor)
  
  # Define colors and shapes
  colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)
  
  colortm <- read.csv("./Data/color-perceptual-kernel.csv")
  shapetm <- read.csv("./Data/shape-perceptual-kernel.csv")
  
  color.pal <- sample(best.combo(ngroups, colors, colortm))
  shape.pal <- sample(best.combo(ngroups, shapes, shapetm))
  
  # Check seed validity
  if(!(is.numeric(seed) | is.na(seed))){
    warning(paste0("Invalid Seed: type must be numeric (type is ",
                   mode(seed),
                   "). Proceeding with automatically generated seed, ",
                   "and setting quiet=F."))
    seed <- NA
    quiet <- FALSE
  }
  
  # Set seed if not set
  if(is.na(seed)){
    seed = sample(0:1e6, 1) + sample(0:1e3, 1) + sample(0:100, 1)
  }
  if(!quiet){
    message(paste0("Setting Seed: ", seed))
  }
  set.seed(seed)
  
  # Create storage and indexing variables
  plots <- lapply(1:7, function(i) ggplot() + geom_blank())
  answers <- data.frame()
  data <- data.frame()
  j <- 1
  
  # Generate data (to be used in all plots)
  dframe <- linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, ngroups=ngroups)
  
  # Slope alone
  pos.x <- sample(1:20, 1)
  nulldata <- rdply(19, function(.sample)  
                     linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                                        ngroups=ngroups, null.line=T))
  lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y), size=3) + 
    facet_wrap(~.sample) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=pos.x, 
                    target2=NA, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="Slope")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1
  
  # Color alone
  pos.x <- sample(1:20, 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.group=T))
  lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=color.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=NA, 
                    target2=pos.x, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="Color")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1

  # Shape alone
  pos.x <- sample(1:20, 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.group=T))
  lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shape.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=NA, 
                    target2=pos.x, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="Shape")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1
  
  # Shape+Color (redundant) alone
  pos.x <- sample(1:20, 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.group=T))
  lineupdata <- lineup(true=dframe, pos=pos.x, n=20, samples=nulldata)
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shape.pal) +
    scale_color_manual(values=color.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=NA, 
                    target2=pos.x, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="ColorShape")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1
  
  # Slope vs. Color
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.line=T, null.group=T))
  alt.target <- linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.line=T, null.group=F)
  lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
  lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
                      cbind(.sample=pos.y, alt.target))
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_color_manual(values=color.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=pos.x, 
                    target2=pos.y, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="SlopeColor")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1
  
  # Slope vs. Shape
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.line=T, null.group=T))
  alt.target <- linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                                   ngroups=ngroups, null.line=T)  
  lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
  lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
                      cbind(.sample=pos.y, alt.target))
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shape.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=pos.x, 
                    target2=pos.y, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="SlopeShape")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)
  j <- j+1
  
  # Slope vs. Color+Shape (redundant)
  pos.x <- sample(1:20, 1)
  pos.y <- sample(c(1:20)[which(1:20!=pos.x)], 1)
  nulldata <- rdply(19, function(.sample)  
    linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                       ngroups=ngroups, null.line=T, null.group=T))
  alt.target <- linear.group.trend(n=n, sd.groups=sd.groups, sd.data=sd.data, 
                                   ngroups=ngroups, null.line=T)  
  lineupdata <- lineup(true=permute.groups(dframe), pos=pos.x, n=20, samples=nulldata)
  lineupdata <- rbind.fill(subset(lineupdata, .sample!=pos.y), 
                      cbind(.sample=pos.y, alt.target))
  
  plots[[j]] <- 
    ggplot(data=lineupdata) + 
    geom_point(aes(x=x, y=y, shape=factor(group), color=factor(group)), size=3) + 
    facet_wrap(~.sample) + 
    scale_shape_manual(values=shape.pal) +
    scale_color_manual(values=color.pal) +
    theme_lineup()
  tmp <- data.frame(name=paste0(group, "-", seed, "-", j), 
                    target1=pos.x, 
                    target2=pos.y, 
                    seed=seed, 
                    idx=j, 
                    group=group, 
                    type="SlopeColorShape")
  lineupdata$name <- tmp$name
  data <- rbind.fill(data, lineupdata)
  answers <- rbind(answers, tmp)

  return(list(plots=plots, answers=answers, data=data))
}

#' @title Lineup Theme for ggplot2
#' 
#' @param base_size text size (default 12)
#' @param base_family font family (default "")
theme_lineup <- function(base_size = 12, base_family = ""){
  require(ggplot2)
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(legend.position="none", 
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank())}