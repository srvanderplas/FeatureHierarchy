
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

sim.clusters <- function(K, N, q=.3){  
  xc <- sample(1:K, replace=F)
  yc <- sample(1:K, replace=F)
  xc <- jitter(xc, amount=.2)
  yc <- jitter(yc, amount=.2)
  while(cor(xc,yc)<.25 | cor(xc,yc)>.95){
    xc <- sample(1:K, replace=F)
    yc <- sample(1:K, replace=F)
    xc <- jitter(xc, amount=.2)
    yc <- jitter(yc, amount=.2)
  }
  
  yc <- scale(yc)
  xc <- scale(xc)
  
  groups <- sample(K, N, replace=TRUE, prob=abs(rnorm(K, mean=1/K, sd=0.5/K^2)))
  
  yerr <- rnorm(N, sd=q)
  xerr <- rnorm(N, sd=q)
  
  m1.data <- data.frame(x=xc[groups]+xerr, y=yc[groups]+yerr, group=groups)

# ggplot(aes(x=x, y=y), data=m1.data) + geom_point(aes(colour=factor(group)), data=m1.data) + coord_equal(ratio=1) + geom_point(data=m2.data)
  return(m1.data)
}

sim.line <- function(K, N, sd=.3, slope=1){
  # Simulate data from line
  m2.data <- data.frame(x=jitter(seq(-1-2*sd, 1+2*sd, length.out=N)), y=0)
  m2.data$y <- slope*m2.data$x + rnorm(N, 0, sd)
  
  return(m2.data)
}

mixture.sim <- function(lambda, K, N, q=.3, sd=.3, slope=1){
  m1.data <- sim.clusters(K=K, N=N, q=q)
#   m1.data[,c("x", "y")] <- scale(m1.data[,c("x", "y")])
  m2.data <- sim.line(K=K, N=N, sd=sd, slope=slope)
#   m2.data[,c("x", "y")] <- scale(m2.data[,c("x", "y")])
  
#   qplot(data=m1.data, x=x, y=y, color=factor(group), size=I(3)) + coord_equal(ratio=1)
  
  ll <- rbinom(n=N, size=1, prob=lambda)  # one model or the other
  mix.data <- data.frame(
    x = ll*m1.data$x + (1-ll)*m2.data$x,  
    y=ll*m1.data$y + (1-ll)*m2.data$y,
    group=as.numeric(m1.data$group)  
    )    

  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  mix.data$group <- cutree(hclust(dist(mix.data[,c("x", "y")])), k=K) # grouping by the best K clusters

  return(mix.data)
}

gen.data <- function(input){
  
  pos <- sample(1:20, size=2)
  # Trend
  dframe <- mixture.sim(lambda=0, N=input$N, K=input$K, q=input$q, sd=input$sd)
  # Clusters
  dframe2 <- mixture.sim(lambda=1, N=input$N, K=input$K, q=input$q, sd=input$sd)
  # Nulls
  nulldata <- rdply(19, function(.sample) 
        mixture.sim(lambda=.5, 
                    N=input$N, 
                    K=input$K, 
                    q=input$q, 
                    sd=input$sd, 
                    slope=runif(1, .2, .8)
        ))

  data <- lineup(true=dframe, pos=pos[1], n=20, samples=nulldata)
  data <- rbind.fill(
    subset(data, .sample!=pos[2]), cbind(.sample=pos[2], dframe2))
  data$target1 <- pos[1]
  data$target2 <- pos[2]
  
  data
}

gen.plot <- function(dd, aes, stats, colorp, shapep){
#   colorp <- best.combo(length(unique(dd$group)), colors, colortm)
#   shapep <- best.combo(length(unique(dd$group)), shapes, shapetm)
#   range <- range(c(dd$x, dd$y))
#   
  plot <- ggplot(data=dd, aes(x=x, y=y)) + theme_lineup() + facet_wrap(~.sample)  + coord_fixed(ratio=1)
  
  # Set Aesthetics
  if(length(aes)==0){
    plot <- plot + geom_point(size=3, shape=1) + 
      scale_shape_discrete(solid=F)
  } else if(length(aes)==1){
    if("Color"%in%aes){
      plot <- plot + geom_point(aes(color=factor(group)), size=3, shape=1) + 
        scale_color_manual(values=colorp)
    } else {
      plot <- plot + geom_point(aes(shape=factor(group)), size=3) + 
        scale_shape_manual(values=shapep)
    }
  } else {
    plot <- plot + geom_point(aes(color=factor(group), shape=factor(group)), size=3) + 
      scale_color_manual(values=colorp) + 
      scale_shape_manual(values=shapep)
  }
  
  # Set other geoms/aids
  if("Reg. Line"%in%stats){
    plot <- plot + geom_smooth(method="lm", color="black", se=F)
  } 
  if("Error Bands"%in%stats){
    plot <- plot + geom_ribbon(stat="smooth", method="lm", fill="black", color="transparent", alpha=.3)
  }
  
  if("Ellipses"%in%stats){
    if("Color"%in%aes){
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(colour=factor(group)), fill="transparent")
    } else if("Shape"%in%aes){
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), colour="black", fill="transparent")
    } else {
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), colour="black", fill="transparent")
    }
  }
  
  plot
}