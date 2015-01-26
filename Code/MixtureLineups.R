
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

sim.clusters <- function(K, N, q=2/3){
  if(q==0){q <- .01}
  if(q==1){q <- .99}
  
  
  xc <- sample(1:K, replace=F)
  yc <- sample(1:K, replace=F)
  xc <- jitter(xc, amount=.2)
  yc <- jitter(yc, amount=.2)
  while(cor(xc,yc)<.25 | cor(xc,yc)>.9){
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

#   m1.data$y <- scale(m1.data$y, center=0, scale=sqrt(1/3*q^2*K^2))*2
#   m1.data$x <- scale(m1.data$x, center=.5*K, scale=sqrt(1/12*K^2))*2
# ggplot(aes(x=x, y=y), data=m1.data) + geom_point(aes(colour=factor(group)), data=m1.data) + coord_equal(ratio=1) + geom_point(data=m2.data)
  return(m1.data)
}

sim.line <- function(K, N, sd=1.5){
  # Simulate data from line
  m2.data <- data.frame(x=jitter(seq(-4, 4, length.out=N)), y=0)
  m2.data$y <- m2.data$x + rnorm(N, 0, sd)
  
  return(m2.data)
}

mixture.sim <- function(lambda, K, N, q=2/3, sd=1.5){
  m1.data <- sim.clusters(K=K, N=N, q=q)
  m1.data[,c("x", "y")] <- scale(m1.data[,c("x", "y")]) 
  m2.data <- sim.line(K=K, N=N, sd=sd)
  m2.data[,c("x", "y")] <- scale(m2.data[,c("x", "y")])
  
#  if(lambda==0){
#    tmp <- m1.data
#    m1.data <- with(tmp, data.frame(x=x, y=.5*x+y, group=group))
#  }
#   qplot(data=m1.data, x=x, y=y, color=factor(group), size=I(3)) + coord_equal(ratio=1)
  
  ll <- rbinom(n=N, size=1, prob=lambda)  # one model or the other
  mix.data <- data.frame(
  #  x=lambda*m1.data$x + (1-lambda)*m2.data$x,
  #  y=lambda*m1.data$y + (1-lambda)*m2.data$y,
    x = ll*m1.data$x + (1-ll)*m2.data$x,  
    y=ll*m1.data$y + (1-ll)*m2.data$y,
    group=as.numeric(m1.data$group)  
    )    
  
#   # Sample group according to lambda - 
#   # If lambda = 1, don't permute. If lambda = 0, permute everything
#   # For entries that will be permuted, transition to group +/-1 with equal probability 
#   # (unless edge, then transition to self or either +/- 1 with equal prob).
#   if(lambda>0 & lambda < 1){
#     fix <- rbinom(N, 1, 1-lambda)
#   } else if(lambda==0){
#     fix <- rep(1, N)
#   } else {
#     fix <- 0
#   }
#   
#   mix.data$group <- (1-fix)*mix.data$group + fix*sample(mix.data$group, size=N, replace=F)
  
#  mix.data$group <- mix.data$group%%K + 1
  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  mix.data$group <- cutree(hclust(dist(mix.data[,c("x", "y")])), k=K) # grouping by the best K clusters
                       
  return(mix.data)
}