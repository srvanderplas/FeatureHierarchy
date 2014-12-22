
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


# q = separation between clusters
sim.clusters <- function(K, N, q=2/3){
  
  X <- data.frame(matrix(rnorm(N*floor(q*N)), nrow=N))
  X$class <- rep(1:K, each=ceiling(N/K))[1:N]
  library(MASS)
  l1 <- lda(factor(class)~., data=X)
  p1 <- predict(l1)
#   qplot(LD1, LD2, data=data.frame(p1$x), colour=p1$class)
  
  # Set up data set
  m1.data <- data.frame(p1$x)
  names(m1.data) <- c("x", "y")
  m1.data$group <- p1$class
  m1.data$x <- scale(m1.data$x)*2
  m1.data$y <- scale(m1.data$y)*2
#   qplot(x, y, data=m1.data, colour=group)
  
  return(m1.data)
}

sim.line <- function(K, N, a=1){
  # Simulate data from line
  m2.data <- data.frame(x=jitter(seq(-4, 4, length.out=N)), y=0)
  m2.data$y <- a*m2.data$x + rnorm(N, 0, 1)
  
  return(m2.data)
}

mixture.sim <- function(lambda, K, N, q=2/3){
  m1.data <- sim.clusters(K=K, N=N, q=q)
  m1.data[,c("x", "y")] <- scale(m1.data[,c("x", "y")])
  m2.data <- sim.line(K=K, N=N)
  m2.data[,c("x", "y")] <- scale(m2.data[,c("x", "y")])

  mix.data <- data.frame(
    x=lambda*m1.data$x + (1-lambda)*m2.data$x,
    y=lambda*m1.data$y + (1-lambda)*m2.data$y,
    group=as.numeric(m1.data$group)
    )  
  
  # Sample group according to lambda - 
  # If lambda = 1, don't permute. If lambda = 0, permute everything
  # For entries that will be permuted, transition to group +/-1 with equal probability 
  # (unless edge, then transition to self or either +/- 1 with equal prob).
  
  mix.data$group <- sapply(mix.data$group, function(i){
    trans.vec <- c(-1*(i>1), 1*(i<K))
    if(lambda>0 & lambda < 1){
      i + rbinom(1, 1, 1-lambda)*sample(trans.vec, size=1, replace=F)
    } else if(lambda==1){
      i
    } else{
      i + sample(trans.vec, size=1, replace=F)
    }
  })
  
  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  return(mix.data)
}