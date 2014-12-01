require(plyr)
linear.trend <- function(n=30, sd.data=5, n.outliers=0){
  a <- sample(c(-1, 1), 1)*rnorm(1, mean=2.5)
  b <- rnorm(1)
  df <- data.frame(x=seq(-5, 5, length.out=n))
  df$y <- a*df$x+b+rnorm(n, sd=sd.data)
  df$outlier <- FALSE
  df$group <- as.numeric((df$x+rnorm(n, sd=1))>0)+1   
  df$x <- df$x+rnorm(n, sd=0.01)
  df$z <- rnorm(1, 1, .5)*df$x+rnorm(1, 1, .5)*df$y+rnorm(n, sd=0.1)
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
    df$group[outlier] <- 3
  }

  return(df)
}

permute.groups2 <- function(lineupdata, ngroups=3, pos=sample(1:20, 1)){
  ddply(lineupdata, .(.sample), function(df){
    dst <- dist(df[,c("x", "y", "z")])
#     dst <- dist(df[,c("x", "y")])
    if(sum(df$.sample==pos)==0){
      df$group.k = cutree(hclust(dst, method="complete"), round(nrow(df)/(ngroups)))%%ngroups+1
    } else {
      df$group.k = cutree(hclust(dst, method="complete"), ngroups)
    }
    df
  })
}