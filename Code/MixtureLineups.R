interactive_lineup <- function(method, plotobj, filename, script, toggle="toggle") {
  z = as.list(match.call()[-1])
  eval(call(eval(z$method), plotobj))
  require(gridSVG)
  grobs <- grid.ls()
  
  idx <- grep("panel-", grobs$name)
  for (i in idx) { 
    grid.garnish(grobs$name[i],
                 onmouseover=paste("frame('",grobs$name[i+2], ".1')", sep=""),
                 onmouseout=paste("deframe('",grobs$name[i+2], ".1')", sep=""), 
                 onmousedown=paste(sprintf("%shigh(evt, '", toggle),grobs$name[i+2], ".1')", sep=""))
  }
  
  # use script on server to get locally executable javascript code
  # or use inline option
  grid.script(filename=script)
  grid.export(filename, uniqueNames=FALSE, exportJS="inline", exportCoords="inline", exportMappings="inline")
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

sim.clusters <- function(K, N, sd.cluster=.3){  
  xc <- sample(1:K, replace=F)
  yc <- sample(1:K, replace=F)
  xc <- jitter(xc, amount=.2)
  yc <- jitter(yc, amount=.2)
  while(cor(xc,yc)<.25 | cor(xc,yc)>.75){
    xc <- sample(1:K, replace=F)
    yc <- sample(1:K, replace=F)
    xc <- jitter(xc, amount=.2)
    yc <- jitter(yc, amount=.2)
  }
  
  yc <- scale(yc)
  xc <- scale(xc)
  
  groups <- sample(K, N, replace=TRUE, prob=abs(rnorm(K, mean=1/K, sd=0.5/K^2)))
  
  yerr <- rnorm(N, sd=sd.cluster)
  xerr <- rnorm(N, sd=sd.cluster)
  
  m1.data <- data.frame(x=xc[groups]+xerr, y=yc[groups]+yerr, group=groups)
  return(m1.data)
}

sim.line <- function(K, N, sd.trend=.3){
  # Simulate data from line
  m2.data <- data.frame(x=jitter(seq(-1, 1, length.out=N)), y=0)
  m2.data$y <- m2.data$x + rnorm(N, 0, sd.trend)
  
  return(m2.data)
}

mixture.sim <- function(lambda, K, N, sd.trend=.3, sd.cluster=.3){
  m1.data <- sim.clusters(K=K, N=N, sd.cluster=sd.cluster)
  m1.data[,c("x", "y")] <- scale(m1.data[,c("x", "y")])
  m2.data <- sim.line(K=K, N=N, sd.trend=sd.trend)
  m2.data[,c("x", "y")] <- scale(m2.data[,c("x", "y")])
  
  ll <- rbinom(n=N, size=1, prob=lambda)  # one model or the other
  mix.data <- data.frame(
    x = ll*m1.data$x + (1-ll)*m2.data$x,  
    y = ll*m1.data$y + (1-ll)*m2.data$y,
    group=as.numeric(m1.data$group)  
    )

  mix.data[,c("x", "y")] <- scale(mix.data[,c("x", "y")])
  
  mix.data$group <- cutree(hclust(dist(mix.data[,c("x", "y")])), k=K) 
  # grouping by the best K clusters

  return(mix.data)
}

gen.data <- function(input){

  pos <- sample(1:20, size=2)
  # Trend
  dframe <- mixture.sim(lambda=0, N=input$N, K=input$K, sd.trend=input$sd.trend, sd.cluster=input$sd.cluster)
  # Clusters
  dframe2 <- mixture.sim(lambda=1, N=input$N, K=input$K, sd.trend=input$sd.trend, sd.cluster=input$sd.cluster)
  # Nulls
  nulldata <- rdply(19, function(.sample) 
        mixture.sim(lambda=.5, 
                    N=input$N, 
                    K=input$K, 
                    sd.cluster=input$sd.cluster, 
                    sd.trend=input$sd.trend
        ))

  data <- lineup(true=dframe, pos=pos[1], n=20, samples=nulldata)
  data <- rbind.fill(
    subset(data, .sample!=pos[2]), cbind(.sample=pos[2], dframe2))
  data$target1 <- pos[1]
  data$target2 <- pos[2]
  
  data
}

eval.df <- function(df){
  data.frame(
    line=summary(lm(y~x, data=df))$r.squared, 
    group=cluster(df)
  )
}

eval.data <- function(df){
  
  nulls <- subset(df, .sample!=target1 & .sample!=target2)
  groups <- subset(df, .sample==target2)
  lines <- subset(df, .sample==target1)
  
  nl <- ddply(nulls, .(.sample), eval.df)
  
  cl <- eval.df(groups)
  ll <- eval.df(lines)
  
  c(null.line = max(nl$line), 
    null.cluster = max(nl$group), 
    line=ll$line, 
    cluster=cl$group)
}

eval.data.quantiles <- function(i, data.set.parms, reps=3){
  tmp.sub <- NULL
  partmp <- data.frame()
  pardata <- data.frame()
  pardata.subplot.stats <- data.frame()
  pardata.stats <- data.frame()
  ntries <- 0
  while(sum(tmp.sub)<reps & ntries < 100){
    set <- (i-1)*reps+sum(as.numeric(tmp.sub))+1
    message(paste0("set = ", set))
    data.sub <- data.frame(set=set, gen.data(as.list(data.set.parms)))
    data.sub.subplot.stats <- 
      ddply(data.sub, .(set, .sample), 
            function(df){
              reg <- lm(y~x, data=df)
              data.frame(.sample=unique(df$.sample), 
                         LineSig = summary(reg)$r.squared, 
                         ClusterSig = cluster(df), 
                         lineplot=unique(df$target1), 
                         groupplot=unique(df$target2))
            })
    data.sub.stats <- data.frame(
      data.set.parms[,c("K", "sd.trend", "sd.cluster", "N")], 
      summarize(
        data.sub.subplot.stats,
        set=unique(set),
        line=LineSig[.sample==lineplot], 
        cluster=ClusterSig[.sample==groupplot], 
        null.line = max(LineSig[.sample!=lineplot & .sample!=groupplot]), 
        null.cluster=max(ClusterSig[.sample!=groupplot & .sample!=lineplot]),
        lineplot = unique(lineplot),
        groupplot = unique(groupplot)))
    
    # Calculate quantiles of datasets compared to simulated quantiles
    tmp <- data.frame(
      data.set.parms[,c("K", "sd.trend", "sd.cluster", "N")], 
      sim.quantile(data.sub.stats))
    tmp$set <- set
    
    # Require all quantiles to be between (.2, .8)
    tmp.sub1 <- as.logical(rowSums(
      tmp[,c("line", "cluster", "null.line", "null.cluster")]>.2 & 
        tmp[,c("line", "cluster", "null.line", "null.cluster")]<.8)==4)
    
    # increment ntries
    ntries <- ntries + 1
    
    if(tmp.sub1){
      tmp.sub <- c(tmp.sub, tmp.sub1)
      partmp <- rbind.fill(partmp, tmp)
      pardata <- rbind.fill(pardata, data.sub)
      pardata.stats <- rbind.fill(pardata.stats, data.sub.stats)
      pardata.subplot.stats <- rbind.fill(pardata.subplot.stats, data.sub.subplot.stats)
    }
  }
  
  if(ntries==100){
    warning(paste0("Limit of 100 tries reached for \n", paste(names(data.set.parms), data.set.parms, sep="\t", collapse="\n"), "\nIncomplete results returned."))
  }
  
  return(list(data=pardata, data.stats=pardata.stats, data.subplot.stats=pardata.subplot.stats, quantile.eval=partmp, ntries=ntries))
}

gen.plot <- function(dd, aes, stats, colorp=NULL, shapep=NULL){
  pointsize <- 1.5
  if(is.null(colorp)) colorp <- best.combo(length(unique(dd$group)), colors, colortm)
  if(is.null(shapep)) shapep <- best.combo(length(unique(dd$group)), shapes, shapetm)
  
  plot <- ggplot(data=dd, aes(x=x, y=y)) + theme_lineup() + facet_wrap(~.sample)  

  # lines and ellipses are not data structure, so reduce contrast to emphasize points!  
  # Set other geoms/aids
  if("Reg. Line"%in%stats){
    plot <- plot + geom_smooth(method="lm", color="grey30", se=F, fullrange=TRUE)
  } 
  if("Error Bands"%in%stats){
    #     xrange <- range(dd$x)
    tmp <- ddply(dd, .(.sample), function(df){
      model <- lm(y~x, data=df)
      range <- diff(range(df$x))
      newdata <- data.frame(x=seq(min(dd$x)-.1*range, max(dd$x)+.1*range, length.out=400))
      data.frame(.sample=unique(df$.sample), x=newdata$x, 
                 predict.lm(model, newdata=newdata, interval="prediction", level=0.9))
    })
    if("Shade Error Bands"%in%stats & "Error Bands"%in%stats){
      plot <- plot + 
        geom_line(data=tmp, aes(x=x, y=lwr), linetype=2, inherit.aes=F) + 
        geom_line(data=tmp, aes(x=x, y=upr), linetype=2, inherit.aes=F) + 
        geom_ribbon(data=tmp, aes(x=x, ymin=lwr, ymax=upr), fill="black", color="transparent", alpha=.1, inherit.aes=F)
    } else {
      plot <- plot + 
        geom_line(data=tmp, aes(x=x, y=lwr), linetype=2, inherit.aes=F) + 
        geom_line(data=tmp, aes(x=x, y=upr), linetype=2, inherit.aes=F)
    }
  }
  
  if("Ellipses"%in%stats){
    if("Color"%in%aes){
      if("Shade Ellipses"%in%stats){
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(fill=factor(group), colour=factor(group)), alpha=0.1) + 
          scale_fill_manual(values=colorp)
      } else {
        plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(colour=factor(group)), alpha=0.2, fill="transparent")
      }
    } else if("Shape"%in%aes){
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), 
                                  colour="grey15", fill="transparent")
    } else {
      plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), 
                                  colour="grey15", fill="transparent")
    }
  }

  if("Shade Ellipses"%in%stats & "Ellipses" %in% stats){
    plot <- plot + stat_ellipse(geom="polygon", level=.9, aes(group=factor(group)), alpha=0.1, fill="black", color="transparent") 
  }
  
  # points on top of everything
  # Set Aesthetics
  if(length(aes)==0){
    plot <- plot + geom_point(size=pointsize, shape=1) + 
      scale_shape_discrete(solid=F)
  } else if(length(aes)==1){
    if("Color"%in%aes){
      plot <- plot + geom_point(aes(color=factor(group)), size=pointsize, shape=1) + 
        scale_color_manual(values=colorp)
    } else {
      plot <- plot + geom_point(aes(shape=factor(group)), size=pointsize) + 
        scale_shape_manual(values=shapep)
    }
  } else {
    plot <- plot + geom_point(aes(color=factor(group), shape=factor(group)), size=pointsize) + 
      scale_color_manual(values=colorp) + 
      scale_shape_manual(values=shapep)
  }

  
  plot
}

cluster <- function(dframe) {
  # we assume to have x, y, and a group variable
  xmean <- mean(dframe$x)
  ymean <- mean(dframe$y)
  dframe$dist <- with(dframe, (x-xmean)^2 + (y-ymean)^2)
  SSTotal <- sum(dframe$dist)
  dframe <- ddply(dframe, .(group), transform, xgroup=mean(x), ygroup=mean(y))
  dframe$gdist <- with(dframe, (x-xgroup)^2 + (y-ygroup)^2)
  SSGroup <- sum(dframe$gdist)
  (SSTotal - SSGroup)/SSTotal
}

save.pics <- function(df, datastats, plotparms, plotname){
  dataname <- sprintf("set-%d-k-%d-sdline-%.2f-sdgroup-%.2f", i, 
                      datastats$K, datastats$sd.trend, datastats$sd.cluster)
  realfname <- sprintf("set-%d-plot-%d-k-%d-sdline-%.2f-sdgroup-%.2f", i, j, 
                       datastats$K, datastats$sd.trend, datastats$sd.cluster)
  fname <- digest(realfname, serialize=FALSE)
  
  plotobj <- gen.plot(df, aes=get.aes(plotparms), stats=get.stats(plotparms))
  
  if(plotname=="plain") {
    write.csv(df, file = paste0("Images/Lineups/Data/", dataname, ".csv"), row.names=FALSE)
  }
  ggsave(plotobj, filename=paste0("Images/Lineups/", fname, ".pdf"), width=6, height=6, dpi=100)
  #   ggsave(plotobj, filename=paste0("Images/Lineups/", fname, ".png"), width=6, height=6, dpi=100)
  
  interactive_lineup("print", plotobj,
                     filename=paste0("Images/Lineups/", fname, ".svg"), 
                     script="http://www.hofroe.net/examples/lineup/fhaction.js")
  
  data.frame(
    pic_id = unique(df$set),
    sample_size = datastats$K,
    test_param = sprintf("turk16-%s", plotname),
    param_value = sprintf("k-%d-sdline-%.2f-sdgroup-%.2f", datastats$K, datastats$sd.trend, datastats$sd.cluster),
    p_value = sprintf("line-%.5f-cluster-%.5f", datastats$line, datastats$cluster),
    obs_plot_location = sprintf("%d, %d", datastats$lineplot, datastats$groupplot),
    pic_name = paste0("Images/Lineups/", fname, ".svg"),
    experiment = "turk16",
    difficulty = unique(df$set),
    data_name = dataname
  )
}
