palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
palette.options <- expand.grid(c1=1:10, c2=1:10, c3=1:10, c4=1:10)
palette.options <- with(palette.options, palette.options[c1!=c2 & c2!=c3 & c1!=c3 & c1!=c4 & c2!=c4 & c3!=c4,])

colortm <- colortm2 <- read.table("~/Documents/Rprojects/perceptual-kernels/data/kernels/color-tm.txt", sep=",")
colortm2$id <- 1:10
write.csv(colortm2[, c(11, 1:10)], file="~/Documents/Rprojects/FeatureHierarchy/Data/color-perceptual-kernel.csv")

palette.options$dist2way <- with(palette.options, 
                                 as.numeric(unlist(colortm))[(c1-1)*10+c2]) 
palette.options$dist3way <- with(palette.options, 
                                 as.numeric(unlist(colortm))[(c1-1)*10+c2] + 
                                 as.numeric(unlist(colortm))[(c2-1)*10+c3] +
                                 as.numeric(unlist(colortm))[(c1-1)*10+c3]) 
palette.options$dist4way <- 
  with(palette.options, 
       dist3way + 
         as.numeric(unlist(colortm))[(c4-1)*10+c1] + 
         as.numeric(unlist(colortm))[(c4-1)*10+c2] +
         as.numeric(unlist(colortm))[(c4-1)*10+c3]) 

palette.options[which.max(palette.options$dist2way),]
palette.options[which.max(palette.options$dist3way),]
subset(palette.options, dist3way>=2.78669)
