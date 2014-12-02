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

palette.options[which.max(palette.options$dist2way),] # 3,7
palette.options[which.max(palette.options$dist3way),] # 3,4,8
palette.options[which.max(palette.options$dist4way),] # 3,4,7,8

subset(palette.options, dist3way>=2.78669)

palette <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)
palette.options <- expand.grid(c1=1:10, c2=1:10, c3=1:10, c4=1:10)
palette.options <- with(palette.options, palette.options[c1!=c2 & c2!=c3 & c1!=c3 & c1!=c4 & c2!=c4 & c3!=c4,])

shapetm <- shapetm2 <- read.table("~/Documents/Rprojects/perceptual-kernels/data/kernels/shape-tm.txt", sep=",")
shapetm2$id <- 1:10
write.csv(shapetm2[, c(11, 1:10)], file="~/Documents/Rprojects/FeatureHierarchy/Data/shape-perceptual-kernel.csv")

palette.options$dist2way <- with(palette.options, 
                                 as.numeric(unlist(shapetm))[(c1-1)*10+c2]) 
palette.options$dist3way <- with(palette.options, 
                                 as.numeric(unlist(shapetm))[(c1-1)*10+c2] + 
                                   as.numeric(unlist(shapetm))[(c2-1)*10+c3] +
                                   as.numeric(unlist(shapetm))[(c1-1)*10+c3]) 
palette.options$dist4way <- 
  with(palette.options, 
       dist3way + 
         as.numeric(unlist(shapetm))[(c4-1)*10+c1] + 
         as.numeric(unlist(shapetm))[(c4-1)*10+c2] +
         as.numeric(unlist(shapetm))[(c4-1)*10+c3]) 

palette.options[which.max(palette.options$dist2way),] # 10,5
palette.options[which.max(palette.options$dist3way),] # 10,3,1
palette.options[which.max(palette.options$dist4way),] # 1,10,5,3
subset(palette.options, dist3way>=2.92582)%>% arrange(dist3way)
