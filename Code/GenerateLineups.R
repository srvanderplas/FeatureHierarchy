source("./Code/MixtureLineups.R")
source(("./Code/theme_lineup.R")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(nullabor)

# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("./Data/color-perceptual-kernel.csv")
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("./Data/shape-perceptual-kernel.csv")
# shapetm[9:10,] <- 0
# shapetm[, 9:10] <- 0
shapetm[9,] <- 0
shapetm[,9] <- 0
shapetm[10,] <- 0
shapetm[,10] <- 0

# Lineup Design

N <- 3
data.parms <- expand.grid(N=c(40, 60, 80, 100),
                          K=3:5,
                          sd=c(.3, .35, .4),
                          q=c(.3, .35, .4))

plot.parms <- expand.grid(
  color = c(0,1),
  shape = c(0,1),
  reg = c(0,1),
  err = c(0,1),
  ell = c(0,1)
)[c(
  1, # control
  2, 3, 17, # color, shape, ellipse alone
  4, 18, # color + shape, color + ellipse
  5, 13, # trend, trend + error
  6, 14 # color + trend, color + trend + error
  ),]

get.aes <- function(r){
  c("Color", "Shape")[which(as.logical(r[1:2]))]
}

get.stats <- function(r){
  c("Reg. Line", "Error Bands", "Ellipses")[which(as.logical(r[3:5]))]
}

data <- ldply(1:nrow(data.parms), function(i) {data.frame(set=i, gen.data(as.list(data.parms[i,])))})

data.stats <- ddply(data, .(set, .sample), summarize, 
                    linear.r2 = round(summary(lm(y~x))$r.squared, 4),
                    group.r2 = round(summary(lm(y~factor(group)))$r.squared, 4))

d_ply(data, .(set), function(df){
  i <- unique(df$set)
  for(j in 1:nrow(plot.parms)){
    
    ggsave(plot = gen.plot(df, get.aes(plot.parms[j,]), 
                           get.stats(plot.parms[j,])), 
           filename = sprintf("Images/Lineups/set_%d_plot%d.png", i, j), 
           width=6, height=6, units="in")
  }
})

parms$groupName <- with(parms, paste0("n=", n, "-K=", K, "-SDgroup=", SDgroup, "-SDdata=", SDdata))

n.plotsets <- nrow(parms)
idxs <- data.frame(start=seq(1, N*(n.plotsets-1)+1, by=N),
                   end=seq(N, N*(n.plotsets), by=N))

# seeds <- sample(0:1e6, N*3*4) + sample(0:1e3, N*3*4) + sample(0:100, N*3*4)
seeds <- c(699181, 190785, 746705, 938857, 256246, 130991, 829750, 107019, 310685, 968407, 
           982098, 247471, 658815, 828038, 956567, 706934,  74904, 743373, 955470, 339582, 
           230370, 594429, 959280, 677468, 301141, 523820, 397938, 952596, 726989, 247199,
           466865, 221146, 717741, 788710, 751081, 549865, 866498, 941755, 930329, 718688,
           244210, 986750, 477916, 410092,  25072, 416778,  15301, 538460, 974894, 106861,
           980644, 839641, 571738, 362254, 514916, 941565, 110619, 811219, 289144, 140379,
           754422, 142313, 492449, 679018, 603089, 561658, 744639, 843383,  46659, 684303, 
           721195, 295297,  16830, 659357,  74917, 166777, 177777, 585689, 766075, 914235, 
            69910, 215165, 151981, 328809, 678807, 510021,  13994, 319936, 213642, 356772, 
           776940, 262663, 278632, 895642, 479173, 627829, 373171, 284299, 920339, 744813, 
           853463, 911801, 450574, 534677, 158427, 728388, 675524, 381373, 252604, 127060, 
           726349, 754596,  28725, 507875, 407721, 667116, 332922, 688253, 424027, 363511, 
           697796, 298190,  74882, 180266, 314639, 935880, 565868, 336255, 290223, 635180, 
           268594, 290632, 139770, 830747, 842288, 704105, 502931, 569567, 804279, 218429, 
           331136, 462246, 786172, 974956, 981157, 351272, 948544, 753484, 700547, 603588, 
           419920, 192648,  36668,  79884, 528699, 811573, 709611, 604548, 846498, 209019, 
           151445, 133552, 179530,  46216, 291842, 137859, 237709, 130710, 669324, 573619, 
           452843, 993606, 274835, 857082, 380085, 711804, 740789, 105443, 609809, 507201, 
           105960, 620851,  25108, 308780, 964310, 142026,  12242, 170189, 505509, 915185, 
           784952, 128410,    558, 971616, 539486, 616030, 449121, 952850, 407931, 718697, 
           456596, 833674, 918027, 594316, 924058,  46342, 862938, 268262, 939725, 768911, 
           780771, 293950, 427970, 426233, 249353, 520909, 844212, 212696, 664797, 234677, 
           775386, 229197, 637578, 169213, 443902, 275517, 519624, 426132, 404894, 864166, 
           435733, 934165,  61546, 920347, 843822, 844372,   8809, 254220, 922150, 78187)

answers <- data.frame()
data <- data.frame()
plots <- lapply(1:(N*7*n.plotsets), function(i) ggplot() + geom_blank())

for(i in 1:nrow(parms)){
  for(j in 1:N){
    tmp <- make.plot.set(seed=seeds[(i-1)*N+j], n=parms$n[i], ngroups = parms$K[i], sd.groups=parms$SDgroup[i], sd.data=parms$SDdata[i], group=parms$groupName[i])
    plots[((((i-1)*N+j)-1)*7+1):(((i-1)*N+j)*7)] <- tmp$plots
    answers <- rbind.fill(answers, tmp$answers)
    data <- rbind.fill(data, tmp$data)
  }
}


names <- paste0("./Images/Lineups/", rep(1:(N*7), each=nrow(parms)), "-", rep(1:(nrow(parms)), times=(N*7)), ".png")
names <- data.frame(filename = sample(names, 7*N*nrow(parms)), plot.idx=1:(7*N*nrow(parms)), stringsAsFactors=FALSE)
answers <- cbind(names, answers)
answers <- answers[order(answers$plot.idx),]
data <- merge(data, answers[,c("name", "plot.idx", "filename")])

library(doMC)
registerDoMC(8)
tmp <- llply(answers$plot.idx, function(i) 
  ggsave(plots[[i]], filename = names[i,"filename"], width=10, height=8, dpi=300, units="in"), .parallel=TRUE)

data.summary <- ddply(data, .(plot.idx, filename, name, .sample), function(df){
  model <- lm(data=df, y~x)
  slope <- model$coefficients[2]
  slope.t <- summary(model)$coefficients[2,3]
  slope.p <- summary(model)$coefficients[2,4]
  slope.r2 <- summary(model)$r.squared
  tmp <- unique(as.character(df$name))
  model2 <- lm(data=df, y~0+factor(group))
  group.mse <- anova(model2)$`Mean Sq`[2]
  group.p <- anova(model2)$`Pr(>F)`[1]
  group.r2 <- summary(model2)$r.squared
  data.frame(slope=slope, slope.t=slope.t, slope.p=slope.p, slope.r2=slope.r2,
             group.mse=group.mse, group.p=group.p, group.r2=group.r2, 
             n=nrow(df), ngroups=length(unique(df$group)))
})
data.summary <- merge(data.summary, answers)
data.summary$Type1 <- c("", "Slope")[grepl("Slope", data.summary$type)+1]
data.summary$Type2 <- gsub("Slope", "", data.summary$type)

save(plots, names, answers, data.summary, data, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)
