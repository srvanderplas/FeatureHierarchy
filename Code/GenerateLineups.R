source("./Code/LineupFunctions.R")

N <- 5
# seeds <- sample(0:1e6, N*3*4) + sample(0:1e3, N*3*4) + sample(0:100, N*3*4)
seeds <- c(699181, 190785, 746705, 938857, 256246, 
           130991, 829750, 107019, 310685, 968407, 
           982098, 247471, 658815, 828038, 956567, 
           706934,  74904, 743373, 955470, 339582, 
           230370, 594429, 959280, 677468, 301141, 
           523820, 397938, 952596, 726989, 247199,
           466865, 221146, 717741, 788710, 751081,
           549865, 866498, 941755, 930329, 718688,
           244210, 986750, 477916, 410092,  25072,
           416778,  15301, 538460, 974894, 106861,
           980644, 839641, 571738, 362254, 514916,
           941565, 110619, 811219, 289144, 140379)

answers <- data.frame()
data <- data.frame()
plots <- lapply(1:(N*7*3*4), function(i) ggplot() + geom_blank())

for(i in 1:N){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-NoOutliers-sd=3", n=30, sd.data=3, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (N+1):(2*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-NoOutliers-sd=5", n=30, sd.data=5, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (2*N+1):(3*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-NoOutliers-sd=7", n=30, sd.data=7, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}


for(i in (3*N+1):(4*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-3Outliers-sd=3", n=50, sd.data=3, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (4*N+1):(5*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-3Outliers-sd=5", n=50, sd.data=5, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (5*N+1):(6*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 3, group = "3Groups-3Outliers-sd=7", n=50, sd.data=7, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}



for(i in (6*N+1):(7*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-NoOutliers-sd=3", n=50, sd.data=3, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (7*N+1):(8*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-NoOutliers-sd=5", n=50, sd.data=5, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (8*N+1):(9*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-NoOutliers-sd=7", n=50, sd.data=7, n.outliers=0)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}



for(i in (9*N+1):(10*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-3Outliers-sd=3", n=50, sd.data=3, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (10*N+1):(11*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-3Outliers-sd=5", n=50, sd.data=5, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}
for(i in (11*N+1):(12*N)){
  tmp <- make.plot.set(seed=seeds[i], ngroups = 5, group = "5Groups-3Outliers-sd=7", n=50, sd.data=7, n.outliers=3)
  plots[((i-1)*7+1):(i*7)] <- tmp$plots
  answers <- rbind(answers, tmp$answers)
  data <- rbind.fill(data, tmp$data)
}

names <- paste0("./Images/Lineups/", rep(1:(N*3), each=7*4), "-", rep(1:(7*4), times=(N*3)), ".png")
names <- data.frame(filename = sample(names, 7*N*3*4), plot.idx=1:(7*N*3*4), stringsAsFactors=FALSE)
answers <- cbind(names, answers)
answers <- answers[order(answers$plot.idx),]
data <- merge(data, answers[,c("name", "plot.idx", "filename")])

lapply(answers$plot.idx, function(i) 
  ggsave(plots[[i]], filename = names[i,"filename"], width=10, height=8, dpi=300, units="in"))

save(plots, names, answers, data, file="./Images/Lineups/Lineups.rda")
write.csv(answers, "./Images/Lineups/LineupKey.csv", row.names=FALSE)

