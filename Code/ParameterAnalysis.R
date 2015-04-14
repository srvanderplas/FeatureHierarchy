# Stats-analysis
# Compare statistics for each lineup data set


source("MixtureLineups.R")
source("theme_lineup.R")

setwd("../Images/Turk16/")
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(nullabor)
library(grid)
library(digest)
library(Cairo)

# Define colors and shapes
colors <-  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
             "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

shapes <- c(1,0,3,4,8,5,2,6,-0x25C1, -0x25B7)

colortm <- read.csv("../../Data/color-perceptual-kernel.csv")
# colortm[3,4] <- 0
# colortm[4,3] <- 0
colortm[8,] <- 0
colortm[,8] <- 0

shapetm <- read.csv("../../Data/shape-perceptual-kernel.csv")
# shapetm[9:10,] <- 0
# shapetm[, 9:10] <- 0
shapetm[9,] <- 0
shapetm[,9] <- 0
shapetm[10,] <- 0
shapetm[,10] <- 0

# function to create a list of chosen aesthetics
get.aes <- function(r){
  c("Color", "Shape")[which(as.logical(r[1:2]))]
}

# function to create a list of chosen statistics
get.stats <- function(r){
  c("Reg. Line", "Error Bands", "Ellipses")[which(as.logical(r[3:5]))]
}
# answers <- read.csv("picture-details.csv")
# # answers$pic_name <- gsub("Images/Lineups/", "", gsub("svg", "pdf", answers$pic_name))
# answers2 <- answers
# 
# for(i in 1:nrow(answers)){
#   df <- read.csv(sprintf("./Data/%s.csv", answers$data_name[i]), stringsAsFactors=FALSE)
#   
#   stats <- ddply(df, .(.sample, target1, target2), eval.df)
#   
#   write.csv(stats, file=sprintf("./Stats/%s.csv", answers$data_name[i]))
#   
#   plottype <- gsub("turk16-", "", answers$test_param[i])
#   
#   plot.parms <- c(grepl("[Cc]olor", plottype), grepl("[Ss]hape", plottype), grepl("[Tt]rend", plottype), grepl("Error", plottype), grepl("Ellipse", plottype))
#   
#   plotans <- sprintf("%d, %d, %d", unique(df$target1), unique(df$target2), which.min(stats$gini))
#   answers2[i,"obs_plot_location"] <- plotans
# #   plot <- gen.plot(df, aes=get.aes(plot.parms), stats=get.stats(plot.parms)) + 
# #     ggtitle(sprintf("Data Targets: %s, File Targets: %s", plotans, answers$obs_plot_location[i]))
# #   print(plot)
# }
# 
# # write.csv(answers2, "data-picture-details-gini.csv", row.names=FALSE)

answers2 <- read.csv("data-picture-details-gini.csv", stringsAsFactors = F)
res <- ldply(unique(answers2$data_name), function(x){
  return(cbind(data_name = x, read.csv(paste0("./Stats/", x, ".csv"))))
})

library(stringr)
res$target <- paste0(c("", "trend")[(res$.sample==res$target1)+1], c("", "cluster")[(res$.sample==res$target2) + 1])
res$target[res$target==""] <- "decoy"

res$set <- str_extract(res$data_name, pattern="set-\\d{1,2}") %>% str_replace(pattern="set-", "") %>% as.numeric()
res$k <- str_extract(res$data_name, pattern="k-\\d") %>% str_replace(pattern="k-", "") %>% as.numeric()
res$sd.line <- str_extract(res$data_name, pattern="sdline-0.\\d{1,2}") %>% str_replace(pattern="sdline-", "") %>% as.numeric()
res$sd.group <- str_extract(res$data_name, pattern="sdgroup-0.\\d{1,2}") %>% str_replace(pattern="sdgroup-", "") %>% as.numeric()

ggplot(data=res) + 
  geom_jitter(aes(x=line, y=sd.line, color=target)) + 
  facet_grid(target ~ k+sd.group)

ggplot(data=res) + 
  geom_jitter(aes(x=group, y=sd.group, color=target)) + 
  facet_grid(target ~ k+sd.line)
