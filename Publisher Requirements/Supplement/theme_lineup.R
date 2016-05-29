require(ggplot2)
require(grid)

theme_lineup <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(legend.position="none", 
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          plot.margin=unit(c(1,1,0,0), "line")
    )}