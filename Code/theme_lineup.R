require(ggplot2)

theme_lineup <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(legend.position="none", 
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank())}