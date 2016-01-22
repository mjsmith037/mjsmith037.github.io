rm(list=ls())
#setwd('~/Documents/CountriesCitations/Figures/Code/')

library(ggplot2)
library(grid)

fulldata <- read.csv('~/Research/CountriesCitations/Figures/Code/CitePlotData.txt')
## make country names prettier
#data$country <- gsub("', '", "+",data$country)
fulldata$country <- gsub("[\\[\\]']", "", fulldata$country, perl=T)
PlotData <- fulldata[(fulldata$field == "Ecology") & (fulldata$country == "United States"),]
ggplot(PlotData, aes(x=year-.5)) +
    geom_rect(aes(ymin=botci, ymax=topci, xmin=year-.5, xmax=year+.5), fill=I('blue'), alpha=.3) +
    geom_step(aes(y=median), colour=I('black'), linetype=6, size=1) +
    geom_step(aes(y=propcites), colour=I('#D55E00'), size=1) +
    geom_step(aes(y=proppapers), colour=I('darkred'), linetype=5, size=1) +
    coord_cartesian(xlim=c(1996,2012)) +
    scale_x_continuous(breaks=c(seq(1996,2012,5))) +
    scale_color_hue(l=45) +
    facet_grid(country~field) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          strip.background = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "in"),
          strip.text = element_text(size=16))
ggsave(file="../Images/cclineplot.png", width=4, height=2.7)
