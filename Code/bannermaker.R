library(ggplot2)
library(reshape2)

S <- 100
mat <- matrix(runif(S*S/4, -1, 1), S, S/4)
plotdata <- melt(mat)
ggplot(plotdata, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(colour='black') +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    scale_fill_gradient2(high='steelblue4', mid='white', low='firebrick4', midpoint=0) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          legend.position='none')
ggsave('../Images/MatBanner.png', width=12, height=3, units='in', dpi=600)


S <- 100
mat <- matrix(runif(S*S/4, -1, 1), S, S/4)
for (ii in 1:nrow(mat)) {
    for (jj in 1:ncol(mat)) {
        if (jj <= ii/4) mat[ii,jj] <- mat[ii,jj] - 1 else mat[ii,jj] <- mat[ii,jj] + 1
    }
}
plotdata <- melt(mat)
ggplot(plotdata, aes(x=Var1, y=-Var2, fill=value)) +
    geom_tile(colour='black') +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    scale_fill_gradient2(high='steelblue4', mid='white', low='firebrick4', midpoint=0) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          legend.position='none')
ggsave('../Images/MatBanner2.png', width=12, height=3, units='in', dpi=600)


S <- 100
mat <- matrix(runif(S*S/4, -1, 1), S, S/4)
for (ii in 1:nrow(mat)) {
    for (jj in 1:ncol(mat)) {
        if ((jj < ncol(mat)/2 & ii < nrow(mat)/2) |
            (jj >= ncol(mat)/2 & ii >= nrow(mat)/2)) {
            mat[ii,jj] <- mat[ii,jj] - 2
        } else mat[ii,jj] <- mat[ii,jj] + 0.5
    }
}
plotdata <- melt(mat)
ggplot(plotdata, aes(x=Var1, y=-Var2, fill=value)) +
    geom_tile(colour='black') +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
    scale_fill_gradient2(high='steelblue4', mid='white', low='firebrick4', midpoint=0) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          legend.position='none')
ggsave('../Images/MatBanner3.png', width=12, height=3, units='in', dpi=600)
