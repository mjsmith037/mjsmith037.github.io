library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
theme_set(theme_bw())

########## General initialization code used for following operations  ##########
S <- 50

Dist <- "Uniform2"
ones <- matrix(runif(S^2/4, -1, 0), S/2, S/2)
zeros <- matrix(runif(S^2/4, 0, 1), S/2, S/2)
ones.diag <- ones
diag(ones.diag) <- 0

coordBip <- expand.grid(1:25,1:25)
coordMod <- coordBip[coordBip$Var1 != coordBip$Var2,]

########## Figure to show trend in full eigenvalue  ##########
########## distribution along with matrix structure ##########
swapped <- c(1, sum(ones.diag != 0))
MatPlotData <- data.frame()
for (nswap in swapped) {
    modPart <- ones.diag
    bipPart <- zeros
    toswapMod <- sample(1:nrow(coordMod), nswap)
    toswapBip <- sample(1:nrow(coordBip), nswap)
    ## switch a given number of elements from the modular part to the bipartite
    ## part, being careful not to swap diagnoal entries from modular parts
    for (nn in 1:length(toswapMod)) {
        tmpM <- modPart[coordMod[toswapMod[nn], 1], coordMod[toswapMod[nn], 2]]
        modPart[coordMod[toswapMod[nn], 1],
                coordMod[toswapMod[nn], 2]] <- bipPart[coordBip[toswapBip[nn], 1],
                                                       coordBip[toswapBip[nn], 2]]
        bipPart[coordBip[toswapBip[nn], 1], coordBip[toswapBip[nn], 2]] <- tmpM
    }
    ## combine to make matrix and then calculate lambda_1
    modPart.downright <- modPart.upleft <- modPart
    modPart.upleft[lower.tri(modPart)] <- 0
    modPart.downright[upper.tri(modPart)] <- 0
    modPart.upleft <- modPart.upleft + t(modPart.upleft)
    modPart.downright <- modPart.downright + t(modPart.downright)
    tmpMat <- rbind(cbind(modPart.upleft, bipPart),
                    cbind(bipPart, modPart.downright))
    meltedmat <- melt(tmpMat[nrow(tmpMat):1,])
    meltedmat$Mat <- nswap
    MatPlotData <- rbind(MatPlotData, meltedmat)
}
## generate matrix glob
q1 <- ggplot(MatPlotData[MatPlotData$Mat == swapped[1],], aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(colour = "black") +
    scale_fill_gradient(low = "darkred", high = "white") +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    theme(legend.position = "none",
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin=unit(c(0,0,0,0), "in"))
q2 <- ggplot(MatPlotData[MatPlotData$Mat == swapped[2],], aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(colour = "black") +
    scale_fill_gradient(low = "darkred", high = "white") +
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    theme(legend.position = "none",
          plot.background = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin=unit(c(0,0,0,0), "in"))

load(paste0("~/Research/Intra-Inter/Figures/ModAxis/ModularityAxis-", Dist, ".RData"))
p <- ggplot(LinePlotData, aes(x=nswap, y=eig, colour=ll)) +
    geom_point(alpha=.05) +
    geom_smooth(size=1) +
    ylab(expression(lambda)) +
    xlab(paste0("Modularity <",  paste(rep.int("-", 10), collapse='-'), "> Bipartism")) +
    theme(legend.position="none",
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_line(size=2),
          axis.text.y=element_text(size=20),
          axis.title=element_text(size=20))

vp1 <- viewport(width = 0.08, height = 0.3, x = 0.085, y = 0.8)
vp2 <- viewport(width = 0.08, height = 0.3, x = 0.935, y = 0.215)
png("../Images/modaxis.png", width=400, height=250)
print(p)
# print(q1, vp = vp1)
# print(q2, vp = vp2)
dev.off()
