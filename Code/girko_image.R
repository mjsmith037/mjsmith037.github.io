library(ggplot2)
library(MASS)
library(grid)

eye.pairs.from.normal <- function(NumPairs = 10,
                                  mux = 0,
                                  muy = 0,
                                  sigmax = 1/4,
                                  sigmay = 1/4,
                                  rhoxy = -2/3){
    mus <- c(mux, muy)
    covariance.matrix <- matrix(c(sigmax^2,
                                  rhoxy * sigmax * sigmay,
                                  rhoxy * sigmax * sigmay,
                                  sigmay^2),
                                2, 2)
    Pairs <- mvrnorm(NumPairs, mus, covariance.matrix)
    return(Pairs)
}

ellipsefunction <- function(hlaxa = 1, hlaxb = 1, center = 0, npoints = 1000){
    a <- seq(0, 2 * pi, length = npoints + 1)
    x <- hlaxa * cos(a) + center
    y <- hlaxb * sin(a)
    return(data.frame(Real=x, Imaginary=y))
}


S <- 2000
C <- 1
rhoxy <- 0
sigma <- 0.25
dd <- -10

M <- matrix(0, S, S)
elements <- eye.pairs.from.normal(NumPairs = S * (S - 1) / 2, rhoxy=rhoxy)
M[upper.tri(M)] <- elements[,1]
M <- t(M)
M[upper.tri(M)] <- elements[,2]
diag(M) <- dd

eVals <- eigen(M, only.values=TRUE)$values
PointData <- data.frame(Real=Re(eVals), Imaginary=Im(eVals))
EllipseData <- ellipsefunction(hlaxa = sigma * sqrt(S * C) * (1 + rhoxy),
                               hlaxb = sigma * sqrt(S * C) * (1 - rhoxy),
                               center = dd)
ggplot() +
    geom_polygon(aes(x=Real, y=Imaginary), alpha=0.25, fill='steelblue4', data=EllipseData) +
    geom_polygon(aes(x=Real, y=Imaginary), colour='midnightblue', fill=NA, data=EllipseData) +
    geom_point(aes(x=Real, y=Imaginary), size=0.5, data=PointData) +
    geom_vline(xintercept=0, linetype='dashed', size=1) +
    xlim(c(2 * min(PointData$Imaginary), max(PointData$Imaginary))) +
    theme_bw() +
    theme(plot.margin=unit(c(0,0,0,0), 'in'))
ggsave(file='../Images/ellipse.png', width=4, height=2.7)
