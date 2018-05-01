library(igraph)

s1<-matrix(c(0,1,0,-1,0,1,0,-1,0),nrow=3,ncol=3)
s2<-matrix(c(0,1,1,-1,0,1,-1,-1,0),nrow=3,ncol=3)
s3<-matrix(c(0,1,-1,-1,0,1,1,-1,0),nrow=3,ncol=3)
s4<-matrix(c(0,1,1,-1,0,0,-1,0,0),nrow=3,ncol=3)
s5<-matrix(c(0,0,1,0,0,1,-1,-1,0),nrow=3,ncol=3)

d1<-matrix(c(0,1,1,-1,0,1,-1,1,0),nrow=3,ncol=3)
d2<-matrix(c(0,1,1,1,0,1,-1,-1,0),nrow=3,ncol=3)
d3<-matrix(c(0,1,-1,1,0,0,1,0,0),nrow=3,ncol=3)
d4<-matrix(c(0,1,1,-1,0,0,1,0,0),nrow=3,ncol=3)
d5<-matrix(c(0,1,1,-1,0,1,1,-1,0),nrow=3,ncol=3)
d6<-matrix(c(0,1,1,1,0,1,1,1,0),nrow=3,ncol=3)
d7<-matrix(c(0,1,1,1,0,1,1,-1,0),nrow=3,ncol=3)
d8<-matrix(c(0,1,1,1,0,0,1,0,0),nrow=3,ncol=3)

mot.lst <- list(s1, s2, s3, s4, s5, d1, d2, d3, d4, d5, d6, d7, d8)
names(mot.lst) <- c("s1", "s2", "s3", "s4", "s5", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8")

eig.analysis <- function(n, matrices){
    cols <- length(matrices)
    rows <- n
    eigenMATRIX <- matrix(0, nrow = rows, ncol = cols)
    for(i in 1:n){
        ranmat <- lapply(matrices, ran.unif)
        eigs <- sapply(ranmat, maxRE)
        eigenMATRIX[i,] <- eigs
    }
    return(eigenMATRIX)
}

ran.unif <- function(motmat){
    newmat <- apply(motmat, c(1,2), function(x){
        if(x==1){runif(1, 0, 10)}else if(x==-1){runif(1, -1, 0)} else{0}
    })
    diag(newmat) <- runif(3, -1, 0)
    return(newmat)
}

maxRE <- function(rmat){
    lam.max <- max(Re(eigen(rmat, only.values=TRUE)$values))
    return(lam.max)
}


n <- 10000
mot.stab<- eig.analysis(n, mot.lst)
colnames(mot.stab) <- names(mot.lst)

mot.qss <- apply(mot.stab, 2, function(x){sum(x<0)/n})
sorted <- sort(mot.qss, decreasing=TRUE)

svg(filename="../Images/motifs_line.svg", width=7, height=1, bg="transparent")
par(mfrow = c(1, 13), mar = c(0.2, 0.2, 0.2, 0.2))
for (i in 1:13) {
    plot.igraph(graph.adjacency(mot.lst[[which(names(mot.lst) == names(sorted[i]))]]),
                layout = layout.circle,
                vertex.size=40, vertex.color="#8a9aed", vertex.label=NA, vertex.frame.color=NA,
                edge.arrow.size=0.5, edge.width=2, edge.color="#f4e6d4")
}
dev.off()
