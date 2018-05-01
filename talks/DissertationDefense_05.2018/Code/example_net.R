library(igraph)

set.seed(4)
N <- 5
C <- 0.5
mat<-matrix(0,nrow=N,ncol=N)
mat[upper.tri(mat, diag=TRUE)] <- rbinom(N * (N - 1) / 2 + N, 1, C)
mat <- mat + t(mat)
mat <- 1 * (mat != 0)

svg(filename="../Images/nodes.svg", width=4, height=4, bg="transparent")
plot.igraph(graph.adjacency(mat, mode="undirected"), layout=layout.circle,
            vertex.size=40, vertex.color="#8a9aed", vertex.label=NA, vertex.frame.color=NA,
            edge.arrow.size=1, edge.width=3, edge.color=NA)
dev.off()
svg(filename="../Images/links.svg", width=4, height=4, bg="transparent")
plot.igraph(graph.adjacency(mat, mode="undirected"), layout=layout.circle,
            vertex.size=40, vertex.color=NA, vertex.label=NA, vertex.frame.color=NA,
            edge.arrow.size=1, edge.width=3, edge.color="#f4e6d4", edge.loop.angle=-pi/2)
dev.off()
svg(filename="../Images/degree.svg", width=4, height=4, bg="transparent")
plot.igraph(graph.adjacency(mat, mode="undirected"), layout=layout.circle,
            vertex.size=40, vertex.color=NA, vertex.frame.color=NA, vertex.label.color="#f4e6d4", vertex.label=rowSums(mat),
            edge.arrow.size=1, edge.width=3, edge.color=NA, edge.loop.angle=-pi/2)
dev.off()


sum(mat) / N^2
