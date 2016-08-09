# PieColours <<- c("Primary_Producers"="3", "Herbivores"="4", "Consumers"="2", "Parasites"="1")
library(stringr)
library(hash)
library(xtable)

## options: list.files('../Images/animals/', 'svg')
## need to add tree, wolf/coyote, 
c("acorn.svg", "bear2.svg", "bear.svg", "beaver.svg", "deer2.svg", "deer.svg",
  "duck.svg", "fox2.svg", "fox.svg", "grass.svg", "hedgehog.svg", "owl.svg",
  "rabbit2.svg", "rabbit.svg", "raccoon2.svg", "raccoon.svg", "skunk.svg",
  "snake.svg", "squirrel2.svg", "squirrel.svg", "tick.svg", "worm.svg")
sppnames <- c("grass", "acorn", "rabbit2", "squirrel2", "deer2", "fox2", "owl", "bear2", "worm", "tick")
classes <-  c("pp",    "pp",    "hb",      "hb",        "hb",    "hb",   "pr",  "pr",    "pa",   "pa")

fw_dict <- hash(
    "acorn"=c("bear", "bear2", "deer", "deer2", "duck", "hedgehog", "raccoon",
              "raccoon2", "skkunk", "squirrel", "squirrel2"),
    "bear"=c("worm", "tick"),
    "bear2"=c("worm", "tick"),
    "beaver"=c("owl"),
    "deer"=c("bear", "bear2", "tick"),
    "deer2"=c("bear", "bear2", "tick"),
    "duck"=c("fox", "fox2", "fox2", "owl", "snake"),
    "fox"=c("bear", "bear2", "worm", "tick", "owl"),
    "fox2"=c("bear", "bear2", "worm", "tick", "owl"),
    "grass"=c("rabbit", "rabbit2", "bear", "bear2", "deer", "deer2", "hedgehog",
              "duck", "beaver", "skunk", "squirrel", "squirrel2"),
    "hedgehog"=c("fox", "fox2", "owl", "tick"),
    "owl"=c(),
    "rabbit"=c("bear", "bear2", "fox", "fox2", "owl", "worm", "tick", "snake"),
    "rabbit2"=c("bear", "bear2", "fox", "fox2", "owl", "worm", "tick", "snake"),
    "raccoon"=c("bear", "bear2", "tick"),
    "raccoon2"=c("bear", "bear2", "tick"),
    "skunk"=c("owl", "fox", "fox2", "tick", "bear", "bear2", "snake"),
    "snake"=c("fox", "fox2", "owl"),
    "squirrel"=c("bear", "bear2", "fox", "fox2", "owl", "tick", "raccoon", "raccoon2", "snake"),
    "squirrel2"=c("bear", "bear2", "fox", "fox2", "owl", "tick", "raccoon", "raccoon2", "snake"),
    "tick"=c(),
    "worm"=c()
)

fw_mat <- matrix(0, length(sppnames), length(sppnames))
dimnames(fw_mat) <- list(sppnames, sppnames)

GF <- "digraph G {"
GF <- rbind(GF,
            "bgcolor=transparent;",
            "fixedsize=true;",
            "rankdir=BT;")

# Draw the nodes
for (node in sppnames){
    GF <- rbind(GF,
                paste0(node,
                       "[image=\"../Images/animals/", node, ".svg\"",
                       ", label=\"\"",
                       ", shape=none",
                       "];")
                )
}
# Now the links
for (prey in sppnames){
    for (predator in fw_dict[[prey]]){
        if (predator %in% sppnames) {
            GF <- rbind(GF,paste0("\"", prey, "\" -> \"", predator, "\" [penwidth=5];"))
            fw_mat[prey, predator] <- 1
        } else next
    }
}
GF <- rbind(GF,"}")
FileName <- "../Figures/FoodWeb_animals.dot"

#### to make the dot plot
write.table(GF, FileName, quote=FALSE, row.names=FALSE, col.names=FALSE)
system(paste("dot", FileName, "-Tsvg -O "))
#### to save the matrix
write.table(fw_mat, row.names=FALSE, col.names=FALSE, file=paste0("../Code/Unsorted-", length(sppnames), ".txt"))
#### to print t he html matrix
print(xtable(fw_mat, digits=0), type="html")
#### and the other html matrices
g1 <- read.table(bestoutfile) %>% as.vector
g2 <- read.table(bestoutfile_DC) %>% as.vector

print(xtable(fw_mat[order(g1), order(g1)], digits=0), type="html")
print(xtable(fw_mat[order(g2), order(g2)], digits=0), type="html")


write.table(data.frame(classes, t(g1)), file="tmp.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)
write.table(data.frame(classes, t(g2)), file="tmp_DC.txt", col.names=FALSE, row.names=FALSE, quote=FALSE)

for (cc in c("pa", "pr", "hb", "pp", "all")) {
  for (ff in c("tmp.txt", "tmp_DC.txt")) {
    print(paste(cc, system(paste("julia RunImbalance.jl", ff, -1, cc, "."), intern=TRUE)))
  }
}
