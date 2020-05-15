library(igraph)

v <- read.csv("vipers.csv")

edges1 <- (v$A)
nodes1 <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
target <- c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A")

nte <- cbind(nodes1, target, edges1)

id <- c(1:26)

idnodes <- cbind(nodes1, id)

v <- graph_from_data_frame(d=nte, vertices=idnodes, directed=FALSE)
plot(v)
