library(igraph)

v <- read.csv("vipers_hook.csv")

weight <- (v$A)
nodes1 <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
target <- c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A")

nte <- cbind(nodes1, target, weight)
head(nte)

id <- c(1:26)

idnodes <- cbind(nodes1, id)
head(idnodes)

v4plot <- graph_from_data_frame(d=nte, vertices=idnodes, directed=FALSE)
plot(v4plot)

library(ggraph)

cos_mat <- cosine_matrix(dat, lower = .01, upper = .80, filt = .80)

set.seed(1839)
ggraph(v, layout = "nicely") +
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) + 
  geom_node_label(aes(label = name)) +
  theme_void()
