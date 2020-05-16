library(igraph)
library(tidyverse)
d <- read_csv("../Tactile_treatments/tactile_heads2020f.csv", col_names=TRUE)
d <- select(d, -names)
head(d)
d <- as.matrix(d)
d[lower.tri(d, diag=TRUE)] <- NA
library(reshape2)
r <- setNames(melt(d), c('ind1', 'ind2', 'values'))
head(r)
r <- filter(r, !is.na(values))
summary(r)

ind2 <- substring(r$ind2,1,11)

r2 <- data.frame(r$ind1, ind2, r$values)

p <- graph_from_data_frame(r, directed = FALSE, vertices = NULL)
p2 <- graph_from_data_frame(r2, directed = FALSE, vertices = NULL)
col <- c("coral", "cyan")
V(p2)$color <- col[V(p2)$ind2]
V(p2)$color

plot(p, vertex.label = NA)


#now we just need a function that calls these three packages and plots the graph


