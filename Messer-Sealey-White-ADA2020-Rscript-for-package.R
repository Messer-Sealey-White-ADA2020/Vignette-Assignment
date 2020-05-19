library(igraph)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(visNetwork) 

### Data 
x <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)

### Part of the function that works
melt_matrix_4_visNetwork <- function(z){
  colnames(x) <- c("names", 1:length(x))
  x <- select(x, -names) %>%
  as.matrix()
  x[lower.tri(x, diag=TRUE)] <- NA
  x <- melt(x)
  x <- setNames(x, c('ind1', 'ind2', 'values'))
  x <- filter(x, !is.na(values))
  return(x)
}

new <- melt_matrix_4_visNetwork(x)  
head(new)

### PLaying around with qgraph package
library(qgraph)
library(reshape2)

Monkeys <- read_csv("EM_monkey.csv", col_names=TRUE)
Sname <- c(Monkeys$name)
Sno <- c(1:length(Monkeys))
Source <- cbind(Sname,Sno)
colnames(Monkeys) <-  c("name", 1:length(Monkeys))
Monkeys <- select(Monkeys, -name)
Monkeys <- as.matrix(Monkeys)
Monkeys[lower.tri(Monkeys, diag=TRUE)] <- NA 
Monkeys <- setNames(melt(Monkeys), c('Source', 'Target', 'Weight'))
Monkeys <- filter(Monkeys, !is.na(Weight))
summary(Monkeys)
qgraph(Monkeys, directed = FALSE, layout = "spring")

Source

aerial <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)
Sname <- c(aerial$names)
Sno <- c(1:length(aerial))
Source <- cbind(Sname,Sno)
colnames(aerial) <-  c("name", 1:length(aerial))
aerial <- select(aerial, -name)
aerial <- as.matrix(aerial)
aerial[lower.tri(aerial, diag=TRUE)] <- NA 
aerial <- setNames(melt(aerial), c('Source', 'Target', 'Weight'))
aerial <- filter(aerial, !is.na(Weight))
summary(aerial)
qgraph(aerial, directed = FALSE, layout = "spring")


# function play 
Monkeys <- read_csv("EM_monkey.csv", col_names=TRUE)

monkeymelt <- function(g){
  Sname <- c(Monkeys$name)
  Sno <- c(1:length(Monkeys))
  Source <- cbind(Sname,Sno)
  colnames(Monkeys) <-  c("name", 1:length(Monkeys))
  select(Monkeys, -name)
  as.matrix(Monkeys)
  Monkeys[lower.tri(Monkeys, diag=TRUE)] <- NA 
  setNames(melt(Monkeys), c('Source', 'Target', 'Weight'))
  filter(Monkeys, !is.na(Weight))
  qgraph(Monkeys, directed = FALSE, layout = "spring")
}

mm <- monkeymelt(g)

