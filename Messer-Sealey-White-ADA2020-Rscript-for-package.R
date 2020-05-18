library(igraph)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(visNetwork) 

x <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)

### Tidying Data for analysis  
#melting data matrix for aerial treatment
colnames(aerial) <-  c("names", 1:length(aerial))
cleanaerial <- select(aerial, -names)
head(cleanaerial)
cleanaerial <- as.matrix(cleanaerial)
cleanaerial[lower.tri(cleanaerial, diag=TRUE)] <- NA
meltaerial <- setNames(melt(cleanaerial), c('ind1', 'ind2', 'values'))
head(meltaerial)
meltaerial <- filter(meltaerial, !is.na(values))
summary(meltaerial)
###


### Part of the function that works
melt_matrix_4_visNetwork <- function(z){
  colnames(x) <- c("names", 1:length(x))
  select(x, -names)
  setNames(melt(x), c('ind1', 'ind2', 'values'))
}

new <- melt_matrix_4_visNetwork(x)  
head(new)


### What didn't work in the function 
x[lower.tri(x, diag=TRUE)] %>% NA
setNames(melt(x), c('ind1', 'ind2', 'values'))
filter(x, !is.na(values))


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
