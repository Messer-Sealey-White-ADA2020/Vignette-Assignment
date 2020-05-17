library(igraph)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(visNetwork) 

x <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)

### Tidying Data for analysis  
melting data matrix for aerial treatment
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

