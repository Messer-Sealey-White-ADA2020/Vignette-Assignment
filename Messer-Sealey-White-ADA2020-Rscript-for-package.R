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

