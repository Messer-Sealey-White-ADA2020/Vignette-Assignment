library(igraph)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(visNetwork) 

### Data 
x <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)

### Part of the function that works
melt_matrix_4_visNetwork <- function(x){
  colnames(x) <- c("names", 1:nrow(x))
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
unique(new$ind1)
---
  title: "Messer-Sealey-White-ADA-2020-vignette-assignment"
author: "BA White, BA Sealey, E Messer"
date: "5/13/2020"
output: html_document
editor_options: 
  chunk_output_type: console
---
  # Objectives
  The objective of this module is to provide an overview to utilizing network analysis to visualize and analyze interaction, connected, or similarity data. Specifically, we present 1. Tools to modify a data matrix to generate network diagrams, 2. Applying various aesthetics to visNetwork diagrams, and 3. Generate Wilcoxon Test statistics make comparisons across groups.

# Preliminaries
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(igraph) # to make an igraph object ie. network visualization of matrix values
library(visNetwork) # to make your igraph object prettier using visNetwork
library(bbe2020) # adds functionality to melt by converting a matrix generated in other software or elsewhere and converting to a form easily used by igraph
```

# Introduce dataset 
A group of snakes were exposed to the presence of two "simulated" predators (i.e. two treatments), one with a simulated potential aerial "attack" and another with a simulated tactile "attack". A matrix was generated using Pearson correlation coefficient to calculate the behavior similarity indices between all snake's antipredator head movement behavior.  

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
# read in data matrix for aerial treatment
aerial <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)
# store the first column, which should be "names" for later use
aerialnames <- as.character(aerial$names)
# read in data matrix for tactile treatment 
tactile <- read_csv("tactiletreatments_heads2020f.csv", col_names=TRUE)
# store the first column, which should be "names" for later use
tactilenames <- as.character(tactile$names)

## Please Note: you must ensure that your dataframe has a first column named "names" that refers to the labels for the unique row and column data points 
```

# Using the Matrices, develop specific questions:
How similar did individual snakes move their heads in response to an aerial "attack"?
How similar did individual snakes move their heads in response to a tactile "attack"?
How does snake centrality of similarity compare between the aerial and tactile treatments? 

# Background
Networks are made up of two essential components, 'nodes' (represented by symbols) and ‘edges’ (represented by lines between the nodes). These lines typically indicate an interaction between the nodes. Usually, in the context of social networks, the nodes are individuals and the edges are some measured interaction (Croft, James & Krause, 2008). Social network visualizations are called sociograms and illustrate a graphical representation of the data (presented as nodes and edges). Network visualizations can be used to visualize many forms of interaction profile similarity or adjacency matrices in biological systems (Bass et al. 2013).   

# Tidying Data Matrix for iGraph Network analysis using bbe2020
```{r}
melt_matrix_4_ig_visV <- function(x){
  colnames(x) <- c("names", 1:nrow(x))
  x <- select(x, -names) %>%
  as.matrix()
  x[lower.tri(x, diag=TRUE)] <- NA
  x <- melt(x)
  x <- setNames(x, c('ind1', 'ind2', 'values'))
  x <- filter(x, !is.na(values))
  return(x)
}
# melting data matrix for aerial treatment
#meltaerial <- meltme(aerial)
meltaerial <- melt_matrix_4_ig_visV(aerial)
meltaerial
# melting data matirx for tactile treatment 
#melttactile <- meltme(tactile)
melttactile <- melt_matrix_4_ig_visV(tactile)
melttactile
```

# Data Visualization
Here we specifically visualize an individual snake's antipredator behavior divided by all individual's antipredator behavior towards the same type of predator represented as a ratio (or similarity index) in a sociogram. Each sociogram visualizes the similarity of antipredator behavior for either predator among the snakes. Edges represent the similarity index. 

```{r}
# iGraph network plot of aerial treatment
aerialplot <- graph_from_data_frame(meltaerial, directed = FALSE, vertices = NULL)
E(aerialplot)$weight = meltaerial$values
# print iGraph using a sphere layout 
plot(aerialplot, vertex.label = NA, layout = layout_on_sphere, main="Aerial treatment")

# iGraph network plot of tactile treatment
tactileplot <- graph_from_data_frame(melttactile, directed = FALSE, vertices = NULL)
E(tactileplot)$weight = melttactile$values
# print iGraph using a force-directed layout (positions the nodes so that the edges have similar lengths and there are as few crossing edges as possible)
plot(tactileplot, vertex.label = NA, layout = layout_with_fr, main="Tactile treatment")

# converting igraph to visnetwork for aerial treatment
aerialdata <- toVisNetworkData(aerialplot)
## copy column "weight" to new column "value" in list "edges"
aerialdata$edges$width <- 20*aerialdata$edges$value 
## line color
aerialdata$edges$color <- "gray" 
## print plot
visNetwork(nodes = aerialdata$nodes, edges = aerialdata$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")

# converting igraph to visnetwork for tactile treatment
tactiledata <- toVisNetworkData(tactileplot)
## copy column "weight" to new column "value" in list "edges"
tactiledata$edges$width <- 20*tactiledata$edges$value 
## line color
tactiledata$edges$color <- "gray" 
## print plot
visNetwork(nodes = tactiledata$nodes, edges = tactiledata$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")
```

# Calculate eigenvalue centrality of aerial and tactile treatments. 
```{r}
## figuring out aerial centrality
#head(meltaerial) ## checking header names from aerial df

#aeri <- graph_data_frame(meltaerial[,c('ind1','ind2')], directed = FALSE, vertices = NULL)
#E(aeri)$weight = meltaerial$values
#head(aeri)
#plot(aeri, vertex.label = NA, layout = layout_with_fr, main="Aerial Treatment") 
# making new igraph with aerial data
#aeri = graph.data.frame(meltaerial[,c('ind1','ind2')], directed = FALSE, vertices = NULL)
#E(aeri)$weight = meltaerial$values
#head(aeri)
#plot(aeri, vertex.label = NA, layout = layout_with_fr, main="Aerial Treatment") 
#then in visNetwrk 
#aeriv <- toVisNetworkData(aeri)
#visNetwork(nodes = aeriv$nodes, edges = aeriv$edges, height = "500px") %>%
# visIgraphLayout(layout = "layout_with_sugiyama")
# working out centrality 
#acentrality <- evcent(aeri)$vector
#head(acentrality)
#acentrality

## figuring out tactical centrality
#head(r1) ## checking header names from tactical df
# making new igraph with aerial data
#tacti = graph.data.frame(r1[,c('ind1','ind2')], directed = FALSE, vertices = NULL)
#E(tacti)$weight = r1$values
#head(tacti)
#plot(tacti, vertex.label = NA, layout = layout_with_fr, main="Taactical Treatment") 
#then in visNetwrk 
#tactiv <- toVisNetworkData(tacti)
#visNetwork(nodes = tactiv$nodes, edges = tactiv$edges, height = "500px") %>%
# visIgraphLayout(layout = "layout_with_sugiyama")
# working out centrality 
#tcentrality <- evcent(tacti)$vector
#head(tcentrality)
#tcentrality

# aerial treatment 
centralityaerialplot <- evcent(aerialplot)$vector
centralityaerialplot

# tactile treatment 
centralitytactileplot <- evcent(tactileplot)$vector
centralitytactileplot
```

# Calculate centrality of similarity index between an aerial and tactile treatments. 
To determine if there is any significant difference between the predator responses of our snakes towards the two artificially simulated predators (aerial and tactile) we compared the snakes scores of centrality (connectedness). Centrality is a network statistic which is a measure of being well-connected.
```{r}
# First, we need to combine our centrality measures for both the aerial and tactical conditions into one dataframe, so 3 columns: id, treatment (aerial or tactile), centrality score and add in the name of the dataframe in the snake_wilcox analysis below and this should (hopefully) work...

# names and centrility score for aerial treatment
Acent <- bind_cols(name = aerialnames, number = centralityaerialplot)

# names and centrility score for aerial treatment
Tcent <- bind_cols(name = tactilenames, number = centralitytactileplot)

acondition = 'aerial'
tcondition = 'tactile'
Acent <- mutate(Acent, "condition" = paste(acondition))
Acent
Tcent <- mutate(Tcent, "condition" = paste(tcondition))
Tcent

viper_centrality <- full_join(Tcent, Acent, by = "name")
viper_centrality
viper_centrality <- as.data.frame(viper_centrality)
viper_centrality

# install stat library for wilcox test
library(coin)

# Wilcoxon Test to compare the centrality scores for the snakes in the two different treatments 
snake_wilcox_aerial <- viper_centrality %>%
  wilcox_test(number.y ~ condition.y, paired = TRUE, p.adjust.method = "none")
snake_wilcox_aerial
snake_wilcox_tactile <- viper_centrality %>%
  wilcox_test(number.x ~ condition.x, paired = TRUE, p.adjust.method = "none")
snake_wilcox_tactile
```


# Another Example Using Association to Demonstrate Aesthetic Changes 
In this new example, we showcase a selection of different aesthetic changes you can utilize in your visualization of network data based on monkey association patterns. 
```{r}
# Read in another dataset, here we use monkeys 
Monkeys <- read_csv("EM_monkey.csv", col_names=TRUE)
#new <- meltme(new)
head(new)

# This is how you can replace numbers with names 
Sname <- c(Monkeys$name)
Sno <- as.numeric(1:nrow(Monkeys))
Source <- bind_cols(name = Sname, number = Sno)
Source
final <- inner_join(new, Source, by = c("ind1" = "number"))
final <- inner_join(final, Source, by = c("ind2" = "number"))
final <- select(final, name.x, name.y, values)
final

# Visualization of Monkey interactions 
Monkeysp <- graph_from_data_frame(final, directed = FALSE, vertices = NULL)
plot(aerialplot, vertex.label = NA, layout = layout_with_fr, main="Aerial treatment") 

datam <- toVisNetworkData(Monkeysp)
visNetwork(nodes = datam$nodes, edges = datam$edges, height = "500px") %>%
visIgraphLayout(layout = "layout_with_sugiyama")

# New monkey graph: 
#igraph
monk = graph.data.frame(final[,c('name.x','name.y')], directed = FALSE, vertices = NULL)
E(monk)$weight = final$values
head(monk)
plot(monk, vertex.label = NA, layout = layout_with_fr, main="Monkeys") 

# then in visNetwrk 
datam <- toVisNetworkData(monk)
visNetwork(nodes = datam$nodes, edges = datam$edges, height = "500px") %>%
  visIgraphLayout(layout = "layout_with_sugiyama")

# working out centrality 
centrality <- evcent(monk)$vector
head(centrality) 
````

# Inferring Results 
Individual snakes typically behaved significantly different between treatments as expected. 

# Concept Overview 
# Networks are made up of two essential components, 'nodes' (represented by symbols) and ‘edges’ (represented by  lines between the nodes). These lines typically indicate an interaction between the nodes
# Using the melt function bbe2020() to convert matrix data into a dataframe for network visualization & analysis
# Using {igraph}: graph_from_data_frame() and {visNetwork}: toVisNetworkData() to generate sociograms 
# Aesthetic changes in visNetwork sociograms <add in brief code example here>
# Eigenvector centrality statistics in igraph evcent()$vector
# The wilcox_test() is a non-parametric statistical test which compares two paired groups 
