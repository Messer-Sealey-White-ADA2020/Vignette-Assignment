library(igraph)
library(tidyverse) # to enable tidying of data
library(reshape2) # to use with melt function to convert matrix to objects igraph can plot
library(visNetwork) 
aerial <- read_csv("blacktreatments_heads2020f.csv", col_names=TRUE)

colnames(aerial) <-  c("names", 1:length(b0))
cleanaerial <- select(aerial, -names)
head(cleanaerial)

cleanaerial <- as.matrix(cleanaerial)
cleanaerial[lower.tri(cleanaerial, diag=TRUE)] <- NA
meltaerial <- setNames(melt(cleanaerial), c('ind1', 'ind2', 'values'))
head(meltaerial)

meltaerial <- filter(meltaerial, !is.na(values))
summary(meltaerial)

aerialplot <- graph_from_data_frame(meltaerial, directed = FALSE, vertices = NULL)
plot(aerialplot, vertex.label = NA)

visIgraph(aerialplot, randomSeed = TRUE) %>% 
  visOptions(manipulation = TRUE)

VisNPlot <- function(x,
                     nodes = data$nodes, 
                     edges = data$edges, 
                     height = "500px"){

x = toVisNetworkData %>% 
  graph_from_data_frame(meltaerial, directed = FALSE, vertices = NULL) %>%
filter(meltaerial, !is.na(values))%>%
setNames(melt(cleanaerial), c('ind1', 'ind2', 'values')) 
colnames(aerial) <-  c("names", 1:length(aerial)) %>%
      cleanaerial <- as.matrix(cleanaerial) %>%
      cleanaerial[lower.tri(cleanaerial, diag=TRUE)] <- NA %>%
      cleanaerial <- select(x, -names) 
}

VisNPlot(aerial)

cv_summary <- function(d, country_list = "World",
                       plot = TRUE, facet = "country",
                       status = c("confirmed", "deaths", "recovered")) {
  
  # based on `wes_palettes()` color schemes GrandBudapest1, IsleofDogs1,
  # IsleofDogs2 from the {wesanderson} package
  my_palette <- c(
    "#5B1A18", "#FD6467", "#F1BB7B", "#D67236",
    "#0F0D0E", "#9986A5", "#79402E", "#CCBA72", "#D9D0D3", "#8D8680",
    "#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718"
  )
  
  if (facet == "country") {
    fill <- "variable"
    n <- length(unique(d$variable)) / 2
    # need only half of unique # of variable (3)
  }
  
  if (facet == "variable") {
    fill <- "country"
    n <- length(country_list)
    # need number of countries
  }
  
  if ("All" %in% country_list) {
    country_list <- unique(d$country)
    country_list <- setdiff(country_list, "World")
  }
  
  if ("World" %in% country_list) {
    d <- d %>% filter(country %in% country_list)
    
    totals <- d %>%
      group_by(variable) %>%
      summarize(
        country = "World",
        cases = max(cases),
        population = max(population),
        area = max(area),
        density = max(density),
        rate = max(rate, na.rm = TRUE),
        on = max(date)
      ) %>%
      select(country, variable, cases, population, area, density, rate, on) %>%
      arrange(variable) %>%
      ungroup()
  }
  
  if ("World" %nin% country_list) {
    d <- d %>% filter(country %in% country_list)
    totals <- d %>%
      group_by(country, variable) %>%
      summarize(
        cases = max(cases),
        population = max(population),
        area = max(area),
        density = max(density),
        rate = max(rate, na.rm = TRUE),
        on = max(date),
        gdp_capita = fmode(gdp_capita),
        income = fmode(income),
        life_expectancy = fmode(life_expectancy),
        max_sd = max(soc_dist),
        max_mr = max(mov_rest)
      ) %>%
      select(
        country, variable, cases, population, area, density, rate,
        gdp_capita, income, life_expectancy, max_sd, max_mr, on
      ) %>%
      arrange(country, variable) %>%
      ungroup()
  }
  
  if (plot == TRUE) {
    cc <- filter(d, variable %in% status)
    cum_cases_plot <- ggplot(
      data = cc,
      # use the tidy evaluation pronoun .data to slice the chosen fill
      # variable from the data frame
      aes(
        x = date, y = cases + 1, color = .data[[fill]],
        fill = .data[[fill]]
      )
    ) +
      geom_point(size = 0.5) +
      geom_line() +
      # use the tidy evaluation pronoun .data to slice the chosen facet_wrap
      # variable from the data frame
      facet_wrap(~ .data[[facet]], ncol = 5) +
      xlab("Date") +
      ylab("Log Cumulative Cases") +
      scale_y_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      scale_color_manual(
        aesthetics = c("color", "fill"),
        name = NULL, values = my_palette
      )
    
    dc <- filter(d, variable %in% paste0("daily_", status))
    daily_cases_plot <- ggplot(
      data = dc,
      aes(
        x = date, y = cases, color = .data[[fill]],
        fill = .data[[fill]]
      )
    ) +
      geom_point(size = 0.5) +
      geom_line() +
      facet_wrap(~ .data[[facet]], ncol = 5) +
      xlab("Date") +
      ylab("Daily Cases") +
      scale_color_manual(
        aesthetics = c("color", "fill"),
        name = NULL, values = my_palette
      )
  }
  
  if (plot == TRUE) {
    return(list(
      totals = totals,
      cum_cases_plot = cum_cases_plot,
      daily_cases_plot = daily_cases_plot
    ))
  } else {
    return(list(totals = totals))
  }
}