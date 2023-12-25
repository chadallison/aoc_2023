Advent of Code: 25 December 2023
================

``` r
library(tidyverse)
library(igraph)
input = readLines("input.txt")

df = data.frame(input) |>
  separate_wider_delim(input, ":", names = c("from", "to")) |>
  separate_longer_delim(to, " ") |>
  filter(to != "")

edges = as.matrix(df)
comp = graph_from_edgelist(edges, directed = F)
groups = min_cut(comp, value.only = F)
sol = length(groups$partition1) * length(groups$partition2)
sprintf("part 1 solution: %s", sol)
```

    ## [1] "part 1 solution: 562978"
