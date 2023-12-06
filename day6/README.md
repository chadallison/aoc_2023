Advent of Code: 6 December 2023
================

``` r
library(tidyverse)

input = readLines("input.txt")

df = data.frame(time = str_extract_all(input[1], "\\d+") |> unlist() |> as.integer(),
           distance = str_extract_all(input[2], "\\d+") |> unlist() |> as.integer()) |>
  mutate(id = row_number(), .before = 1)

time = as.numeric(str_extract_all(input[1], "\\d+")[[1]])
dist = as.numeric(str_extract_all(input[2], "\\d+")[[1]])

get_combos = function(len_time, record) {
  charge = seq(0, len_time)
  distance = (len_time - charge) * charge
  return(sum(distance > record))
}

# part 1
x = prod(map2_dbl(time, dist, get_combos))
sprintf("part 1 solution: %i", x)
```

    ## [1] "part 1 solution: 281600"

``` r
# part 2
combine_nums = function(x) {
  return(as.numeric(paste0(x, collapse = "")))
}

x = get_combos(combine_nums(time), combine_nums(dist))
sprintf("part 2 solution: %i", x)
```

    ## [1] "part 2 solution: 33875953"
