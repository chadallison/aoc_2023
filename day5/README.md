Advent of Code: 5 December 2023
================

``` r
library(tidyverse)

input = readLines("input.txt")
empty_lines = which(input == "") + 1

seeds = sub("seeds: ", "", input[1]) |> strsplit(" ") |> _[[1]] |> as.numeric()

seed_map = list(
  seed_soil = read.table("input.txt", skip = empty_lines[1], nrows = empty_lines[2] - empty_lines[1] - 2),
  soil_fert = read.table("input.txt", skip = empty_lines[2], nrows = empty_lines[3] - empty_lines[2] - 2),
  fert_water = read.table("input.txt", skip = empty_lines[3], nrows = empty_lines[4] - empty_lines[3] - 2),
  water_light = read.table("input.txt", skip = empty_lines[4], nrows = empty_lines[5] - empty_lines[4] - 2),
  light_temp = read.table("input.txt", skip = empty_lines[5], nrows = empty_lines[6] - empty_lines[5] - 2),
  temp_humid = read.table("input.txt", skip = empty_lines[6], nrows = empty_lines[7] - empty_lines[6] - 2),
  huid_loc = read.table("input.txt", skip = empty_lines[7])
)

seed_map = lapply(seed_map, as.matrix)

# part 1
seed_loc = seeds
mapped = rep(F, length(seeds))

apply_mapping = function(mapping) {
  if (all(mapped)) return(NULL)
  source = mapping[2] + mapping[3]
  seed_diff = seed_loc - mapping[2]
  valid_seeds = seed_loc >= mapping[2] & seed_loc < source
  
  seed_loc[valid_seeds & !mapped] <<- c(mapping[1] + seed_diff)[valid_seeds & !mapped]
  mapped <<- mapped | valid_seeds
  NULL
}

tmp = lapply(seed_map, \(x) {
  mapped <<- rep(F, length(seed_loc))
  apply(x, 1, apply_mapping)
  NULL
})

x = min(seed_loc)
sprintf("part 1 solution: %i", x)
```

    ## [1] "part 1 solution: 1181555926"

``` r
# part 2
seed_loc_2 = split(seeds, (seq(seeds) + 1) %/% 2)
mapped = F
min_seed_loc = Inf
i = Inf
min_i = Inf

tmp = lapply(seed_loc_2, \(x) {
  i <<- x[[1]]
  while (i < x[[1]] + x[[2]]) {
    seed_loc <<- i
    lapply(seed_map, \(x) {
      mapped <<- F
      apply(x, 1, apply_mapping)
    })
    if (seed_loc < min_seed_loc) {
      min_i <<- i
      min_seed_loc <<- seed_loc
    }
    i <<- i + 20000
  }
})

tmp = lapply(list(c(min_i - 20000, 20000* 2)), \(x) {
  i <<- x[[1]]
  while (i <= x[[1]] + x[[2]]) {
    seed_loc <<- i
    lapply(seed_map, \(x) {
      mapped = F
      apply(x, 1, apply_mapping)
    })
    if (seed_loc < min_seed_loc) {
      min_i <<- i
      min_seed_loc <<- seed_loc
    }
    i <<- i + 1
  }
})

sprintf("part 2 solution: %i", min_seed_loc)
```

    ## [1] "part 2 solution: 37817961"
