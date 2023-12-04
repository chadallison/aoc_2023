Advent of Code: 4 December 2023
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
df = data.frame(line = input) |>
  extract(line, c("winning", "have"), ".*: (.*) \\| (.*)") |>
  mutate(winning = str_extract_all(winning, "\\d+"),
         have = str_extract_all(have, "\\d+"),
         overlap = map2(winning, have, intersect),
         n_matches = lengths(overlap))

x = df |>
  filter(n_matches > 0) |>
  mutate(score = 2 ^ (n_matches - 1)) |>
  summarise(x = sum(score)) |>
  pull(x)

sprintf("part 1 solution: %i", x)
```

    ## [1] "part 1 solution: 20407"

``` r
# part 2
games = nrow(df)
matches = df$n_matches
copies = rep(1, games)

for (i in seq_len(games)) {
  if (matches[i] > 0) {
    range = seq(i + 1, min(i + matches[i], games))
    copies[range] = copies[range] + copies[i]
  }
}

sprintf("part 2 solution: %i", sum(copies))
```

    ## [1] "part 2 solution: 23806951"
