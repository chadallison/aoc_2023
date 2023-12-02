Advent of Code: 2 December 2023
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
df = data.frame(line = input) |>
  separate(line, into = c("game", "values"), sep = ": ") |>
  separate_rows(values, sep = "; ") |>
  mutate(round = row_number(),
         game = as.integer(str_remove_all(game, "Game "))) |>
  separate_rows(values, sep = ", ") |>
  separate(values, into = c("num", "color"), sep = " ", convert = T) |>
  select(game, round, color, num)

head(df)
```

    ## # A tibble: 6 × 4
    ##    game round color   num
    ##   <int> <int> <chr> <int>
    ## 1     1     1 blue      4
    ## 2     1     1 green    16
    ## 3     1     1 red       2
    ## 4     1     2 red       5
    ## 5     1     2 blue     11
    ## 6     1     2 green    16

``` r
# part 1
x = df |>
  mutate(possible = case_when(color == "blue" ~ num <= 14,
                            color == "green" ~ num <= 13,
                            color == "red" ~ num <= 12)) |>
  group_by(game) |>
  summarise(n = n(),
            possible = sum(possible)) |>
  filter(n == possible) |>
  pull(game) |>
  sum()

sprintf("part 1 solution: %i", x)
```

    ## [1] "part 1 solution: 2685"

``` r
# part 2
x = df |>
  group_by(game, color) |>
  summarise(max = max(num),
            .groups = "drop") |>
  spread(color, max) |>
  mutate(power = blue * green * red) |>
  summarise(x = sum(power)) |>
  pull(x)

sprintf("part 2 solution: %i", x)
```

    ## [1] "part 2 solution: 83707"
