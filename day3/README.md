Advent of Code: 3 December 2023
================

``` r
library(tidyverse)
```

``` r
input = readLines("input.txt")
```

``` r
# part 1
input_mat = input |>
  strsplit("") |>
  do.call(rbind, args = _)

df = crossing(row = seq_len(nrow(input_mat)), col = seq_len(ncol(input_mat))) |>
  mutate(val = map2_chr(row, col, \(row, col) input_mat[row, col]),
         is_num = val %in% as.character(0:9),
         is_sym = !is_num & val != ".") |>
  group_by(row) |>
  mutate(id = consecutive_id(is_num)) |>
  ungroup() |>
  mutate(id = paste0(row, "_", id))

search = crossing(row_diff = c(-1, 0, 1), col_diff = c(-1, 0, 1)) |>
  filter(!(row_diff == 0 & col_diff == 0)) |>
  mutate(key = 1)

x = df |>
  filter(is_sym) |>
  mutate(key = 1) |>
  left_join(search, by = "key", relationship = "many-to-many") |>
  mutate(search_row = row + row_diff,
         search_col = col + col_diff) |>
  select(-id) |>
  inner_join(df |> filter(is_num & !is_sym) |> select(row, col, num = val, id),
             by = c("search_row" = "row", "search_col" = "col")) |>
  distinct(search_row, id) |>
  left_join(df |> select(row, col, id, val), by = c("search_row" = "row", "id")) |>
  group_by(search_row, id) |>
  summarise(num = paste(val, collapse = "") |> as.numeric(),
            .groups = "drop") |>
  summarise(x = sum(num)) |>
  pull(x)

sprintf("part 1 solution: %i", x)
```

    ## [1] "part 1 solution: 544664"

``` r
# part 2
x = df |>
  filter(val == "*") |>
  mutate(key = 1) |>
  left_join(search, by = "key", relationship = "many-to-many") |>
  mutate(search_row = row + row_diff,
         search_col = col + col_diff) |>
  select(-c(id, row_diff, col_diff, key)) |>
  inner_join(df |> filter(is_num & !is_sym) |> select(row, col, num = val, id),
             by = c("search_row" = "row", "search_col" = "col")) |>
  group_by(row, col, val) |>
  mutate(count_n = length(unique(id))) |>
  filter(count_n == 2) |>
  distinct(row, col, id) |>
  left_join(df |> select(id, val), by = "id") |>
  group_by(row, col, id) |>
  summarise(val = paste(val.y, collapse = "") |> as.numeric(),
            .groups = "drop") |>
  group_by(row, col) |>
  summarise(val = prod(val), .groups = "drop") |>
  summarise(x = sum(val)) |>
  pull(x)

sprintf("part 2 solution: %i", x)
```

    ## [1] "part 2 solution: 84495585"
