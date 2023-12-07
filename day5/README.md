Advent of Code: 6 December 2023
================

``` r
library(tidyverse)
library(janitor)

my_data = as_tibble(read_lines("input.txt"))

return_dest = function(in_source_val, in_source_name, in_dest_name, t_lookup) {
  to_join = tibble(source_name = in_source_name,
                    dest_name = in_dest_name,
                    in_source_val = in_source_val)
  by = join_by(source_name, dest_name, between(in_source_val, source, source_upper))
  best_match = left_join(to_join, t_lookup, by)
  if (is.na(best_match$dest[1])) {
    return(in_source_val)
  } else {
    return(in_source_val + best_match$delta[1])
  }
}

items = c("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

seeds = my_data |>
  slice(1) |>
  separate_longer_delim(value, " ") |>
  slice(-1) |>
  mutate(value = as.numeric(value)) |>
  rename(seed = value)

t_lookup = my_data |>
  filter(!value == "") |>
  slice(-1) |>
  mutate(mapping = ifelse(str_detect(value, "map"), value, NA)) |>
  fill(mapping) |>
  filter(!str_detect(value, "map")) |>
  separate_wider_regex(mapping, patterns = c(source_name = ".*", "-to-", dest_name = ".*", " map:")) |>
  separate_wider_delim(value, delim = " ", names = c("dest", "source", "range")) |>
  mutate(across(dest:range, as.numeric)) |>
  mutate(source_upper = source + range - 1) |>
  mutate(dest_upper = dest + range - 1) |>
  mutate(delta = dest - source)

seeds1 = seeds
seeds1[items[2:length(items)]] = NA
seeds1 = seeds1 |>  
  rowwise() |>
  mutate(soil = return_dest(seed, "seed", "soil", t_lookup)) |>
  mutate(fertilizer = return_dest(soil, "soil", "fertilizer", t_lookup)) |>
  mutate(water = return_dest(fertilizer, "fertilizer", "water", t_lookup)) |>
  mutate(light = return_dest(water, "water", "light", t_lookup)) |>
  mutate(temperature = return_dest(light, "light", "temperature", t_lookup)) |>
  mutate(humidity = return_dest(temperature, "temperature", "humidity", t_lookup)) |>
  mutate(location = return_dest(humidity, "humidity", "location", t_lookup))

part1 = min(seeds1$location)
sprintf("part 1 solution: %i", part1)
```

    ## [1] "part 1 solution: 1181555926"

``` r
return_splits = function(in_source_lower, in_source_upper, in_source_name, in_dest_name, t_lookup) {
  to_join = tibble(source_name = in_source_name,
                    dest_name = in_dest_name,
                    in_source_lower = in_source_lower,
                    in_source_upper = in_source_upper)
  by_lower = join_by(source_name, dest_name, between(in_source_lower, source, source_upper))
  by_upper = join_by(source_name, dest_name, between(in_source_upper, source, source_upper))
  lower_match = left_join(to_join, t_lookup, by_lower)
  upper_match = left_join(to_join, t_lookup, by_upper)
  to_manipulate = lower_match |>
    bind_rows(upper_match) |>
    unique()
  to_use = t_lookup |>
    mutate(in_source_lower = in_source_lower) |>
    mutate(in_source_upper = in_source_upper) |>
    filter(source_name == in_source_name &
           dest_name == in_dest_name &
           group_order >= min(to_manipulate$group_order) &
           group_order <= max(to_manipulate$group_order))
  to_use = to_use |>
    mutate(use_source_lower = ifelse(in_source_lower > source, in_source_lower, source)) |>
    mutate(use_source_upper = ifelse(in_source_upper < source_upper, in_source_upper, source_upper)) |>
    mutate(dest_lower = use_source_lower + delta) |>
    mutate(dest_upper = use_source_upper + delta)
  to_return = to_use |>
    select(dest_lower, dest_upper) |>
    rename(lower = dest_lower, upper = dest_upper)
  return(to_return)
}

compute_next = function(data_in, in_source_name, in_dest_name, t_lookup) {
  for (i in seq_along(data_in$lower)) {
    if (i == 1) {
      data_out = return_splits(data_in$lower[i], data_in$upper[i], in_source_name, in_dest_name, t_lookup)
    } else {
      data_out = data_out |>
        bind_rows(data_out, return_splits(data_in$lower[i], data_in$upper[i], in_source_name, in_dest_name, t_lookup)) |>
        unique()
    }
  }
  return(data_out)
}

seeds2 = seeds |>
  mutate(pair = ifelse(row_number() %% 2 == 1, row_number(), row_number() - 1)) |>
  mutate(type = ifelse(row_number() %% 2 == 1, "lower", "range")) |>
  pivot_wider(names_from = type, values_from = seed) |>
  mutate(upper = lower + range - 1) |>
  select(-range, -pair)

t_lookup_sorted = t_lookup |>
  group_by(source_name, dest_name) |>
  arrange(source, .by_group = TRUE) |>
  select(source_name, dest_name, source, source_upper, delta, everything()) |>
  mutate(lowest_source = min(source)) |>
  mutate(highest_source = max(source_upper)) |>
  mutate(group_order = row_number()) |>
  ungroup()

t_lookup_additional = t_lookup_sorted |>
  ungroup() |>
  select(-lowest_source, -highest_source)

for (i in seq_along(t_lookup_sorted$source_name)) {
  if (t_lookup_sorted$group_order[i] == 1) {
    if (t_lookup_sorted$source[i] != 0) {
      t_lookup_additional = t_lookup_additional |>
        add_row(source_name = t_lookup_sorted$source_name[i],
                dest_name = t_lookup_sorted$dest_name[i],
                source = 0,
                source_upper = t_lookup_sorted$source[i] - 1,
                delta = 0,
                dest = 0,
                dest_upper = t_lookup_sorted$source[i] - 1,
                group_order = 0)
    }
  } else if ((i < length(t_lookup_sorted$source_name) & t_lookup_sorted$group_order[i + 1] == 1) |
             (i == length(t_lookup_sorted$source_name))) {
    if (t_lookup_sorted$source_upper[i] != 4294967295) {
      t_lookup_additional = t_lookup_additional |>
        add_row(source_name = t_lookup_sorted$source_name[i],
                dest_name = t_lookup_sorted$dest_name[i],
                source = t_lookup_sorted$source_upper[i] + 1,
                source_upper = 4294967295,
                delta = 0,
                dest = t_lookup_sorted$source_upper[i] + 1,
                dest_upper = 4294967295,
                group_order = t_lookup_sorted$group_order[i] + 1)
    }
  } else {
    if ((t_lookup_sorted$source[i] != t_lookup_sorted$source_upper[i - 1] + 1) &
        (t_lookup_sorted$source_name[i] == t_lookup_sorted$source_name[i - 1])) {
      t_lookup_additional = t_lookup_additional |>
        add_row(source_name = t_lookup_sorted$source_name[i],
                dest_name = t_lookup_sorted$dest_name[i],
                source = t_lookup_sorted$source_upper[i - 1] + 1,
                source_upper = t_lookup_sorted$source[i] - 1,
                delta = 0,
                dest = t_lookup_sorted$source_upper[i - 1] + 1,
                dest_upper = t_lookup_sorted$source[i] - 1,
                group_order = t_lookup_sorted$group_order[i] + 0.5)
    }
  }
}

t_lookup_additional= t_lookup_additional |>
  select(-group_order) |>
  group_by(source_name, dest_name) |>
  arrange(source, .by_group = TRUE) |>
  mutate(group_order = row_number()) |>
  ungroup()

soil2 = compute_next(seeds2, "seed", "soil", t_lookup_additional)
fertilizer2 = compute_next(soil2, "soil", "fertilizer", t_lookup_additional)
water2 = compute_next(fertilizer2, "fertilizer", "water", t_lookup_additional)
light2 = compute_next(water2, "water", "light", t_lookup_additional)
temperature2 = compute_next(light2, "light", "temperature", t_lookup_additional)
humidity2 = compute_next(temperature2, "temperature", "humidity", t_lookup_additional)
location2 = compute_next(humidity2, "humidity", "location", t_lookup_additional)

part2 = min(location2$lower)
sprintf("part 2 solution: %i", part2)
```

    ## [1] "part 2 solution: 37806486"
