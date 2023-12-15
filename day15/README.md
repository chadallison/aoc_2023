Advent of Code: 15 December 2023
================

``` r
library(tidyverse)

input = readLines("input.txt") |>
  strsplit(",") |>
  unlist()

get_hash = function(str) {
  x = strsplit(str, "") |> unlist()
  current = 0
  for (val in x) {
    current = ((as.integer(charToRaw(val)) + current) * 17) %% 256
  }
  return(current)
}

x = data.frame(raw = input) |>
  mutate(split = strsplit(raw, ""),
         value = map(split, get_hash) |> unlist()) |>
  summarise(x = sum(value)) |>
  pull(x)

sprintf("part 1 solution: %s", x)
```

    ## [1] "part 1 solution: 515495"

``` r
df = data.frame(raw = input) |>
  extract(raw, into = c("label", "operation", "lens"),
          regex = "([a-z]+)([\\-\\=])(.*)",
          convert = T) |>
  mutate(box = sapply(label, get_hash))

do_operation = function(label, operation, lens, box) {
  if (operation == "-") {
    if (label %in% names(box)) return(box[!names(box) %in% label])
    return(box)
  } else if (operation == "=") {
    if (label %in% names(box)) {
      box[label] = lens
      return(box)
    }
    box = c(box, set_names(lens, label))
    return(box)
  }
}

b = map(unique(df$box), \(x) c()) |> set_names(sort(unique(df$box)))
i = 0

while (i < nrow(df)) {
  i = i + 1
  id = as.character(df$box[i])
  b[[id]] = do_operation(label = df$label[i], operation = df$operation[i],
                         lens = df$lens[i], box = b[[id]])
}

frame = enframe(b, name = "box") |>
  mutate(box = as.numeric(box),
         value = map(value, \(v) enframe(v, name = "label", value = "length"))) |>
  unnest(value) |>
  mutate(p = (box + 1) * row_number() * length, .by = box)

x = frame |>
  summarise(sol = sum(p)) |>
  pull(sol)

sprintf("part 2 solution: %s", x)
```

    ## [1] "part 2 solution: 229349"
