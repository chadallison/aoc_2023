Advent of Code: 7 December 2023
================

``` r
library(tidyverse)

input = readLines("input.txt")

df = data.frame(input) |>
  mutate(id = row_number(), .before = 1) |>
  separate(input, into = c("hand", "bid"), sep = " ", convert = T)

l = c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2") |> rev()

hands = df |> 
  dplyr::mutate(h = strsplit(hand,"") |> lapply(\(h) factor(h, levels = l))) |> 
  dplyr::mutate(
    rle = lapply(h, \(h) as.integer(h) |> sort() |> rle()),
    is_five = sapply(rle, \(r) any(r$lengths == 5)),
    is_four = sapply(rle, \(r) any(r$lengths == 4)),
    is_full = sapply(rle, \(r) identical(sort(r$lengths), c(2L,3L))),
    is_trips = sapply(rle, \(r) any(r$lengths == 3)) & !is_full,
    is_twopair = sapply(rle, \(r) identical(sort(r$lengths), c(1L,2L,2L))),
    is_pair = sapply(rle, \(r) any(r$lengths == 2)) & !is_trips & !is_twopair,
    is_highcard = sapply(rle, \(r) all(r$lengths == 1)),
    hand_type = dplyr::case_when(
      is_five ~ 7,
      is_four ~ 6,
      is_full ~ 5,
      is_trips ~ 4,
      is_twopair ~ 3,
      is_pair ~ 2,
      is_highcard ~ 1
    ),
    card_1 = sapply(h, \(h) h[[1]] |> as.integer()),
    card_2 = sapply(h, \(h) h[[2]] |> as.integer()),
    card_3 = sapply(h, \(h) h[[3]] |> as.integer()),
    card_4 = sapply(h, \(h) h[[4]] |> as.integer()),
    card_5 = sapply(h, \(h) h[[5]] |> as.integer()),
    h = NULL,
    rle = NULL
    
  ) |> 
  dplyr::arrange(hand_type, card_1, card_2, card_3, card_4, card_5) |> 
  dplyr::mutate(
    hand_rank = dplyr::row_number(),
    value = hand_rank * bid
  )

sprintf("part 1 solution: %i", sum(hands$value))
```

    ## [1] "part 1 solution: 250232501"

``` r
l2 = c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J") |> rev()

hands2 = df |> 
  dplyr::mutate(h = strsplit(hand,"") |> lapply(\(h) factor(h, levels = l2))) |> 
  dplyr::mutate(
    rle = lapply(h, \(h) as.integer(h) |> sort() |> rle()),
    is_five = sapply(rle, \(r) any(r$lengths == 5)),
    is_four = sapply(rle, \(r) any(r$lengths == 4)),
    is_full = sapply(rle, \(r) identical(sort(r$lengths), c(2L,3L))),
    is_trips = sapply(rle, \(r) any(r$lengths == 3)) & !is_full,
    is_twopair = sapply(rle, \(r) identical(sort(r$lengths), c(1L,2L,2L))),
    is_pair = sapply(rle, \(r) any(r$lengths == 2)) & !is_trips & !is_twopair,
    is_highcard = sapply(rle, \(r) all(r$lengths == 1)),
    count_jokers = sapply(h, \(h) sum(h == "J")),
    hand_type = dplyr::case_when(
      is_five ~ 7,
      is_four & count_jokers == 1  ~ 7,
      is_four & count_jokers == 4 ~ 7,
      is_full & count_jokers == 3 ~ 7,
      is_full & count_jokers == 2 ~ 7,
      is_four ~ 6,
      is_trips & count_jokers == 1 ~ 6,
      is_trips & count_jokers == 3 ~ 6,
      is_twopair & count_jokers == 2 ~ 6,
      is_full ~ 5,
      is_twopair & count_jokers == 1 ~ 5,
      is_trips ~ 4,
      is_pair & count_jokers == 2 ~ 4,
      is_pair & count_jokers == 1 ~ 4,
      is_twopair ~ 3,
      is_pair ~ 2,
      is_highcard & count_jokers == 1 ~ 2,
      is_highcard ~ 1
    ),
    card_1 = sapply(h, \(h) h[[1]] |> as.integer()),
    card_2 = sapply(h, \(h) h[[2]] |> as.integer()),
    card_3 = sapply(h, \(h) h[[3]] |> as.integer()),
    card_4 = sapply(h, \(h) h[[4]] |> as.integer()),
    card_5 = sapply(h, \(h) h[[5]] |> as.integer()),
    h = NULL,
    rle = NULL
  ) |> 
  dplyr::arrange(hand_type, card_1, card_2, card_3, card_4, card_5) |> 
  dplyr::mutate(
    hand_rank = dplyr::row_number(),
    value = hand_rank * bid
  )

sprintf("part 2 solution: %i", sum(hands2$value))
```

    ## [1] "part 2 solution: 249138943"
