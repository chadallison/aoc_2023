Advent of Code: 8 December 2023
================

``` r
data = readLines("input.txt")

lr = unname(c("L" = 2L, "R" = 3L)[strsplit(data[1], "")[[1]]])
gr = do.call(rbind, strsplit(data[-(1:2)], "[ \\= \\(,)]+"))

go = function(cur, .end = "Z$") {
  k = 0L
  while (!grepl(.end, cur)) {
    cur = gr[gr[,1] == cur, lr[k %% length(lr) + 1L]]
    k = k + 1L
  }
  k
}

# part 1
sprintf("part 1 solution: %s", go("AAA", "ZZZ"))
```

    ## [1] "part 1 solution: 18113"

``` r
# part 2
sprintf("part 2 solution: %.f", Reduce(pracma::Lcm, sapply(grep("A$", gr[, 1], value = T), go)))
```

    ## [1] "part 2 solution: 12315788159977"
