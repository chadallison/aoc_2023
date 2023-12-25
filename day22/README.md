Advent of Code: 22 December 2023
================

``` r
df = sapply(strsplit(readLines("input.txt"), "[~,]"), as.integer)
df = df[, order(df[3,])] # sorting bricks
df[c(1, 4), ] = df[c(1, 4), ] - min(df[c(1, 4), ]) + 1L # shift x
df[c(2, 5), ] = df[c(2, 5), ] - min(df[c(2, 5), ]) + 1L # shift y
df[6L, ] = df[6L, ] - df[3L, ]
flr = matrix(0L, ncol = max(df[4, ]), nrow = max(df[5, ]))

let_fall = function(bricks) {
  z = integer(ncol(bricks))
  for (k in 1:ncol(bricks)) {
    b = bricks[, k]
    br_col = b[1L]:b[4L]
    br_row = b[2L]:b[5L]
    z[k] = max(flr[br_row, br_col]) + 1L
    flr[br_row, br_col] = z[k] + b[6L]
  }
  return(z)
}

x0 = let_fall(df)
x1 = sapply(1:ncol(df), \(k) let_fall(df[, -k]))

# part 1
sprintf("part 1 solution: %s", sum(sapply(1:ncol(df), \(k) sum(x1[,k] != x0[-k]) == 0L)))
```

    ## [1] "part 1 solution: 439"

``` r
# part 2
sprintf("part 2 solution: %s", sum(sapply(1:ncol(df), \(k) sum(x1[,k] != x0[-k]))))
```

    ## [1] "part 2 solution: 43056"
