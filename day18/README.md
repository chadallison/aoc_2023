Advent of Code: 18 December 2023
================

``` r
parse_input = function(path) {
  d = matrix(unlist(strsplit(readLines(path), " ")), ncol = 3, byrow = T)
  list(list(dir = d[, 1],
            n = as.integer(d[, 2])),
       list(dir = chartr("0123", "RDLU", substr(d[, 3], 8, 8)),
            n = strtoi(substr(d[, 3], 3, 7), 16)))
}

area = function(d) {
  x = c(0, cumsum(unname(c(U = -1, D = 1, L = 0, R = 0)[d$dir]) * d$n))
  y = c(0, cumsum(unname(c(U = 0, D = 0, L = -1, R = 1)[d$dir]) * d$n))
  abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y) / 2) + sum(d$n) / 2 + 1 # shoelace formula
}

part1 = function(d) {
  area(d[[1]])
}

part2 = function(d) {
  format(area(d[[2]]), digits = 20)
}

d = parse_input("input.txt")
sprintf("part 1 solution: %s", part1(d))
```

    ## [1] "part 1 solution: 47675"

``` r
sprintf("part 2 solution: %s", part2(d))
```

    ## [1] "part 2 solution: 122103860427465"
