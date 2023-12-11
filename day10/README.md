Advent of Code: 10 December 2023
================

``` r
data10 = unname(unlist(read.fwf("input.txt", widths = rep(1L, 140L))))
tl = list(L = c(-1i, 1), J = c(-1i, -1), "7" = c(1i, -1), F = c(1i, 1)) # turn list

lp = ((which(data10 == "S") - 1L) %% 140 + 1L) * 1i + which(data10 == "S") %/% 140L + 1L
dir = 1i # found manually by looking at input

for (k in seq_len(1e5) + 1L) {
  lp[k] = lp[k - 1L] + dir
  cur = data10[(Re(lp[k]) - 1L) * 140 + Im(lp[k])]
  if (cur %in% c("L", "J", "7", "F")) {
    dir = tl[[cur]][abs(Re(dir) + 2 * Im(dir))]
  } else if (cur == "S") break
  
}
# part 1
x = (k - 1L) / 2L
sprintf("part 1 solution: %s", x)
```

    ## [1] "part 1 solution: 6907"

``` r
# part 2
ar = sum((Im(lp)[-length(lp)] + Im(lp[-1])) * diff(Re(lp))) / 2L
x = abs(ar) + 1L - (k - 1L) / 2L
sprintf("part 2 solution: %s", x)
```

    ## [1] "part 2 solution: 541"

``` r
# explanation
# for part 1 we use a turn list (tl) to pick the new direction at each turn.

# for part 2 we use the Shoelace formula first to determine the are of the polygon, drawn by the loop
# The formula is given by Area = 1/2 * sum_{i = 1} ^ n (y_i + y_{i + 1}) * (x_i - x_{i + 1})
# Given the are of the polygon, we can use Pick's theorem:
# Area = interior + boundary / 2 - 1, and solve for interior.
```
