Advent of Code: 12 December 2023
================

``` r
library(tidyverse)
input = readLines("input.txt")

input_clean <- str_split(input," ")

update_arrangements <- function(row, col, test_spring, test_record){
  
  arrangements[row, col] <<- 0
  next_string <- test_record[col]
  
  if(test_spring[row] %in% c("?",".")){
    arrangements[row, col] <<- arrangements[row, col] + arrangements[row + 1, col]
  }
  if (test_spring[row] %in% c("?","#")){
    if (row + next_string > length(test_spring) + 1){
      
    } else if (row + next_string == length(test_spring) + 1){
      if (all(test_spring[row:(row - 1 + next_string)] %in% c("#","?"))){
        arrangements[row, col] <<- arrangements[row, col] + arrangements[length(test_spring) + 1, col + 1]
      }
    } else {
      if ((all(test_spring[row:(row - 1 + next_string)] %in% c("#","?"))) &
          (test_spring[row + next_string] %in% c(".","?"))){
        arrangements[row, col] <<- arrangements[row, col] + arrangements[row + next_string + 1, col + 1]
      }
    }
  }
  return(NULL)
}

dynprog_line <- function(test_spring, test_record){
  
  arrangements <<- matrix(rep(-1, (length(test_spring)+1) * (length(test_record) + 1)), nrow = (length(test_spring) + 1))
  
  for (row in 1:length(test_spring)){
    arrangements[row, length(test_record) + 1] <<- if_else(all(test_spring[row:length(test_spring)] %in% c("?",".")), 1, 0)
  }
  arrangements[length(test_spring) + 1, ] <<- 0
  arrangements[length(test_spring) + 1, length(test_record) + 1] <<- 1
  
  for (row in seq(length(test_spring), 1, by = -1)){
    for (col in seq(length(test_record), 1, by = -1)){
      update_arrangements(row, col, test_spring, test_record)
    }
  }
  
  return(arrangements[1,1])
}

springs <- lapply(input_clean, function(line){
  line[1] %>% str_split("") %>% unlist
})

records <- lapply(input_clean, function(line){
  line[2] %>% str_split(",") %>% unlist %>% as.numeric
})

ans_1 <- mapply(dynprog_line, springs, records) %>% sum()
sprintf("part 1 solution: %s", ans_1)
```

    ## [1] "part 1 solution: 7407"

``` r
springs_2 <- lapply(springs, function(line){
  c(line,"?",line,"?",line,"?",line,"?", line)
})

records_2 <- lapply(records, function(line){
  rep(line,5)
})

options(digits = 22)
ans_2 <- mapply(dynprog_line, springs_2, records_2) %>% sum()
sprintf("part 2 solution: %s", ans_2)
```

    ## [1] "part 2 solution: 30568243604962"
