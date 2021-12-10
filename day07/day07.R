library("tidyverse")

test_input <- c(16,1,2,0,4,2,7,1,2,14)
day07_input <- scan("day07/input.txt", numeric(), sep = ",")

efficient_crabs <- function(input) min(map_dbl(1:max(input), function(y) sum(abs(map_dbl(input, ~.x - y)))))
efficient_crabs(test_input)
efficient_crabs(day07_input)

#### PART 2 ####
expensive_crabs <- function(input) min(map_dbl(1:max(input), function(y) sum(map_dbl(input, function(x) sum(1:abs(x-y))))))
expensive_crabs(test_input)
expensive_crabs(day07_input)
