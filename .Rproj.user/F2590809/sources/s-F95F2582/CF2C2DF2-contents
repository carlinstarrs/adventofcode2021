
#each lanternfish creates a new lanternfish once every 7 days
# you can model each fish as a single number that represents the number of days until it creates a new lanternfish.

# a new lanternfish would surely need slightly longer 
# before it's capable of producing more lanternfish: two more days for its first cycle.


test_input <- c(3,4,3,1,2)
day06_input <- scan("day06/input.txt", numeric(), sep = ",")

lanternfish_farm <- function(input, days){
  for(i in 1:days){
    input <- input - 1
    bebes <- rep(8, sum(input == -1))
    input[input == -1] <- 6
    input <- c(input, bebes[bebes > 0])
  }
  
  return(length(input))
}

lanternfish_farm(test_input, 80)
lanternfish_farm(day06_input, 80)

#### PART 2 ####
# too slow :( 

lanternfish_farm <- function(input, days){
  bebe_counter <<- c()
  for(i in 1:days){
    input <- input - 1
    bebe_counter <<- c(bebe_counter, setNames(sum(input == -1), i))
    input[input == -1] <- 6
  }
  
  return(input)
}



lanternfish_farm(test_input, 256)
lanternfish_farm(day06_input, 256)
