test_input <- c(199,
           200,
           208,
           210,
           200,
           207,
           240,
           269,
           260,
           263)

day01_input <- read.csv("C:/Users/Carlin/Documents/GitHub/adventofcode2021/Day1/input.txt", header = FALSE)[,1]

#count the number of times a depth measurement increases from the previous measurement. 
# 199 (N/A - no previous measurement)
# 200 (increased)
# 208 (increased)
# 210 (increased)
# 200 (decreased)
# 207 (increased)
# 240 (increased)
# 269 (increased)
# 260 (decreased)
# 263 (increased)

run_day01 <- function(input){
  increases <- c(0)
  for(i in 1:length(input)){
    if(i == 1){
      out <- "N/A - no previous measurement"
    } else {
      test <- input[i] - input[i - 1] 
      
      if(test > 0) {
        out <- "(increased)"
        increases <- increases + 1
      } else if(test < 0){
        out <- "(decreased)"
      } else {
        out <- "(same)"
      }
    }
    print(paste(input[i], out))
    
  }
  return(increases)
}

run_day01(test_input)
run_day01(day01_input)


#### PART 2 ####

# 199  A      
# 200  A B    
# 208  A B C  
# 210    B C D
# 200  E   C D
# 207  E F   D
# 240  E F G  
# 269    F G H
# 260      G H
# 263        H

# count the number of times the sum of measurements in this sliding window increases from the previous sum. 
# So, compare A with B, then compare B with C, then C with D, and so on. Stop when there aren't enough measurements \
# left to create a new three-measurement sum.
# 
# In the above example, the sum of each three-measurement window is as follows:
# 
# A: 607 (N/A - no previous sum)
# B: 618 (increased)
# C: 618 (no change)
# D: 617 (decreased)
# E: 647 (increased)
# F: 716 (increased)
# G: 769 (increased)
# H: 792 (increased)

run_day01_p2 <- function(input){
  increases <- c(0)
  for(i in 3:length(input)){
    value <- sum(input[(i-2):i])
    if(i <= 3){
      out <- "N/A - no previous measurement"
    } else {
      test <- sum(input[(i-2):i]) - sum(input[(i-3):(i-1)]) 
      if(test > 0) {
        out <- "(increased)"
        increases <- increases + 1
      } else if(test < 0){
        out <- "(decreased)"
      } else {
        out <- "(no change)"
      }
    }
    print(paste(value, out))
  }
  return(increases)
}

run_day01_p2(test_input)
run_day01_p2(day01_input)
