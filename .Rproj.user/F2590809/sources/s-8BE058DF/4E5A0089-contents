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
lanternfish_farm2 <- function(input, days){
  input <- table(input)
  for(i in 1:days){
    input <- setNames(input, as.numeric(names(input)) - 1)
    if(any(names(input) == "-1")){
      input <- c(input[!names(input) == "-1"], 
                 setNames(input["-1"], 6),
                 setNames(input["-1"], 8))
      input <- tapply(input, names(input), sum, na.rm = TRUE)
    }
  }
  
  return(print(sum(input), digits = nchar(sum(input))))
}

lanternfish_farm2(test_input, 256)
lanternfish_farm2(day06_input, 256)
