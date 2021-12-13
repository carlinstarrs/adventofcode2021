test_input <- c(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
)

day03_input <- read.csv("day03/input.txt", header = FALSE, colClasses = "character")[,1]

parse_input <- function(input){
  out <- matrix(ncol = nchar(input[1]), nrow = length(input))
  for(i in 1:length(input)){
    out[i,] <- as.numeric(strsplit(input[i], "")[[1]])
  }
  return(out)
}


power_consumption <- function(input){
  parsed <- parse_input(input)
  
  gamma <- c()  #binary
  epsilon <- c() #binary
  
  for(i in 1:ncol(parsed)){
    ones <- sum(parsed[,i])
    zeros <- nrow(parsed) - ones
    
    if(ones > zeros){
      gamma[i] <- 1
      epsilon [i] <- 0
    } else {
      gamma[i] <- 0
      epsilon[i] <- 1
    }
  }
  
  gamma <- strtoi(paste(gamma, collapse = ""), base = 2)
  epsilon <- strtoi(paste(epsilon, collapse = ""), base = 2)
  
  return(list("gamma" = gamma, 
              "epsilon" = epsilon, 
              "product" = gamma * epsilon))
}

power_consumption(test_input)
power_consumption(day03_input)


##### PART 2 ####
get_rating <- function(input, type){
  parsed <- data.frame(parse_input(input))
  
  if(type == "02"){
    val1 <- 1
    val2 <- 0
  } else if (type == "C02"){
    val1 <- 0
    val2 <- 1
  }
  
  for(i in 1:ncol(parsed)){
    if(nrow(parsed) > 1){
      ones <- sum(parsed[,i])
      zeros <- nrow(parsed) - ones
      
      if(ones == zeros){
        parsed <- parsed[parsed[,i] == val1,]
      } else if(ones > zeros){
        parsed <- parsed[parsed[,i] == val1,]
      } else {
        parsed <- parsed[parsed[,i] == val2,]
      }
    }
  }
  
  rating <- strtoi(paste(parsed[1,], collapse = ""), base = 2)
  
  return(rating)
}

life_support_rating <- function(input){
  o2_gen_rating <- get_rating(input, "02")
  co2_scrub_rating <- get_rating(input, "C02")
  
  return(o2_gen_rating * co2_scrub_rating)
}  

life_support_rating(test_input)
life_support_rating(day03_input)
