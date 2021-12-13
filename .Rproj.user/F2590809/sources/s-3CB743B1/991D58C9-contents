library("tidyverse")

test_input <- read.table("day11/test_input.txt")
minitest <- read.table("day11/minitest.txt")
day11_input <- read.table("day11/input.txt")

parse_input <- function(input){
  input %>% 
    separate(col = "V1", into = paste0("V", 0:nchar(input[1,])), sep = "") %>% 
    select(-V0) %>% 
    map_dfr(as.numeric)
}

flashy_octopi <- function(input, steps){
  input <- parse_input(input)
  i <- 1
  flashes <- 0
  for(i in 1:steps){
    #energy level increases by 1
    input <- input + 1
    
    #octopus with energy level > 9 flashes
    tens <- tens0 <- which(input == 10, arr.ind = TRUE)
    while(nrow(tens) > 0){
      fg <- matrix(data = 0, nrow = nrow(input), ncol = ncol(input))
      idx <- tens[1,]
      #adjacent octopi (all directions)...
      exp <- list(c(idx + 1), 
                  c(idx - 1), 
                  c(idx + c(1,-1)), 
                  c(idx + c(-1, 1)), 
                  c(idx + c(1, 0)), 
                  c(idx + c(0, 1)), 
                  c(idx + c(-1, 0)), 
                  c(idx + c(0, -1)))
      
      exp <- discard(exp, function(x) any(x > 10) | any(x < 1))
      
      walk(exp, function(x) fg[x[1], x[2]] <<- fg[x[1], x[2]] + 1)
      
      #...increase energy level by 1
      input <- input + fg 
      
      #octopus that flash go back to 0
      input[idx[1], idx[2]] <- 0
      
      # #adjacent octopi with energy level > 9 also flash
      tens <- which(input >= 10, arr.ind = TRUE) 
      tens0 <- unique(rbind(tens0, tens))
    }
    
    #octopi can only flash once per step
    input[tens0]  <- 0
    i <- i + 1
    flashes <- flashes + nrow(tens0)
  }
  return(flashes)
}

flashy_octopi(test_input, 10)
flashy_octopi(test_input, 100)
flashy_octopi(day11_input, 100)

#### PART 2 ####
flashy_octopi2 <- function(input){
  input <- parse_input(input)
  i <- 1
  flashes <- 0
  tens0 <- 0
  
  while(!all(input == 0)){
    #energy level increases by 1
    input <- input + 1
    
    #octopus with energy level > 9 flashes
    tens <- tens0 <- which(input == 10, arr.ind = TRUE)
    while(nrow(tens) > 0){
      fg <- matrix(data = 0, nrow = nrow(input), ncol = ncol(input))
      idx <- tens[1,]
      #adjacent octopi (all directions)...
      exp <- list(c(idx + 1), 
                  c(idx - 1), 
                  c(idx + c(1,-1)), 
                  c(idx + c(-1, 1)), 
                  c(idx + c(1, 0)), 
                  c(idx + c(0, 1)), 
                  c(idx + c(-1, 0)), 
                  c(idx + c(0, -1)))
      
      exp <- discard(exp, function(x) any(x > 10) | any(x < 1))
      
      walk(exp, function(x) fg[x[1], x[2]] <<- fg[x[1], x[2]] + 1)
      
      #...increase energy level by 1
      input <- input + fg 
      
      #octopus that flash go back to 0
      input[idx[1], idx[2]] <- 0
      
      # #adjacent octopi with energy level > 9 also flash
      tens <- which(input >= 10, arr.ind = TRUE) 
      tens0 <- unique(rbind(tens0, tens))
    }
    
    #octopi can only flash once per step
    input[tens0]  <- 0
    i <- i + 1
    flashes <- flashes + nrow(tens0)
  }
  return(i - 1)
}


flashy_octopi2(test_input)
flashy_octopi2(day11_input)
