test_input <- c("forward 5",
                "down 5",
                "forward 8",
                "up 3",
                "down 8",
                "forward 2")

day02_input <- read.csv("day02/input.txt", header = FALSE)[,1]

move_submarine <- function(input, x, y){
  x <- 0 #horizontal position (forward)
  y <- 0 #depth (down/up)
  
  for(i in 1:length(input)){
    command <- strsplit(input[i], " ")[[1]]
    dir <- command[1]
    num <- as.numeric(command[2])
    
    if(dir %in% "forward"){
      x <- x + num
    } else if(dir == "up"){
      y <- y - num
    } else if (dir == "down"){
      y <- y + num
    }
  }
  
  return(list("x" = x, 
              "y" = y, 
              "product" = x * y))
}

move_submarine(day02_input, x, y)

move_submarine2 <- function(input, x, y){
  x <- 0 #horizontal position (forward)
  y <- 0 #depth (down/up)
  aim <- 0 #down X increases your aim by X units.
  # up X decreases your aim by X units.
  # forward X does two things:
  #   It increases your horizontal position by X units.
  #   It increases your depth by your aim multiplied by X.
  
  for(i in 1:length(input)){
    command <- strsplit(input[i], " ")[[1]]
    dir <- command[1]
    num <- as.numeric(command[2])
    
    if(dir %in% "forward"){
      x <- x + num
      y <- y + aim * num
    } else if(dir == "up"){
      aim <- aim - num
    } else if (dir == "down"){
      aim <- aim + num
    }
  }
  
  return(list("x" = x, 
              "y" = y, 
              "product" = x * y))
}

move_submarine2(day02_input, x, y)
