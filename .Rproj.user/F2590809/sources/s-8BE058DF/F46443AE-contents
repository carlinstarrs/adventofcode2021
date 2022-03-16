library("tidyverse")

test_input <- readLines("day15/test_input.txt")
day15_input <- readLines("day15/day15_input.txt")
#add up the risk levels of each position you enter

#the starting position is never entered

#Your goal is to find a path with the lowest total risk.
#you cannot move diagonally

#What is the lowest total risk of any path from the top left to the bottom right?
input <- test_input

ll <- nchar(input[1])

out <- data.frame(input) %>% 
  mutate(row_id = 1:nrow(.)) %>% 
  separate_rows(input, sep = "") %>% 
  filter(!input == "") %>% 
  mutate(col_id = rep_len(1:ll, length.out = nrow(.)), 
         pos_id = 1:nrow(.))

start_coords <- c(1,1)
final_coords <- c(nchar(input[1]), length(input))

get_coord_riskval <- function(x){
  out$input[out$col_id == x[2] & out$row_id == x[1]]
}

get_coord_pos_id <- function(x, y){
  out$pos_id[out$col_id == x[2] & out$row_id == x[1]]
}

get_coord_from_pos_id <- function(x){
  c(out$row_id[out$pos_id == x], out$col_id[out$pos_id == x])
}

get_riskval_from_pos_id <- function(x){
  as.numeric(out$input[out$pos_id == x])
}


tick <- Sys.time()

winner <- list()

i <- 1
pos <- c(1,1)
path <- list(get_coord_pos_id(pos)) #initial
names(path) <- get_coord_pos_id(pos) #initial

##
while(length(path) > 0){
  
  pos_id <- tail(path[[1]], 1) #start from the end of the first path in the list
  pos <- get_coord_from_pos_id(pos_id)
  
  directions <- list("down" = c(pos[1] + 1, pos[2]),
                     "right" = c(pos[1], pos[2] + 1),
                     "left" = c(pos[1], pos[2] - 1),
                     "up" = c(pos[1] - 1, pos[2])) %>% 
    keep(~all(.x > 0)) %>% 
    map(get_coord_pos_id)  %>% 
    compact()
  
  #if there's no winner, take the most direct route
  if(length(winner) == 0){
    if((i %% 2) == 0){
      directions <- directions$right
    } else {
      directions <- directions$down
    }
  } 
  
  
  new_path <- map(path[names(path) == pos_id], function(x){ 
    compact(map(directions, function(y){
      if(!any(x == y)){ #check that new coordinate hasn't been visited
        c(x, y)
      } else {
        return(NULL)
      }
    })) 
  }) 
  
  new_path <- unlist(new_path, recursive = FALSE, use.names = FALSE)

  path <- unname(append(new_path, path[!names(path) == pos_id]))
  path <- unique(path)
  
  names(path) <- map(path, ~tail(.x, 1))
  riskval <- map_dbl(path, function(x) reduce(map(x, function(y) get_riskval_from_pos_id(y)), sum))
  
  #check riskval is less than winner's riskval
  if(length(winner) > 0){
    path <- path[riskval < as.numeric(names(winner))] 
  } 
  
  riskval <- map_dbl(path, function(x) reduce(map(x, function(y) get_riskval_from_pos_id(y)), sum))
  
  #prune any duplicate final nodes to the one with the minimum current risk value
  if(any(table(names(riskval)) > 1)){
    dupes <- riskval[names(riskval) %in% names(table(names(riskval))[table(names(riskval)) > 1])]
    dupes <- path[names(path) %in% names(dupes)]
    
    filtered_dupes <- list()
    for(j in 1:length(unique(names(dupes)))){
      fd_dupes <- dupes[names(dupes) == unique(names(dupes)[j])]
      fd_rv <-  map_dbl(fd_dupes, function(x) reduce(map(x, function(y) get_riskval_from_pos_id(y)), sum))
      
      fd_rvmin <- which(fd_rv == min(fd_rv))
      if(any(is.infinite(fd_rvmin))) browser()
      if(any(is.na(fd_rvmin))) browser()
      filtered_dupes <- c(filtered_dupes, fd_dupes[fd_rvmin])
    }

    path <- path[!names(path) %in% names(dupes)]
    path <- append(filtered_dupes, path)
  }


  if(any(names(path) == "100")){
    #if it did and the new winner has a lower risk value, replace the winner
    if(length(winner) != 0){
      finisher <- path[names(path) == "100"]
      finisher_riskval <- map_dbl(finisher, function(x) reduce(map(x, function(y) get_riskval_from_pos_id(y)), sum))
      min_riskval <- which(finisher_riskval == min(finisher_riskval))
      finisher <- finisher[min_riskval]

      if(any(finisher_riskval < as.numeric(names(winner)))){
        winner <- finisher[1]
        names(winner) <- finisher_riskval[min_riskval][1]
      }
      
      path <- path[!names(path) == 100]
      
    } else {
      winner <- path[names(path) == max(out$pos_id)]
      names(winner) <- reduce(map(winner[[1]], function(x) get_riskval_from_pos_id(x)), sum)
      path <- path[!names(path) == 100]
      
      if(!is.na(i)){
        i <- NA
        pos <- c(1,1)
        path <- list(get_coord_pos_id(pos)) #initial
        names(path) <- get_coord_pos_id(pos) 
      }
    }
  }
  
  #print(head(path))
  #print(length(path))
  i <- i + 1
}

tock <- Sys.time()
tick-tock

final_riskval <- as.numeric(names(winner)) - 1
