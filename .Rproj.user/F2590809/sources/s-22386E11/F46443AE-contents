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


winner <- list()
travelled_paths <- list()

tick <- Sys.time()
tries <- 1000

for(try in 1:tries){
  ##
  i <- 1
  pos <- c(1,1)
  path <- list(get_coord_pos_id(pos)) #initial
  names(path) <- get_coord_pos_id(pos) #initial
  end_path <- FALSE
  ##
  while(end_path == FALSE){
    pos_id <- get_coord_pos_id(pos)
    
    directions <- list("right" = c(pos[1], pos[2] + 1),
                       "left" = c(pos[1], pos[2] - 1),
                       "up" = c(pos[1] - 1, pos[2]),
                       "down" = c(pos[1] + 1, pos[2])) %>% 
      keep(~all(.x > 0)) %>% 
      map(get_coord_pos_id)  %>% 
      keep(~all(!.x %in% path[[1]])) %>%  #check that new position hasn't been visited before
      compact()
    
    if(length(directions) == 0){
      
      end_path <- TRUE
      
    } else {
      
      if(any(map_lgl(directions, ~.x == max(out$pos_id)))){
        #check if path reaches end
        directions <- compact(directions[directions == max(out$pos_id)])
      } else if(length(winner) == 0){
        if((i %% 2) == 0){
          directions <- directions$right
        } else {
          directions <- directions$down
        }
      } else {
        directions <- directions[[sample(1:length(directions))[1]]]
      }
      
      path <- map2(path[names(path) == pos_id], directions, ~c(.x, .y))
      names(path) <- tail(path[[1]], 1)
      
      riskval <- map(path, function(x) reduce(map(x, function(y) get_riskval_from_pos_id(y)), sum))
      names(riskval) <- names(path)
      
      pos <- get_coord_from_pos_id(tail(path[[1]], 1))
      i <- i + 1
      
      #is there a winner with a lower risk value
      if(length(winner) != 0){
        if(riskval > as.numeric(names(winner))){
          end_path <- TRUE
        }
      } 
      
      #did the path complete?
      if(any(map_lgl(directions, ~.x == max(out$pos_id)))){
        end_path <- TRUE
        
        #if it did and the new winner has a lower risk value, replace the winner
        if(length(winner) != 0){
          if(riskval < as.numeric(names(winner))){
            winner <- path
            names(winner) <- riskval
          }
        } else {
          winner <- path
          names(winner) <- riskval
        }
      }
    }
  }
  
  names(path) <- riskval
  travelled_paths <- c(travelled_paths, path)
}

tock <- Sys.time()
print(tick - tock)
#print(travelled_paths)

keep(travelled_paths, ~tail(.x, 1) == 100)
