library("tidyverse")

test_input <- readLines("day15/test_input.txt")
day15_input <- readLines("day15/day15_input.txt")
#add up the risk levels of each position you enter

#the starting position is never entered

#Your goal is to find a path with the lowest total risk.
#you cannot move diagonally

#What is the lowest total risk of any path from the top left to the bottom right?
input <- test_input



parse_input <- function(input){
  ll <- nchar(input[1])
  
  out <- data.frame(input) %>% 
    mutate(row_id = 1:nrow(.)) %>% 
    separate_rows(input, sep = "") %>% 
    filter(!input == "") %>% 
    mutate(col_id = rep_len(1:ll, length.out = nrow(.)), 
           pos_id = 1:nrow(.))
}




find_winner <- function(input){
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
  
  get_total_riskval <- function(x){
    reduce(map_dbl(x, ~get_riskval_from_pos_id(.x)), sum)
  }
  
  out <- parse_input(input)
  end <- max(out$pos_id)
  
  winner <- tibble()
  i <- 1
  path <- tibble("x" = 1, 
                 "y" = 1, 
                 "pos_id" = get_coord_pos_id(c(1,1)), 
                 "path" = as.list(pos_id), 
                 "riskval" = get_riskval_from_pos_id(pos_id))
  ##
  tryCatch(
    while(nrow(path) > 0){
      
      pos <- c(path$x[1], path$y[1])
      pos_id <- path$pos_id[1]
      
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
      
      
      new_path <- map_dfr(path$path[path$pos_id == pos_id], function(p){
        map_dfr(directions, function(d){
          if(!any(p == d)){ #check that new coordinate hasn't been visited
            pos2 <- get_coord_from_pos_id(d)
            tibble("x" = pos2[1],
                   "y" = pos2[2],
                   "pos_id" = d, 
                   "path" = list(c(p, d)), 
                   "riskval" = get_total_riskval(unlist(path)), 
            )
          } else {
            return(NULL)
          }
        }) 
      }) 
      
      path <- bind_rows(new_path, path[path$pos_id != pos_id,])
      
      #check riskval is less than winner's riskval
      if(length(winner) > 0){
        path <- path[path$riskval < winner$riskval,] 
      } 
      
      #prune any duplicate final nodes to the one with the minimum current risk value
      path <- path %>% 
        group_by(pos_id) %>% 
        slice(which.min(riskval)) %>% 
        arrange(riskval)
      
      if(any(path$pos_id == end)){
        #if it did and the new winner has a lower risk value, replace the winner
        if(length(winner) != 0){
          
          if(any(path$riskval < winner$riskval)){
            winner <- path %>% 
              filter(pos_id == end) %>% 
              slice(which.min(riskval))
          }
          
          path <- path[path$pos_id != end,]
          
        } else {
          winner <- path[path$pos_id == end]
          
          path <- path[path$pos_id != end,]
          
          if(!is.na(i)){
            i <- NA
            path <- tibble("x" = 1, 
                           "y" = 1, 
                           "pos_id" = get_coord_pos_id(c(1,1)), 
                           "path" = as.list(pos_id), 
                           "riskval" = get_riskval_from_pos_id(pos_id))
          }
        }
      }
      
      #print(head(path$path))
      #print(nrow(path))
      i <- i + 1
    }
    , error = function(e) browser())
}



tick <- Sys.time()
find_winner(test_input)
tock <- Sys.time()
tick-tock

final_riskval <- winner$riskval - 1
