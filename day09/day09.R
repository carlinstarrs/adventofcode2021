library("tidyverse")

test_input <- data.frame("vals" = c("2199943210",
                                    "3987894921",
                                    "9856789892",
                                    "8767896789",
                                    "9899965678"))

day09_input <- data.frame("vals" = readLines("day09/input.txt"))

risk_level_calc <- function(input){
  out <- input %>% 
    separate(col = vals, into = paste0("V", 0:nchar(.$vals)[1]), sep = "") %>% 
    select(-V0) %>% 
    add_row(.before = 1) %>% 
    add_row(.after = nrow(.)) %>% 
    add_column("V000" = NA, .before = 1) %>% 
    add_column("V999" = NA, .after = ncol(.)) %>% 
    mutate(row_id = 1:n()) %>% 
    pivot_longer(cols = -row_id) %>%
    mutate(col_id = as.numeric(str_remove(name, "V"))) %>% 
    select(value, col_id, row_id) %>% 
    arrange(row_id, col_id) %>% 
    mutate(right = lead(value), 
           left = lag(value)) %>% 
    arrange(col_id, row_id) %>% 
    mutate(up = lag(value), 
           down = lead(value)) %>% 
    mutate(across(c(right, left, up, down), ~replace_na(value < .x, TRUE))) %>% 
    rowwise() %>% 
    mutate(check = sum(right, left, up, down)) %>% 
    filter(!is.na(value) & check == 4) 
  
  risk_level <- out %>% 
    mutate(risk_level = as.numeric(value) + 1) %>% 
    pull(risk_level) %>% 
    sum()
  
  return(list("low_points" = out, 
              "risk_level" = risk_level))
}

risk_level_calc(test_input)
risk_level_calc(day09_input)

#### PART 2 ####


largest_basins <- function(input){
  get_adjacent_values <- function(lp){
    #add to basin value counter
    basin_values <<- c(basin_values, lp$value)
    
    start_col <- lp$col_id
    start_row <- lp$row_id
    
    lp %>%
      select(right, left, up, down) %>% 
      pivot_longer(cols = everything()) %>% 
      filter(value < 9) %>% 
      pull(name) %>% 
      map(function(x){
        if(x == "left"){
          return(c(start_col - 1, "col_id"))
        } else if(x == "right"){
          return(c(start_col + 1, "col_id"))
        } else if (x == "up"){
          return(c(start_row - 1, "row_id"))
        } else {
          return(c(start_row + 1, "row_id"))
        }
      }) %>% 
      map_dfr(function(x){
        #get adjacent cols/rows from pool
        if(x[2] == "row_id"){
          basin_fg %>% filter(col_id == start_col & row_id == x[1]) 
        } else {
          basin_fg %>% filter(col_id == x[1] & row_id == start_row) 
        }
      })
  }
  
  low_points <- risk_level_calc(input)$low_points
  
  fg <- input %>% 
    separate(col = vals, into = paste0("V", 0:nchar(.$vals)[1]), sep = "") %>% 
    select(-V0) %>% 
    add_row(.before = 1) %>% 
    add_row(.after = nrow(.)) %>% 
    add_column("V000" = NA, .before = 1) %>% 
    add_column("V999" = NA, .after = ncol(.)) %>% 
    mutate(row_id = 1:n()) %>% 
    pivot_longer(cols = -row_id) %>%
    mutate(col_id = as.numeric(str_remove(name, "V"))) %>% 
    select(value, col_id, row_id) %>% 
    arrange(row_id, col_id) %>% 
    mutate(right = lead(value), 
           left = lag(value)) %>% 
    arrange(col_id, row_id) %>% 
    mutate(up = lag(value), 
           down = lead(value)) %>% 
    left_join(low_points %>% select(value, col_id, row_id) %>% mutate(low_point = TRUE), c("value", "col_id", "row_id")) %>% 
    arrange(row_id, col_id) 
  
  basins <- list()
  
  for(i in 1:nrow(fg[fg$low_point == TRUE & !is.na(fg$low_point),])){
    basin_fg <- fg
    basin_values <- c()
    
    #starting low point
    lp <- basin_fg %>% 
      filter(low_point == TRUE) %>% 
      slice(i) 
    
    #remove starting point from pool
    basin_fg <- basin_fg %>% anti_join(lp, c("value", "col_id", "row_id", "right", "left", "up", "down", "low_point"))
    
    while(nrow(lp) > 0){
      adjacents <- get_adjacent_values(lp %>% slice(1))
      
      #remove adjacent points from pool
      basin_fg <- basin_fg %>% anti_join(adjacents, c("value", "col_id", "row_id", "right", "left", "up", "down", "low_point"))
      
      #add to queue
      lp <- lp %>% 
        slice(-1) %>% 
        bind_rows(adjacents)
    }
    
    print(basin_values)
    
    basins <- c(basins, list(basin_values))
  }
  
  basin_sizes <- as.numeric(map_chr(basins, ~length(.x)))
  big_bois <- sort(basin_sizes, TRUE)
  return(prod(big_bois[1:3]))
}

largest_basins(test_input)
largest_basins(day09_input)  
