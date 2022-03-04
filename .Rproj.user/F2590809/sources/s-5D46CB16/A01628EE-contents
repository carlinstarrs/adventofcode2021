library("tidyverse")
library("tidygraph")
library("ggraph")
#the only way to know if you've found the best path is to find all of them.

#start in the cave named start, and your destination is the cave named end

#find the number of distinct paths that start at start, 
# end at end, and don't visit small caves more than once

#all paths you find should visit small caves at most once, 
# and can visit big caves any number of times.

test_input <- readLines("day12/test_input.txt")
big_test_input <- readLines("day12/bigger_test_input.txt")
day12_input <- readLines("day12/input.txt")

cave_dive <- function(input){
  caves <- imap_dfr(input, ~tibble("val" = str_split(.x, pattern = "-")[[1]]), .id = "id") %>% 
    group_by(id) %>% 
    mutate(dir = c("from", "to")) %>%
    ungroup()
  
  smol_caves <- unique(caves$val[str_detect(caves$val, "[[:lower:]]")])
  
  cnx <- caves %>% 
    pivot_wider(id_cols = id, 
                names_from = dir, 
                values_from = val)  %>% 
    bind_rows(select(., from = to, to = from)) %>% 
    filter(to != "start", from != "end") %>% 
    mutate(id = 1:nrow(.))
  
  
  step <- 0
  validated <- data.frame()
  node <- cnx %>% filter(from == "start") 
  while(nrow(node) > 0){
    if(step != 0){
      node <- node %>% filter(.[,step+3] != "end")
    }
    
    
    next_node <- cnx %>% 
      filter(from %in% unlist(node[,step+3])) 
    
    node <- pmap_dfr(node, function(...){
      dat <- tibble(...)
      next_steps <- data.frame(next_node$to[next_node$from %in% dat[,step+3]])
      names(next_steps) <- paste0("STEP", step)
      
      bind_cols(dat, next_steps)
    }) 
    
    node <- pmap_dfr(node, function(...){
      check <- table(c(..., use.names = FALSE))
      if(any(check[names(check) %in% smol_caves] > 1)){
        return(NULL)
      } else {
        return(tibble(...))
      }
    })
    
    if(ncol(node) > 0){
      validated <- validated %>% 
        bind_rows(node %>% filter(.[,step+4] == "end"))
    }
    
    step  <- step + 1
    print(nrow(validated))
  }
  
  return(validated)
}

cave_dive(test_input)
out <- cave_dive(big_test_input)
out <- cave_dive(day12_input)

#### PART 2 ####
cave_dive <- function(input){
  caves <- imap_dfr(input, ~tibble("val" = str_split(.x, pattern = "-")[[1]]), .id = "id") %>% 
    group_by(id) %>% 
    mutate(dir = c("from", "to")) %>%
    ungroup()
  
  smol_caves <- unique(caves$val[str_detect(caves$val, "[[:lower:]]")])
  smol_cave_counter <- table(smol_caves) - 1
  
  cnx <- caves %>% 
    pivot_wider(id_cols = id, 
                names_from = dir, 
                values_from = val)  %>% 
    bind_rows(select(., from = to, to = from)) %>% 
    filter(to != "start", from != "end") %>% 
    mutate(id = 1:nrow(.))
  
  
  step <- 0
  validated <- data.frame()
  node <- cnx %>% filter(from == "start") 
  while(nrow(node) > 0){
    if(step != 0){
      node <- node %>% filter(.[,step+3] != "end")
    }
    
    
    next_node <- cnx %>% 
      filter(from %in% unlist(node[,step+3])) 
    
    node <- pmap_dfr(node, function(...){
      dat <- tibble(...)
      next_steps <- data.frame(next_node$to[next_node$from %in% dat[,step+3]])
      names(next_steps) <- paste0("STEP", step)
      
      bind_cols(dat, next_steps)
    }) 
    
    node <- pmap_dfr(node, function(...){
      check <- table(c(..., use.names = FALSE))
      smol_check_caves <- check[names(check) %in% smol_caves]

      if(any(smol_check_caves[names(smol_check_caves) %in% c("start", "end")] > 1)){
        return(NULL)
      } else if(any(smol_check_caves > 2)){
        return(NULL)
      } else if(length(smol_check_caves[smol_check_caves == 2]) > 1){
        return(NULL)
      } else {
        return(tibble(...))
      }
      
    })
    
    if(ncol(node) > 0){
      validated <- validated %>% 
        bind_rows(node %>% filter(.[,step+4] == "end"))
    }
    
    step  <- step + 1
    print(nrow(validated))
  }
  
  return(validated)
}

out <- cave_dive(test_input)
out <- cave_dive(big_test_input)
out <- cave_dive(day12_input)

