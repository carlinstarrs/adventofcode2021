library("tidyverse")

test_input <- data.frame("vals" = readLines("day05/test_input.txt"))
day05_input <- data.frame("vals" = readLines("day05/input.txt"))

parse_input <- function(input){
  input %>% 
    separate(col = vals, into = c("c1", "c2"), sep = " -> ") %>% 
    rowid_to_column(var = "id") %>% 
    pivot_longer(cols = -id, 
                 values_to = "coords") %>% 
    select(-name) %>% 
    mutate(coord_id = rep(1:2, length.out = nrow(.))) %>% 
    separate(col = coords, into = c("x", "y"), sep = ",") %>% 
    mutate(x = as.numeric(x), 
           y = as.numeric(y))
}

avoid_vents <- function(input, diagonal = FALSE){
  input %>% 
    parse_input() %>% 
    group_by(id) %>% 
    summarise(xdiff = list(x[coord_id == 1]:x[coord_id == 2]), 
              ydiff = list(y[coord_id == 1]:y[coord_id == 2])) %>% 
    {if (diagonal == FALSE) rowwise(.) %>% filter(length(xdiff) == 1 | length(ydiff) == 1) else .} %>%
    ungroup() %>% 
    unnest(cols = c(xdiff, ydiff)) %>% 
    unite(col = "combined", xdiff, ydiff, sep = ",") %>% 
    group_by(combined) %>% 
    tally() %>% 
    filter(n >= 2) %>% 
    nrow(.)
}

avoid_vents(test_input)
avoid_vents(day05_input)
avoid_vents(test_input, diagonal = TRUE)
avoid_vents(day05_input, diagonal = TRUE)
