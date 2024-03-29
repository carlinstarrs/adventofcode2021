#Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

#The submarine is still trying to display numbers by producing output on signal wires a through g, 
# but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed 
# up separately for each four-digit display!

#if 2 segments are on, it has to equal 1 (c and f)

#make a note of all ten unique signal patterns you see, and then write down a single four digit output value
library("tidyverse")

test_input <- readLines("day08/test_input.txt")
test_input2 <- readLines("day08/test_input2.txt")
day08_input <- readLines("day08/input.txt")

parse_input <- function(input){
  data.frame("val" = input) %>% 
    separate(col = "val", into = c("sp", "output"), sep = "\\|") %>% 
    mutate(id = 1:nrow(.)) %>% 
    pivot_longer(cols = -id, 
                 names_to = "val", 
                 values_to = "pat") %>% 
    mutate(pat = trimws(pat)) %>% 
    separate_rows(pat, sep = " ") %>% 
    mutate("char" = nchar(pat))
}

disp_ref <- data.frame("num" = 0:9, 
                       "pat" = c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")) %>% 
  mutate("char" = nchar(pat)) %>% 
  group_by(char) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  arrange(count, char)

part1 <- function(input) { 
  input %>% 
    parse_input() %>% 
    filter(val == "output") %>% 
    right_join(disp_ref %>% filter(num %in% c(1,4,7,8)), by = "char", suffix = c("_sp", "_ref")) %>% 
    group_by(id, num) %>% 
    mutate(matches = n()) %>% 
    ungroup() %>% 
    nrow()
}


part1(test_input2)
part1(day08_input)


####PART 2####
part2 <- function(input){
  dat <- input %>% 
    parse_input() %>% 
    left_join(disp_ref %>% filter(num %in% c(1,4,7,8)), by = "char", suffix = c("_sp", "_ref"))  
  
  
  str_diff <- function(x, y){
    if(nchar(x) > nchar(y)){
      setdiff(strsplit(x, "")[[1]], strsplit(y, "")[[1]])
    } else {
      setdiff(strsplit(y, "")[[1]], strsplit(x, "")[[1]])
    }
    
  }
  
  lines <- split(dat, dat$id)
  
  doinit <- function(df){
    outs <- df %>% 
      filter(val == "output") %>% 
      pull(pat_sp)
    
    df <- df %>% 
      filter(val == "sp")
    
    top <- str_diff(df$pat_sp[df$num == 7 & !is.na(df$num)][1], df$pat_sp[df$num == 1 & !is.na(df$num)][1])
    bottom <- unique(unlist(keep(map(df$pat_sp[df$char == 6], ~str_diff(.x, paste0(df$pat_sp[df$num == 4 & !is.na(df$num)][1], top))), ~length(.x) == 1)))
    lower_left <- str_diff(df$pat_sp[df$num == 8 & !is.na(df$num)][1], paste0(df$pat_sp[df$num == 4 & !is.na(df$num)][1], top, bottom))
    middle <-  unlist(keep(map(df$pat_sp[df$char == 5], ~str_diff(paste0(df$pat_sp[df$num == 7 & !is.na(df$num)], bottom), .x)), ~length(.x) == 1))
    upper_left <-  str_diff(paste0(df$pat_sp[df$num == 1 & !is.na(df$num)], middle), df$pat_sp[df$num == 4 & !is.na(df$num)])
    
    upd <- which(map_lgl(df$pat_sp, ~identical(str_diff(.x, paste0(df$pat_sp[df$num == 4 & !is.na(df$num)], top, bottom)), character(0))) == TRUE)
    df$num[df$pat_sp == df$pat_sp[upd]] <- 9
    
    upd <- which(map_lgl(df$pat_sp, ~identical(str_diff(.x, str_remove(df$pat_sp[df$num == 8 & !is.na(df$num)], middle)), character(0))) == TRUE)
    df$num[df$pat_sp == df$pat_sp[upd]] <- 0
    
    df$num[df$char == 6 & is.na(df$num)] <- 6
    
    upper_right <- str_diff(df$pat_sp[df$num == 8 & !is.na(df$num)], df$pat_sp[df$num == 6 & !is.na(df$num)])
    
    upd <- which(map_lgl(df$pat_sp, ~identical(str_diff(.x, str_remove_all(df$pat_sp[df$num == 8 & !is.na(df$num)], paste(c(lower_left, upper_right), collapse = "|"))), character(0))) == TRUE)
    df$num[df$pat_sp == df$pat_sp[upd]] <- 5
    
    upd <- which(map_lgl(df$pat_sp, ~identical(str_diff(.x, str_remove_all(df$pat_sp[df$num == 8 & !is.na(df$num)], paste(c(upper_left, lower_left), collapse = "|"))), character(0))) == TRUE)
    df$num[df$pat_sp == df$pat_sp[upd]] <- 3
    
    df$num[df$char == 5 & is.na(df$num)] <- 2
    
    lower_right <- unlist(keep(map(df$pat_sp[df$char == 5 & is.na(df$num)], ~str_diff(str_remove_all(df$pat_sp[df$num == 8 & !is.na(df$num)], paste(c(lower_left, upper_right), collapse = "|")), .x)), ~length(.x) == 1))

    
    out <- c()
    for(i in 1:length(outs)){
      out <- c(out, df$num[which(map_lgl(df$pat_sp, ~identical(str_diff(.x, outs[i]), character(0))) == TRUE)])
    }
    
    return(paste(out, collapse = ""))
  }

  reduce(map(lines, ~as.numeric(doinit(.x))), sum)
  
}

part2(test_input)
part2(test_input2)
part2(day08_input)
