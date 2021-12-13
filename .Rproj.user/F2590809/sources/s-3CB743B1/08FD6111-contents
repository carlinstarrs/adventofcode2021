library("tidyverse")

test_fileloc <- "day04/test_input.txt"
day04_fileloc <- "day04/input.txt"

parse_input <- function(fileloc){
  list("draws" = as.numeric(read.csv(fileloc, header = FALSE, nrows = 1)[1,]),
       "cards" = read.table(fileloc, header = FALSE, skip = 1) %>% 
         mutate(card_id = rep(1:(nrow(.)/5), each = 5)) %>% 
         mutate(row_id = rep_len(1:5, length.out = nrow(.))) %>% 
         pivot_longer(cols = matches("^V.$"), 
                      names_to = "col_id", 
                      values_to = "value") %>% 
         mutate(col_id = as.numeric(str_remove(col_id, "V"))))
}

test_input <- parse_input(test_fileloc)
day04_input <- parse_input(day04_fileloc)

play_bingo <- function(input){
  draws <- input$draws
  cards <- input$cards 
  
  i <- 1
  winner_check <- FALSE
  while(winner_check == FALSE){
    cards$value[cards$value == draws[i]] <- NA
    cards <- cards %>% 
      group_by(card_id, row_id) %>% 
      mutate(row_score = sum(is.na(value))) %>% 
      ungroup() %>%
      group_by(card_id, col_id) %>% 
      mutate(col_score = sum(is.na(value))) %>% 
      ungroup()
 
    winner_check <- any(cards$col_score == 5 | cards$row_score == 5)
    i <- i + 1
  }
  
  winner <- cards %>% 
    group_by(card_id) %>% 
    filter(any(col_score == 5 | row_score == 5)) %>% 
    mutate(score = sum(value, na.rm = TRUE)) %>% 
    pull(score) %>% 
    unique()
  
  return(winner * draws[i - 1])
  
}

play_bingo(test_input)
play_bingo(day04_input)

#### PART 2 ####
lose_bingo <- function(input){
  draws <- input$draws
  cards <- input$cards
  
  i <- 1
  loser_check <- FALSE
  while(loser_check == FALSE){
    cards$value[cards$value == draws[i]] <- NA
    cards <- cards %>% 
      group_by(card_id, row_id) %>% 
      mutate(row_score = sum(is.na(value))) %>% 
      ungroup() %>%
      group_by(card_id, col_id) %>% 
      mutate(col_score = sum(is.na(value))) %>% 
      ungroup() %>% 
      group_by(card_id) %>% 
      mutate(winner = any(col_score == 5 | row_score == 5)) %>% 
      ungroup()
    
    if(sum(cards$winner) == (nrow(cards)  - 25)){
      last_winner <- cards %>% 
        filter(winner == FALSE)
      last_draw <- draws[i + 1]
    }

    loser_check <- sum(cards$winner) == nrow(cards)
    i <- i + 1

  }
  
  loser <- cards %>% 
    filter(card_id == unique(last_winner$card_id)) %>% 
    mutate(score = sum(value, na.rm = TRUE)) %>% 
    pull(score) %>% 
    unique()
  
  return(loser * last_draw)
}

lose_bingo(test_input)
lose_bingo(day04_input)
