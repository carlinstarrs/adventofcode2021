test_input <- data.frame("vals" = c("2199943210",
                                    "3987894921",
                                    "9856789892",
                                    "8767896789",
                                    "9899965678"))

day09_input <- data.frame("vals" = readLines("day09/input.txt"))

risk_level_calc <- function(input){
  input %>% 
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
    filter(!is.na(value) & check == 4) %>% 
    mutate(risk_level = as.numeric(value) + 1) %>% 
    pull(risk_level) %>% 
    sum()
}

risk_level_calc(test_input)
risk_level_calc(day09_input)

#### PART 2 ####
basin_start <- input[1,1]


input %>% 
  separate(col = vals, into = paste0("V", 0:nchar(.$vals)[1]), sep = "") %>% 
  select(-V0) %>% 
  add_row(.before = 1) %>%
  add_row(.after = nrow(.)) %>%
  add_column("V000" = NA, .before = 1) %>%
  add_column("V999" = NA, .after = ncol(.)) %>%
  mutate(row_id = 1:n()) %>% 
  pivot_longer(cols = -row_id) %>%
  mutate(col_id = as.numeric(str_remove(name, "V"))) %>% 
  select(value, col_id, row_id) 
