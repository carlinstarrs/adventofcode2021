library("tidyverse")
test_input <- read.csv("day13/test_input.txt", header = FALSE)
input <- read.csv("day13/input.txt", header = FALSE)

instructions <- input %>% 
  filter(grepl("fold", .[,1])) %>% 
  pull(1)

coords <- input %>% 
  filter(!grepl("fold", .[,1])) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  select(x = 1, y = 2) 

coords <- coords + 1 #one indexing...

grows <- as.numeric(str_extract(instructions[1:2][which(grepl("y=", instructions[1:2]))], "\\d+"))
gcols <- as.numeric(str_extract(instructions[1:2][which(grepl("x=", instructions[1:2]))], "\\d+"))

#grid size should be based on first 2 folds * 2
fg <- matrix(nrow = grows*2+1, ncol = gcols*2+1)
fg[is.na(fg)] <- 0

pwalk(coords, function(x,y) fg[y,x] <<- 1)

walk(instructions, function(inst){
  fold_line <- as.numeric(str_extract(inst, "\\d+")) + 1
  fold_axis <- str_remove(str_extract(inst, "(x|y)="), "=")

  #split the matrix
  if(fold_axis == "y"){
    p1 <- fg[(1:(fold_line-1)),]
    p2 <- fg[((fold_line+1):nrow(fg)),]
    p2 <- p2[nrow(p2):1,]
    
    fg <<- matrix(map2(c(p1), c(p2), ~sum(.x, .y, na.rm = TRUE)), ncol = ncol(p1))
  } else {
    p1 <- fg[,(1:(fold_line-1))]
    p2 <- fg[,((fold_line+1):ncol(fg))]
  
    fg <<- matrix(map2(c(p1), c(p2[,ncol(p2):1]), ~sum(.x, .y, na.rm = TRUE)), nrow = nrow(p1))
  }
})

length(fg[fg!=0])

tt <- data.frame(fg) %>% 
  rowid_to_column() %>% 
  pivot_longer(cols = -rowid) %>%
  mutate(colid = as.numeric(str_extract(name, "\\d+")), 
         rowid = rowid * -1)

ggplot(tt) + 
  geom_tile(aes(y = rowid, x = colid, fill = value), stat = "identity", position = "identity") 
