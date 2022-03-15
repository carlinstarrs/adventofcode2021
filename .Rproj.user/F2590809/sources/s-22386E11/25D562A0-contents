library("tidyverse")

test_input <- readLines("day14/test_input.txt")
input <- readLines("day14/input.txt")

#simultaneously consider all three pairs:
get_pairs <- function(x){
  pts <- c()
  for(i in 1:(nchar(x)-1)){
    pts <- c(pts, substr(x, i, i+1))
  }
  return(table(pts))
}

#the second value in a pair will get repeated
get_overlaps <- function(pairs){
  overlaps <- map_chr(names(pairs), ~substr(.x, 2, 2))
  return(setNames(c(pairs, use.names = FALSE)*-1, overlaps))
}


polymerize <- function(input, steps){
  polymer_template <- input[1]
  rules <- tibble(input[3:length(input)]) %>% 
    separate(col = 1, into = c("rule1", "rule2"), sep = "->") %>% 
    mutate(rule1 = trimws(rule1), 
           rule2 = paste0(substr(rule1, 1, 1), trimws(rule2), substr(rule1, 2, 2)), 
           dict = map(rule2, ~table(strsplit(.x, split = "")[[1]])), 
           new_pairs = map(rule2, function(x){
             get_pairs(x)
           })) 
  
  for(i in 1:steps){
    if(i == 1){
      pts <- get_pairs(polymer_template)
      last_letter <- setNames(1, substr(polymer_template,nchar(polymer_template),nchar(polymer_template)))
    } else {
      pts <- new_pairs
    }
    
    overlaps <- get_overlaps(pts)

    pp <- imap(names(pts), function(x,y) unlist(rules$dict[rules$rule1 == x]) * pts[y])
    
    out <- c(unlist(pp), overlaps, last_letter)
    counter <- tapply(out, names(out), sum, na.rm = TRUE)
    
    new_pairs <- imap(names(pts), function(x,y) unlist(rules$new_pairs[rules$rule1 == x]) * pts[y]) %>% unlist()
    new_pairs <- tapply(new_pairs, names(new_pairs), sum, na.rm = TRUE)
  }
  print(counter)

  out_diff <- counter[which.max(counter)]-counter[which.min(counter)]
  return(print(out_diff, digits = nchar(out_diff)))
}

polymerize(test_input, 10)
polymerize(input, 10)

polymerize(test_input, 40)
polymerize(input, 40)