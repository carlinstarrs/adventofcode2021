test_input <- readLines("day10/test_input.txt")
day10_input <- readLines("day10/input.txt")

syntax_checker <- function(input){
  pairss <- setNames(c(")", "}", "]", ">"), c("(", "{", "[", "<")) 
  mismatches <- c()
  illegal_chars <- c()
  autocompletes <- c()
  for(i in 1:length(input)){
    line1 <- input[i]
    check <- TRUE
    patt <- c("(\\(\\))|(\\{\\})|(<>)|(\\[\\])")
    while(check == TRUE){
      line1 <- str_remove(line1, patt)
      check <- any(grepl(patt, line1))
    }
    
    if(any(grepl("\\)|\\}|\\]|>", line1))){
      illegal_chars <- c(illegal_chars, str_split(line1, "")[[1]][min(which(str_split(line1, "")[[1]] %in% pairss))])
      mismatches <- c(mismatches, pairss[str_split(line1, "")[[1]][min(which(str_split(line1, "")[[1]] %in% pairss)) - 1]]) 
    } else {
      autocompletes <- c(autocompletes, paste(rev(map_chr(str_split(line1, "")[[1]], ~pairss[.x])), collapse = ""))
    }
  }
  
  score_autocompletes <- function(aa){
    scores <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)
    total_score <- c(0)
    aa2 <- c(scores[str_split(aa, "")[[1]]], use.names = FALSE)
    for(i in 1:length(aa2)){
      total_score <- (total_score * 5) + aa2[i]
    }
    return(total_score)
  }
  
  scored_autocompletes <- sort(map_dbl(autocompletes, ~score_autocompletes(.x)))
  scored_autocompletes[ceiling(length(scored_autocompletes)/2)]
  
  
  return(list("corrupted_score" = sum(table(illegal_chars) * c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)), 
              "autocomplete_score" = scored_autocompletes[ceiling(length(scored_autocompletes)/2)]))
}

syntax_checker(test_input)
syntax_checker(day10_input)
