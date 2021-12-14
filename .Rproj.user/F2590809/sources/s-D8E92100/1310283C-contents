#Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:

#The submarine is still trying to display numbers by producing output on signal wires a through g, 
# but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed 
# up separately for each four-digit display!

#if 2 segments are on, it has to equal 1 (c and f)

#make a note of all ten unique signal patterns you see, and then write down a single four digit output value

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
  ungroup()

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

dict <- data.frame("letter1" = letters[1:7])

tt <- test_input %>% 
  parse_input() %>% 
  right_join(disp_ref, by = "char", suffix = c("_sp", "_ref")) %>% 
  group_by(id, pat_sp) %>% 
  mutate(count = n()) %>% 
  ungroup()

smol <- tt %>% 
  filter(count == 1) %>% 
  slice_min(char)

dict$letter2[dict$letter1 %in% strsplit(smol$pat_sp, "")[[1]]] <- smol$pat_ref
