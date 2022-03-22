library("tidyverse")
library("compositions")

translator <- tibble("value" = c(0:9, LETTERS[1:6]), 
                     "binary" = c("0000", "0001", "0010", "0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"))

binary <- c("0000", "0001", "0010", "0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111")
values <-  c(0:9, LETTERS[1:6])

hex_dict <- setNames(binary, values)
bin_dict <- setNames(values, binary)

input <- readLines("day16/day16_input.txt")

hex_string1 <- "D2FE28"
hex_string2 <- "38006F45291200"
hex_string3 <- "EE00D40C823060"
hex_string4 <- "8A004A801A8002F478"
hex_string5 <- "620080001611562C8802118E34"
hex_string6 <- "C0015000016115A2E0802F182340"
hex_string7 <- "A0016C880162017C3686B18A3D4780"

type_version <- function(packet){
  version <- unbinary(str_pad(str_sub(packet, 1, 3), 4, "left", 0))
  type_id <-  unbinary(str_pad(str_sub(packet, 4, 6), 4, "left", 0))
  
  return(list("version" = version, 
              "type_id" = type_id))
}

literal_value <- function(bits){
  fives <- list()
  bins <- seq(from = 1, to = nchar(bits), by = 5)
  for(i in 1:length(bins)){
    if(is.na(bins[i+1])){
      i2 <- nchar(bits)
    } else {
      i2 <- bins[i+1] - 1
    }
    fives[[i]] <- str_sub(bits, bins[i], i2)
  }
  
  last_group <- detect_index(fives, ~str_sub(.x,1,1) == 0)
  
  if(last_group < length(fives)){
    lit_leftover <- paste(fives[(last_group+1):length(fives)], collapse = "")
    
    if(all(str_split(lit_leftover, "")[[1]] == 0)){
      lit_leftover <- NA
    }
  } else {
    lit_leftover <- NA
  }
  
  
  lit_value <- unbinary(paste(map_chr(fives[1:last_group], ~str_sub(.x, 2, nchar(.x))), collapse = ""))
  
  return(list("value" = lit_value, 
              "leftover" = lit_leftover))
}

length_type_0 <- function(packet){
  subpacket_bits <- 15
  sp1 <- unbinary(str_sub(packet, 8, subpacket_bits + 7))
  subpackets <- str_sub(packet, subpacket_bits + 8, sp1 + subpacket_bits + 7)
  leftover <- str_sub(packet,  sp1 + subpacket_bits + 8, nchar(packet))
  return(list("subpacket" = subpackets, 
              "leftover" = leftover))
}

length_type_1 <- function(packet){
  subpacket_bits <- 11
  sp1 <- unbinary(str_sub(packet, 8, subpacket_bits + 7))
  subpackets <- str_sub(packet, subpacket_bits + 8, nchar(packet))
  return(list("subpacket" = subpackets, 
              "counter" = sp1))
}

part1 <- function(hex_string){
  packet <- paste(hex_dict[str_split(hex_string, "")[[1]]], collapse = "")
  
  lvs <- list()
  tvs <- list()
  counter <- 1
  leftover <- NA
  
  while(counter > 0){
    packet_tv <- type_version(packet)
    
    tvs <- c(tvs, packet_tv)
    
    if(packet_tv$type_id == 4){ #literal packet
      lv_out <- literal_value(str_sub(packet, 7, nchar(packet)))
      
      if(!is.na(lv_out$leftover)){
        if(length_type_ID == 1){
          counter <- counter - 1
        } else {
          counter <- counter + 1
        }
        packet <- lv_out$leftover
      } else if (!is.na(leftover)){
        packet <- leftover
        leftover <- NA
      } else {
        counter <- 0
      }
      
      lvs <-  c(lvs, list("value" = lv_out$value))
      
    } else { #operator packet
      length_type_ID <- str_sub(packet, 7, 7)
      
      if(length_type_ID == 0){
        packet <- length_type_0(packet) 
        leftover <- packet$leftover
        packet <- packet$subpacket
      } else {
        packet <- length_type_1(packet) #the subpacket becomes the packet
        counter <- counter + packet$counter
        packet <- packet$subpacket
      }
    }
    
  }
  
  return(list("versions" = unlist(tvs[names(tvs) == "version"], use.names = FALSE),
              "type_ids" = unlist(tvs[names(tvs) == "type_id"], use.names = FALSE),
              "lvs" = unlist(lvs, recursive = FALSE, use.names = FALSE)))
  
}


sum(part1(hex_string1)[[1]])
sum(part1(hex_string2)[[1]])
sum(part1(hex_string3)[[1]])
sum(part1(hex_string4)[[1]])
sum(part1(hex_string5)[[1]])
sum(part1(hex_string6)[[1]])
sum(part1(hex_string7)[[1]])
sum(part1(input)[[1]])
