split_line <- function(text){
  if(length(text) == 1){
    text <- text %>% str_split("\n") %>% .[[1]]
  }
  return(text)
}

mean_length <- function(text){
  text %>%
    str_length %>%
    mean(na.rm = T)
}


split_at <- function(text, trim){
  col1 <- text %>% 
    substring(1, trim - 1)
  col2 <- text %>% 
    str_sub(trim, 10000)
  
  c(col1, col2)
}

page_width <- function(text){
  text %>%
    str_length %>%
    sort(decreasing = T) %>% 
    .[1:round(length(text)/5)] %>%
    median(na.rm = T)
}

check_space_structure <- function(text, round = F){
  
  text <- text %>% 
    split_line
  
  med_length <- text %>%
    str_length() %>%
    keep(~.x > 0) %>%
    median(na.rm = T)
  
  trans_cut <- seq(med_length) %>%
    map_chr(~{
      text %>%
        str_sub(.x, .x) %>%
        paste0(collapse = "")
    })
  
  res <- round(str_count(trans_cut, " |\\-")/str_length(trans_cut), 2)
  
  if(round){tmp <- ifelse(tmp < .8, 0, tmp)}
  
  return(res)
}

trim_column <- function(col_text, benchmark = .94){
  
  if(mean_length(col_text) == 0){ return(NA)}
  
  width <- page_width(col_text)
  med <- width/2
  penalty <- ifelse(length(col_text) <= 7, 0, 5)
  
  space_structure <- col_text %>%
    .[1:(length(col_text)-penalty)] %>%
    str_pad(width, "right", " ") %>%
    str_sub(med - 20, med + 20) %>%
    check_space_structure() %>%
    zoo::rollmean(k = 2)
  
  if(!any(space_structure > benchmark)){return(NA)}
  
  # cumsum(space_structure == 1)
  
  
  # max <- which.max( space_structure - lead(space_structure, 1))[1] + med - 20
  max <- which.max( space_structure)[1] + med - 20
  return(max)
  
}

split_column <- function(text){
  
  text %>% #bashR::simule_map(1)
    # paste(collapse = "\n") %>%
    map_chr(~{
      
      trim <- .x %>%
        split_line %>%
        trim_column()
      
      if(is.na(trim)){return(.x)}
      
      .x %>% 
        split_line %>%
        split_at(trim) %>%
        paste(collapse = "\n") 
    }) 
  
}
