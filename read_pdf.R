read_pdftools <- function(path){
  pdftools::pdf_text(path)
}

read_xpdf <- function(path){
  tmp <- tm::readPDF(engine = "xpdf", control = list(text = "-layout -fixed 3 -enc UTF-8"))(elem = list(uri = path), language = "de")
  out <- paste0(tmp$content, sep = "\n", collapse = "\n") %>%
    stringr::str_split("\f") %>% 
    .[[1]] %>%
    stringr::str_split("\n")
  return(out)
}


read_pdf <- function(path, method = "pdftool"){
  text <- switch(
    method, 
    "pdftool" = try(read_pdftools(path), silent = T),
    "xpdf" = try(read_xpdf(path), silent = T)
  )
  
  if((inherits(text, "try-error") | all(text == "")) & method  == "pdftool"){
    text <- try(read_xpdf(path), silent = T)
  }
  
  if(inherits(text, "try-error")| all(unlist(text) == "")){
    message("Text could not be read")
    return(NA_character_)
  } else {
    return(text)
  }
  
}