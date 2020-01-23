#' Clean text from  SEC Edgar file
#' 
#' 
#' @param text text to be cleaned
#' @return text in lower case without symbols or whitespace
#' 

require(tidyverse)

cleanText <- function(text) {
  
  text <- tolower(text)
  
  text <- str_replace_all(text, "[^:[:alnum:]]", " ")
  
  text <- str_squish(text)
  
  return(text)
}