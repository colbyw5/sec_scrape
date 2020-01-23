#' Extract State abbreviation from free text
#'
#' This function reads and parses state abbreviaiton from SEC Edgar 
#' filings
#' 
#' 
#' @param state_text text in which a marker for state (e.g. 'state of incorportion)
#' @param marker is the state marker to detect
#' @return state abbreviation
#' 

require(tidyverse)
require(stringdist)
require(knitr)

extractState <- function(state_text, marker){
  
  # creating dataset of states and abbreviations to extract
  
  states <- data.frame(state_abb = c(state.abb, "DC", "DC", "US", "PR", "US"),
                       state_name = tolower(c(state.name, "District of Columbia", "Washington D", "United States", "Puerto Rico", "National Banking Association")))
  
  state_extract <- tolower(paste(states$state_name, collapse = "|"))
  
  # finding state marker location
  
  state_text = cleanText(state_text)
  
  marker_location <- mean(unlist(str_locate(state_text, marker)))
  
  # extracting state closest to marker
  
  marker_distance <- abs(rowMeans(str_locate_all(state_text, state_extract)[[1]]) - marker_location)
  
  state <- unlist(str_extract_all(state_text, state_extract))[which.min(marker_distance)]
  
  state <- ifelse(state %in% states$state_name, state, NA)
  
  # correcting for state mispellings by one letter
  
  if (length(state) == 0 & (nchar(state_text) > 0)){
    
    state <- states$state_name[which(sapply(str_split(state_text, pattern = " ")[[1]], stringdist, states$state_name) == 1) %% length(states$state_name)]
    
  }
  
  # if state is found, return abbreviation
  
  if(length(state) != 0){
    
    state_abb <- states$state_abb[states$state_name == state]
    
  }else{
    
    state_abb <- ""
  }
  
  return(as.character(state_abb))
  
}
