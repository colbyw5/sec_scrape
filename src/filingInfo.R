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


#' Extract information from 10-K filing on SEC Edgar
#'
#' This function reads and parses 10-K filings hosted on 
#' SEC Edgar
#' 
#' 
#' @param file_link the url provided by the SEC filing index
#' @param file_lines number of lines from file to be parsed
#' @param state_markers text fields used to find the state of incorporation
#' @return dataframe of state, cik, filing period, filing, url and cik from the filing url
#' 

require(tidyverse)
require(htm2txt)

#######
#Test

#file_link <- "https://www.sec.gov/Archives/edgar/data/726517/0000726517-95-000019.txt"
#file_lines <- 1500
#state_markers <- c("state of in", "incorporated under", "state or oth", "incorporated in", "state or")
######

filingInfo <- function(file_link, file_lines = 1500, state_markers = c("state of incorporation")){
  
  # loading states dataset
  
  states <- data.frame(state_abb = c(state.abb, "DC", "DC", "US", "PR"),
                       state_name = tolower(c(state.name, "District of Columbia", "Washington D", "United States",
                                              "Puerto Rico")))
  
  state_extract <- tolower(paste(states$state_name, collapse = "|"))
  
  # read in first 500 lines of file
  
  filing_text <- tryCatch(
    
    {paste(readr::read_lines(file = file_link, n_max = file_lines), collapse = " ")
    },error = function(cond){
      message(paste(cond, "\n", file_link))
      
      
      
      filing_data <- data_frame(state = NA,
                                cik = NA,
                                filing_period = NA,
                                edgar.link = file_link,
                                file_cik = str_split(file_link, "/")[[1]][7])
      
      return(filing_data)
    })
  
  if (is.data.frame(filing_text)){
    return(filing_text)
    break
  } 
  
  # remove html markup, quotes and extra white space, convert to lower case
  
  if (str_detect(filing_text, pattern = "<div")){
    filing_text <- htm2txt(filing_text)
  }
  
  filing_text <- cleanText(filing_text)
  
  
  
  # test if state is in the header: if yes, read it, no: search for marker, return surrounding text
  
  state_header <- str_detect(filing_text, "state of incorporation:")
  
  # initiate dataframe for collection
  
  filing_data <- data_frame(state = character(),
                            cik = character(),
                            filing_period = character(),
                            link_cik = character())
  
  if (state_header){
    
    # if state is found in header, extracting the abbreviation
    
    state_marker <- data.frame(str_locate_all(filing_text, "state of incorporation")[[1]]) %>% 
      mutate(mark = "state",
             location = (start + end)/2)
    
    cik_marker <- data.frame(str_locate_all(filing_text, "central index key")[[1]]) %>% 
      mutate(mark = "cik",
             location = (start + end)/2)
    
    # arranging cik and states to extract in order of file
    
    markers <- bind_rows(state_marker, cik_marker) %>% 
      arrange(location, mark) %>% 
      filter((mark == "cik" & mark != lead(mark)) | (mark == "state" & mark != lag(mark)))
    
    state_marker <- data.frame(start = markers$start[markers$mark == "state"],
                               end = markers$end[markers$mark == "state"])
    
    cik_marker <- data.frame(start = markers$start[markers$mark == "cik"],
                             end = markers$end[markers$mark == "cik"])
    
    for (cik in 1:nrow(cik_marker)){
      
      filing_data[cik, 1] <- toupper(str_sub(filing_text, start = state_marker$end[cik] + 3, end = state_marker$end[cik] + 4))
      
      filing_data[cik, 2] <- str_sub(filing_text, start = cik_marker$end[cik] + 3, end = cik_marker$end[cik] + 12)
    }
    
    filing_data$edgar.link <- file_link
    
  }else{
    
    # parsing text body for state markers
    
    exit_loop <- FALSE
    
    for (marker in state_markers){
      
      if (str_detect(filing_text, marker)){
        
        state_marker <- str_locate(filing_text, marker)
        
        state_text <- str_sub(filing_text, start = max(state_marker[1] - 500, 0), end = min(state_marker[2] + 500, nchar(filing_text)))
        
        state_abb <- extractState(state_text = state_text, marker = marker)
        
        filing_data[1, 1] <- state_abb
        
        filing_data[1, 2] <- str_pad(str_split(file_link, '/')[[1]][7], side = "left", width = 10, pad = "0")
        
        filing_data$edgar.link <- file_link
        
        # breaking if state has been found
        if (nchar(state_abb) > 1){
          exit_loop <- TRUE
        }
      }
      
      if (exit_loop){
        break
      }
    }
  }
  
  # extracting filing period
  
  if (str_detect(filing_text, "period of report") & nrow(filing_data) > 0){
    
    period_marker <- str_locate(filing_text, "period of report")[2]
    
    filing_data$filing_period <- str_sub(filing_text, start = period_marker + 2, end = period_marker + 10)
    
    
    
    
    
  }
  
  # add cik from url
  
  if (nrow(filing_data) > 0){
    filing_data$link_cik <- str_pad(str_split(file_link, "/")[[1]][7], side = "left", width = 10, pad = "0")
  }
  
  # return final dataframe
  
  closeAllConnections()
  
  return(filing_data)
}


