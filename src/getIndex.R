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

#' Read raw text file from SEC Edgar, remove markings and header
#'
#' This function reads and parses 10-K filings hosted on 
#' SEC Edgar
#' 
#' 
#' @param path_to_file path to local file
#' @return processed raw text file from SEC Edgar
#' 

cleanData <- function(path_to_file){
  
  
  
  # Unzip file
  
  R.utils::gunzip(path_to_file, destname = "./temp_filing_dir/master.txt", temporary = FALSE, skip = FALSE, overwrite = TRUE, remove = TRUE)
  
  # Removing ''' to allow scan for '|' to run without fail due to occurrence of ''' in company name
  
  raw_data <- gsub("'", "", readLines("./temp_filing_dir/master.txt"))
  
  # Marking end of file description
  
  header_end <- grep("--------------------------------------------------------", raw_data, useBytes = TRUE)
  
  # Writting back to storage
  
  writeLines(raw_data, "./temp_filing_dir/master.txt")
  
  scraped_data <- scan("./temp_filing_dir/master.txt", what = list("", "", "", "", ""), flush = F, skip = header_end, sep = "|", quiet = T)
  
  # Remove punctuation characters from company names
  
  company_name <- gsub("[[:punct:]]", " ", scraped_data[[2]], perl = T)
  
  # Produce final dataset
  
  final_data <- data.frame(cik = scraped_data[[1]], company.name = company_name, form.type = scraped_data[[3]], date.filed = scraped_data[[4]], edgar.link = paste("https://www.sec.gov/Archives/", scraped_data[[5]], sep = ""))
  
  
  return(final_data)
  
}


#' Get index of filings from SEC Edgar
#'
#' This function extracts the index file of all SEC filings from
#' SEC edgar, given a year range, quarter selection and form type
#' 
#' @param years range of years for index
#' @param quarter selection from 1-4 for quarters of index
#' @param form_types vector of form types to be included index
#' @return dataframe of filing index


# cleans raw text filing from SEC Edgar
#source("./cleanData.R")
# removes symbols and whitespace from text
#source("./src/cleanText.R")

getIndex <- function(years, quarter, form_types){
  
  # Create temporary directory for filing
  
  dir.create("./temp_filing_dir")
  
  # initialzing list to keep filing data
  
  filing_index <- list()
  
  for (year in years){
    
    for (quarter in 1:4){
      
      # constructing link to filing
      
      file_link <- paste("https://www.sec.gov/Archives/edgar/full-index/", year, "/QTR", quarter, "/master.gz", sep = "")
      
      # download zipped file
      
      utils::download.file(file_link, destfile = "./temp_filing_dir/master.idx", quiet = TRUE)
      
      # clean data from file
      
      filing_data <- cleanData(path_to_file = "./temp_filing_dir/master.idx")
      
      # filter for filing type of interest, adding to list of filing indices
      
      filing_index[[paste(year, quarter, sep = "_")]] <- filing_data[filing_data$form.type %in% form_types,]
      
    }
  }
  
  # Delete temporary directory
  
  unlink("./temp_filing_dir", recursive = TRUE)
  
  # Closing open file connections
  
  closeAllConnections()
  
  ## state will be the state abbreviation
  
  inc_links <- bind_rows(filing_index)
  
  return(inc_links)
  
}