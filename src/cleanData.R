#' Read raw text file from SEC Edgar, remove markings and header
#'
#' This function reads and parses 10-K filings hosted on 
#' SEC Edgar
#' 
#' 
#' @param path_to_file path to local file
#' @return processed raw text file from SEC Edgar
#' 

require(tidyverse)

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