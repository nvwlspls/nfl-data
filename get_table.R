library("XML")
library("RCurl")
library("stringr")

get_table <- function(url, selector){
  
  full_selector <- paste0('//*[@id=\'', selector, '\']')
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  tableNodes <- getNodeSet(url_parsed, full_selector)
  df <- readHTMLTable(tableNodes[[1]])
  
  return(df)
  
}
