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

exists_check <- function(stat, year){
  
  target_filename <- paste0("nfl_stat_", year, "_", stat, ".csv")
  
  return(target_filename %in% list.files("."))

}

stats_list <- c(
  "passing",
  "rushing", 
  "receiving", 
  "scrimmage", 
  "defense",
  "kicking",
  "returns", 
  "scoring"
  #TODO add in advanced stats urls
  # "passing_adv" ,
  # "rushing_adv" ,
  # "receiving_adv", 
  # "defense_adv",
  # "team_adv"
)


# for each year go around and collect all of the stats
for (year in 1960:2020){
  
  for (stat in stats_list){
    
    url <- paste0("https://www.pro-football-reference.com/years/", paste(year),
                  "/", stat, ".htm")
    
    # look in the directory if the file already exists skip this iteration
    file_is_here <- exists_check(stat, year)
    if (file_is_here == TRUE){
      
      print(paste0("nfl_stat_", year, "_", stat, ".csv"," already exists"))
      next
    }
    if (stat == "scrimmage"){
      selector_stat <- "receiving_and_rushing"
    } else {
      selector_stat <- stat
    }
    this_stat_table <- -1
    tryCatch(
        this_stat_table <- get_table(url, selector_stat), 
        error = function(c){
          print(paste0(stat, " not available for ", paste0(year)));
        }
    )
    
    if (this_stat_table != -1){
      write.csv(this_stat_table, 
                file = paste0("nfl_stat_", year, "_", stat, ".csv"))
      print(paste0('just wrote', year, " : " ,stat))
    }

  }
}