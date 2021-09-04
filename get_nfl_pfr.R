library("XML")
library("RCurl")
library("stringr")

#  get our functions
source("get_table.R")

exists_check <- function(output_dir, filename){

  # get the last item in filename split list
  filename_tail <- tail(str_split(filename, "/")[[1]], n=1 )
  
  return(filename_tail %in% list.files(output_dir))

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

get_nfl_pfr_data <- function(min_year = 1960, max_year = 2020, 
                             output_dir = "./data/seasons"){
  
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
  for (year in strtoi(min_year):strtoi(max_year)){
    for (stat in stats_list){
      this_filename <- paste0(output_dir, "/", "nfl_stat_", year, "_", stat, 
                              ".csv" )
      url <- paste0("https://www.pro-football-reference.com/years/", paste(year),
                    "/", stat, ".htm")
      
      # look in the directory if the file already exists skip this iteration
      file_is_here <- exists_check(output_dir, this_filename)
      
      if (file_is_here == TRUE){
        
        print(paste0(this_filename, " already exist"))
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
                  file = this_filename)
        print(paste0('just wrote', year, " : " ,stat))
      }
    }
  }
}

get_nfl_pfr_data()
