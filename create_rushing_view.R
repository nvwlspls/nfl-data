source("get_table.R")

read_pfr_table <- function(filename){
  
  df <- read.csv(filename)
  
  # remove records that are really just headers
  df <- df[df$Tm != "Tm",]
  
  return(df)
  
}
# TODO: Create a parse names function

create_rushing_stats_view <- function(min_year = 1960, max_year = 2020, 
                                      data_dir = "./data/seasons"){
  
  # calculate the rushing fantasy points for the year
  for (year in strtoi(min_year):strtoi(2020)){
    
    this_filename <- paste0(data_dir, "/", "nfl_stat_", year, "_rushing.csv")
    
    rushing_datafame <- read_pfr_table(this_filename)
    
    # in each row calculate the total fantasy points and add that to a new 
    # column
    rushing_datafame$fantasy_points <- 
      apply(rushing_datafame, 1,function(x){
        
        yards_points <- round((strtoi(x[["Yds"]]) / 10 ), 2)
        td_points <- round((strtoi(x[["TD"]]) * 6), 2 )
        fumble_points <- round((strtoi(x[["Fmb"]]) * -2), 2)
        
        return(sum(c(yards_points, td_points, fumble_points)))
      })

    rushing_data_frame$fantasy_rank <- 
      order(rushing_datafame$fantasy_points, decreasing = TRUE)
  }
  
}

create_rushing_stats_view()