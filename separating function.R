my.dt.fun <- function(dt){
  
  # make an empty list
  agency_dataframes <- list()
  
  date_col <- "dates"
  
  num_cols <- ncol(dt)
  
  for (agency_index in 1:(num_cols-1)) {  # Skip last column with dates
    agency_name <- colnames(dt)[agency_index]
    
    # Select the date column and the current agency's trip column
    agency_dataframe <- dt[, .(dates = dt[[date_col]], trips = dt[[agency_name]])]
    
    agency_dataframes[[agency_name]] <- agency_dataframe
  }
  
  return(agency_dataframes)
}
