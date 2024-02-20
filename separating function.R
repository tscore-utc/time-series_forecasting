my.dt.fun <- function(myData){
  # This function is used to clean up your data, specifically a file with one column of dates and more than one column of ridership data. This function will take your file and convert it into a list of data tables. I demonstrate how to use this function using the .xlsx file I downloaded from the NTD database (https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release). If you aren't using this specific dataset, you probably want to edit or comment everything out down to the ####. If you only have one time series to forecast with, you really only need to make the data into a data table, parse your dates (lubridate), and make sure your date column is named "dates" and your ridership data is named "trips".
  
  # library(readxl)
  # library(data.table)
  # library(lubridate)
  
  # Import the UPT sheet from the .xlsx file using the readxl package, making sure your column names are added as a row (the dates are contained in the column names). I already did this so I will pass myData from my global environment into my function.
  # myData <- read_excel("Downloads/December 2023 Complete Monthly Ridership (with adjustments and estimates).xlsx", sheet = "UPT", col_names = FALSE)
  
  # Set the first row as the column names
  colnames(myData) <- as.character(myData[1, ])
  
  # Select heavy rail only. Keep the row with the column names
  newData <- myData[myData$Mode == "HR" | myData$`NTD ID` == "NTD ID", ]
  
  # Drop any row that contains NA
  newData <- newData[complete.cases(newData), ]
  
  # I also chose to drop Honolulu (row 15) and San Juan (row 12). They won't work with the following functions due to having no data for several years (zeroes)
  newData <- newData[-c(12, 15), ]
  
  # Transpose
  tData <- t(newData)
  
  # Make agency name the column name. Our date column will be named "Agency"
  colnames(tData) <- as.character(tData[3, ])
  
  # Reduce to agency names, dates, and ridership info only 
  tData <- tData[-c(1:10), ]
  
  ####
  
  # Make the array into a data table using the data table package. If you have a dataframe, use setDT() instead of as.data.table()
  dt <- as.data.table(tData)
  
  # Convert the dates into a usable format using the lubridate package. See the lubridate cheatsheet if your dates aren't in mm/yyyy format.
  dt$Agency <- parse_date_time(dt$Agency, "my") # update "my" for your date format
  
  # Make an empty list
  agency_dataframes <- list()
  
  # Identify the date column which we previously named "Agency"
  date_col <- "Agency"
  
  # Count columns for the loop
  num_cols <- ncol(dt)
  
  # Loop to separate all the agencies into dates-ridership
  for (i in 2:(num_cols)) {  # Skip first column with dates
    agency_name <- colnames(dt)[i]
    
    # Select the date column and the current agency's trip column
    agency_dataframe <- dt[, .(dates = dt[[date_col]], trips = dt[[agency_name]])]
    
    # Add the separated data series to the empty list we made before
    agency_dataframes[[agency_name]] <- agency_dataframe
  }
  
  # Output the resulting list to our global environment
  return(agency_dataframes)
}
