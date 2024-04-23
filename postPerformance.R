my.performance.fun <- function(dtList){
  # This function creates 12-month forecasts using a minimum of 25 data points (for my data, the post-COVID training dataset needs to span at least from April 2020 to April 2022). The full series forecasts will be done for the same periods as the post-COVID to allow for comparison. The function collects the MAPE and MASE of each forecast for later plotting.
  # Very computationally intense on computer, could only do 8 agencies at a time.
  
  # library(forecast)
  # library(forecastHybrid)
  # library(data.table)
  # library(writexl)
  # library(zoo)
  
  # Set a seed to ensure the same random parameters are used for nnetar each time
  set.seed(1234)
  
  # Initialize an empty list to store the time series objects and forecasting models
  tsList <- list()
  
  # Create empty data frames to store performance measures
  MAPE_df <- data.frame(Agency = character(), Model = character(), stringsAsFactors = FALSE)
  MASE_df <- data.frame(Agency = character(), Model = character(), stringsAsFactors = FALSE)
  
  # Loop through each data frame in the list (from the separating function)
  for (i in 1:length(dtList)) {
    cat("Processing data frame:", i, "\n")
    dataInput <- dtList[[i]]
    
    # Convert "trips" to a numeric type (if it's a character or factor)
    trips <- as.numeric(gsub(",| ", "", dataInput$trips))
    dates <- dataInput$dates
    
    # Create time series object
    rides_ts <- ts(trips, start = c(year(min(dates)), month(min(dates))), freq = 12)
    
    # Create data frame for MAPE results
    MAPE_results <- data.frame(
      Agency = rep(names(dtList)[i], each = 7),
      Model = rep(c("ETS", "ARIMA", "STLETS", "STLARIMA", "TBATS", "NNET", "Hybrid (ANST)"), each = 1),
      stringsAsFactors = FALSE)
    
    # Create data frame for MASE results
    MASE_results <- data.frame(
      Agency = rep(names(dtList)[i], each = 7),
      Model = rep(c("ETS", "ARIMA", "STLETS", "STLARIMA", "TBATS", "NNET", "Hybrid (ANST)"), each = 1),
      stringsAsFactors = FALSE)
    
    # Forecast for each possible testing window, starting from test_start_date (through Dec 2023 is from 0:8)
    for (j in 0:8) {
      cat("  - Processing iteration j =", j, "\n")
      
      # Subset the data into train and test
      test_start_date <- c(2022+4/12) + c(j/12)
      test_end_date <- test_start_date + c(11/12)
      train_start_date <- c(2002) # April 2020 = 2020+3/12 (for post-only), 2002 for full
      train_end_date <- test_start_date - c(1/12)
      
      # Create ts objects for training and testing data using the window function
      rides_ts_train <- window(rides_ts, start = train_start_date, end = train_end_date)
      rides_ts_test <- window(rides_ts, start = test_start_date, end = test_end_date)
      
      # Create a list for each time series object, including the time series object and time series models
      tsModelList <- list(
        time_series = rides_ts,
        time_series_train = rides_ts_train,
        time_series_test = rides_ts_test,
        ets.model = ets(rides_ts_train),
        arima.model = auto.arima(rides_ts_train, stepwise = F),
        stlets.model = stlm(rides_ts_train, method = 'ets'),
        stlarima.model = stlm(rides_ts_train, method = 'arima'),
        tbats.model = tbats(rides_ts_train, use.parallel = TRUE),
        nnet.model = nnetar(rides_ts_train),
        hybrid.model = hybridModel(rides_ts_train, models = "anst",
                                   s.args = list(method = 'arima'),
                                   errorMethod = "RMSE",
                                   weights = "equal",
                                   cvHorizon = 12,
                                   parallel = TRUE)
      ) # can specify number of seasonal lags used in the nnetar model, e.g., P = 12 for monthly data
      
      # Estimate forecasts for each model and add them to the list
      tsModelList$ets.forecast <- forecast(tsModelList$ets.model, h = 12)
      tsModelList$arima.forecast <- forecast(tsModelList$arima.model, h = 12)
      tsModelList$stlets.forecast <- forecast(tsModelList$stlets.model, h = 12)
      tsModelList$stlarima.forecast <- forecast(tsModelList$stlarima.model, h = 12)
      tsModelList$tbats.forecast <- forecast(tsModelList$tbats.model, h = 12)
      tsModelList$nnet.forecast <- forecast(tsModelList$nnet.model, PI = TRUE, h = 12) # PI simulates 1000 forecasts for prediction intervals
      tsModelList$hybrid.forecast <- forecast(tsModelList$hybrid.model, h = 12)
      
      # Estimate model performance and add to list
      tsModelList$ets.accuracy <- accuracy(tsModelList$ets.forecast, rides_ts_test)
      tsModelList$arima.accuracy <- accuracy(tsModelList$arima.forecast, rides_ts_test)
      tsModelList$stlets.accuracy <- accuracy(tsModelList$stlets.forecast, rides_ts_test)
      tsModelList$stlarima.accuracy <- accuracy(tsModelList$stlarima.forecast, rides_ts_test)
      tsModelList$tbats.accuracy <- accuracy(tsModelList$tbats.forecast, rides_ts_test)
      tsModelList$nnet.accuracy <- accuracy(tsModelList$nnet.forecast, rides_ts_test)
      tsModelList$hybrid.accuracy <- accuracy(tsModelList$hybrid.forecast, rides_ts_test)
      
      # Add MAPE and MASE results to earlier data frames
      column_name <- paste0(as.yearmon(test_start_date)) # month of first testing observation
      MAPE_results[[as.character(column_name)]] <- c(tsModelList$ets.accuracy[10], tsModelList$arima.accuracy[10], tsModelList$stlets.accuracy[10], tsModelList$stlarima.accuracy[10], tsModelList$tbats.accuracy[10], tsModelList$nnet.accuracy[10], tsModelList$hybrid.accuracy[10])
      MASE_results[[as.character(column_name)]] = c(tsModelList$ets.accuracy[12], tsModelList$arima.accuracy[12], tsModelList$stlets.accuracy[12], tsModelList$stlarima.accuracy[12], tsModelList$tbats.accuracy[12], tsModelList$nnet.accuracy[12], tsModelList$hybrid.accuracy[12])
      
    }
    
    # Row bind the accuracy results to the main accuracy data frames
    MAPE_df <- rbind(MAPE_df, MAPE_results)
    MASE_df <- rbind(MASE_df, MASE_results)
    
    # Assign the agency names to everything
    tsList[[names(dtList)[i]]] <- rides_ts
    
    # Store the list of time series objects, forecasting models, and accuracy measures in the empty list we made
    tsList[[i]] <- tsModelList
    
    cat("Done with i =", i, "\n")
  }
  
  # Save the accuracy results to my files
  write_xlsx(MAPE_df, path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/slidingWindow_MAPE_full1.xlsx") # replace my path with yours
  write_xlsx(MASE_df, path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/slidingWindow_MASE_full1.xlsx") # replace my path with yours
  
  # Output models
  return(tsList)
  
}
