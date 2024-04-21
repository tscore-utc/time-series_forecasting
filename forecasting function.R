my.forecast.fun <- function(dtList) {
  # This is the function used to take the output of the separating function, make time series objects from the data, apply the time series models, and then forecast. It also collects the testing MAPE and MASE of each forecast and saves them in an Excel file. 
  # This is a somewhat computationally intensive function and as such, you should expect it to take some time to finish. It takes my laptop anywhere from 2-10 minutes.
  
  # library(forecast)
  # library(forecastHybrid)
  # library(writexl)
  
  
  # Set a seed to ensure the same random parameters are used for nnetar each time
  set.seed(1234)
  
  # Initialize an empty list to store the time series objects and forecasting models
  tsList <- list()
  
  # Create an empty data frame to store performance measures
  accuracy_df <- data.frame()
  
  # Loop through each data frame in the list (from the separating function)
  for (i in 1:length(dtList)) {
    dataInput <- dtList[[i]]
    
    # Convert "trips" to a numeric type (if it's a character or factor)
    trips <- as.numeric(gsub(",| ", "", dataInput$trips))
    dates <- dataInput$dates
    
    # Create time series object
    rides_ts <- ts(trips, start = c(year(min(dates)), month(min(dates))), freq = 12)
    
    # Subset the data into train and test (yyyy, mm)
    train_start_date <- c(2002, 1) # full = (2002, 1), post = (2020, 4)
    train_end_date <- c(2022, 12)
    test_start_date <- c(2023, 1)
    
    # Create ts objects for training and testing data using the window function
    rides_ts_train <- window(rides_ts, start = train_start_date, end = train_end_date)
    rides_ts_test <- window(rides_ts, start = test_start_date)
    
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
    
    # Create a data frame to store accuracy results
    accuracy_results <- data.frame(
      Agency = names(dtList)[i],
      Model = c("ETS", "ARIMA", "STLETS", "STLARIMA", "TBATS", "NNET", "Hybrid (ANST)"),
      MAPE.test = c(tsModelList$ets.accuracy[10], tsModelList$arima.accuracy[10], tsModelList$stlets.accuracy[10], tsModelList$stlarima.accuracy[10], tsModelList$tbats.accuracy[10], tsModelList$nnet.accuracy[10], tsModelList$hybrid.accuracy[10]),
      MASE.test = c(tsModelList$ets.accuracy[12], tsModelList$arima.accuracy[12], tsModelList$stlets.accuracy[12], tsModelList$stlarima.accuracy[12], tsModelList$tbats.accuracy[12], tsModelList$nnet.accuracy[12], tsModelList$hybrid.accuracy[12])
    )
    
    # Bind the accuracy results to the main accuracy data frame
    accuracy_df <- rbind(accuracy_df, accuracy_results)
    
    # Assign the agency names to everything
    tsList[[names(dtList)[i]]] <- rides_ts
    
    # Store the list of time series objects, forecasting models, and accuracy measures in the empty list we made
    tsList[[i]] <- tsModelList

  }
  
  # Save the accuracy results to my files
  write_xlsx(accuracy_df, path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/accuracy_results_fullForecastDec.xlsx") # replace my path with yours
  
  # Output models
  return(tsList)
}
