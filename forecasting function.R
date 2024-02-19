my.forecast.fun <- function(dtList) {
  # Set a seed to ensure the same random parameters are used for nnetar each time
  set.seed(1234)
  
  # Initialize an empty list to store the time series objects and forecasting models
  tsList <- list()
  
  # Create an empty data frame to store accuracy results
  accuracy_df <- data.frame()
  
  # Loop through each data frame in the list
  for (i in 1:length(dtList)) {
    dataInput <- dtList[[i]]
    
    # Convert "trips" to a numeric type (if it's a character or factor)
    trips <- as.numeric(gsub(",| ", "", dataInput$trips))
    dates <- dataInput$dates
    
    # Create ts object
    rides_ts <- ts(trips, start = c(year(min(dates)), month(min(dates))), freq = 12)
    
    # Subset the data into train and test using indexing
    # train_start_date <- c(2020, 4)
    train_end_date <- c(2022, 8)
    test_start_date <- c(2022, 9)
    
    # Create ts objects for training and testing data using the window function
    rides_ts_train <- window(rides_ts, end = train_end_date)
    rides_ts_test <- window(rides_ts, start = test_start_date)
    
    # Create a list for each time series object, including the time series object and forecasting models
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
    )
    
    # Calculate forecasts for each model and add them to the list
    tsModelList$ets.forecast <- forecast(tsModelList$ets.model, h = 12)
    tsModelList$arima.forecast <- forecast(tsModelList$arima.model, h = 12)
    tsModelList$stlets.forecast <- forecast(tsModelList$stlets.model, h = 12)
    tsModelList$stlarima.forecast <- forecast(tsModelList$stlarima.model, h = 12)
    tsModelList$tbats.forecast <- forecast(tsModelList$tbats.model, h = 12)
    tsModelList$nnet.forecast <- forecast(tsModelList$nnet.model, PI = TRUE, h = 12)
    tsModelList$hybrid.forecast <- forecast(tsModelList$hybrid.model, h = 12)
    
    # Calculate accuracy for the models and add to list
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
    
    # Assign the name of the data frame to everything
    tsList[[names(dtList)[i]]] <- rides_ts
    
    # Store the list of time series object, forecasting models, and accuracy measures in the empty list we made
    tsList[[i]] <- tsModelList

  }
  
  write_xlsx(accuracy_df, path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/accuracy_results_bigdata.xlsx")
  
  return(tsList)
}
