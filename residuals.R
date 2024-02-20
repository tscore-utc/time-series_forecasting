my.res.fun <- function(forecastList){
  # This function performs the residual diagnostics of your forecasts using the Ljung-Box test. It produces: the time plots, the ACF plots of residuals, and a histogram of the residuals with an overlaid normal distribution for comparison. The plots will be saved to your files in .jpg format; for all but the hybrid models, the three plots are all in a single picture. The function also records the p-values of the residuals; a higher p-value (less significant) indicates a lack of pattern in the residuals (i.e., the residuals are white noise), meaning the models captured all information in the data. A significant p-value means some information remains in the residuals; this is not an indication that the model will produce poor forecasts, but rather that it could be improved. Additionally, models whose residuals are white noise may also be able to be improved.
  
  
  # Initialize lists for the residuals and p-values
  resList <- list()
  pValuesList <- list()
  
  # Open a sink to capture output
  sink("/dev/null")
  
  # Loop through my forecast list (output of forecasting function) and pull out residuals for each forecast
  for (i in 1:length(forecastList)) {
    dataInput <- forecastList[[i]]
    
    # Find the residuals for each forecast
    ets.residuals <- checkresiduals(dataInput$ets.forecast)
    arima.residuals <- checkresiduals(dataInput$arima.forecast)
    stlets.residuals <- checkresiduals(dataInput$stlets.forecast)
    stlarima.residuals <- checkresiduals(dataInput$stlarima.forecast)
    tbats.residuals <- checkresiduals(dataInput$tbats.forecast)
    nnet.residuals <- checkresiduals(dataInput$nnet.forecast)
    
    # Find residuals for hybrid separately because checkresiduals() won't work
    hybrid.residuals <- residuals(dataInput$hybrid.forecast)
   
    # Perform the Ljung-Box test on the hybrid and store the p-values
    lboxHybrid <- Box.test(hybrid.residuals, lag = 10, type = "Ljung-Box")
    pValueHybrid <- lboxHybrid$p.value
    
    # Store residuals
    resList[[i]] <- list(
      ets = ets.residuals,
      arima = arima.residuals,
      stlets = stlets.residuals,
      stlarima = stlarima.residuals,
      tbats = tbats.residuals,
      nnet = nnet.residuals,
      hybrid = hybrid.residuals
    )
    
    # Extract p-values from residuals
    pValuesList[[i]] <- data.frame(
      Agency = names(forecastList)[i],
      Model = c("ETS", "ARIMA", "STLETS", "STLARIMA", "TBATS", "NNET", "Hybrid (ANST)"),
      P.Values =c(ets.residuals$p.value,
      arima.residuals$p.value,
      stlets.residuals$p.value,
      stlarima.residuals$p.value,
      tbats.residuals$p.value,
      nnet.residuals$p.value,
      pValueHybrid)
    )
      
    # Assign the names from the old list to items in new list
    names(resList)[i] <- names(forecastList)[i]
    names(pValuesList)[i] <- names(forecastList)[i]
    
    # Set the path in your files where you want to save the residuals to
    outputDir <- "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/post residuals"
    
    # Save residuals charts as image files
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_ETS.jpg")))
    checkresiduals(dataInput$ets.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_ARIMA.jpg")))
    checkresiduals(dataInput$arima.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_STLETS.jpg")))
    checkresiduals(dataInput$stlets.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_STLARIMA.jpg")))
    checkresiduals(dataInput$stlarima.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_TBATS.jpg")))
    checkresiduals(dataInput$tbats.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_NNET.jpg")))
    checkresiduals(dataInput$nnet.forecast)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_Hybrid.jpg")))
    hybrid.plot1 <- autoplot(hybrid.residuals)
    print(hybrid.plot1)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_Hybrid_histogram.jpg")))
    hybrid.plot2 <- gghistogram(hybrid.residuals)
    print(hybrid.plot2)
    dev.off()
    
    jpeg(file.path(outputDir, paste0(names(forecastList)[i], "_residuals_Hybrid_acf.jpg")))
    hybrid.plot3 <- ggAcf(hybrid.residuals)
    print(hybrid.plot3)
    dev.off()
    
  }
  
  # Close the sink to restore normal output
  sink()
  
  # Save excel with p-values to your files
  write_xlsx(pValuesList, path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/postRes_dec.xlsx")
  
  # Output the residuals to your environment
  return(list(residuals = resList, pValues = pValuesList))
}
