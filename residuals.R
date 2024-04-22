my.res.fun <- function(forecastList){
  
  # Initialize lists for the residuals and p-values
  resList <- list()
  pValuesList <- list()
  
  # Open a sink to capture output
  sink("/dev/null")
  
  # Loop through my forecast list and pull out residuals for each forecast
  for (i in 1:length(forecastList)) {
    dataInput <- forecastList[[i]]
    
    # Find the residuals for each forecast, would use lag = 24 but for post results the hybrid returns NA
    ets.residuals <- checkresiduals(dataInput$ets.forecast, lag = 12)
    arima.residuals <- checkresiduals(dataInput$arima.forecast, lag = 12)
    stlets.residuals <- checkresiduals(dataInput$stlets.forecast, lag = 12)
    stlarima.residuals <- checkresiduals(dataInput$stlarima.forecast, lag = 12)
    tbats.residuals <- checkresiduals(dataInput$tbats.forecast, lag = 12)
    nnet.residuals <- checkresiduals(dataInput$nnet.forecast, lag = 12)
    
    # Find residuals for hybrid separately because checkresiduals() won't work
    hybrid.residuals <- residuals(dataInput$hybrid.forecast)
   
    # Perform the Ljung-Box test on the hybrid and store the p-values
    lboxHybrid <- Box.test(hybrid.residuals, lag = 12, type = "Ljung-Box")
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
    
    outputDir <- "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/pre residuals"
    
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
  
  pValues <- do.call(rbind, pValuesList)
  
  write_xlsx(list(pValues), path = "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/data/preRes.xlsx")
  
  return(list(residuals = resList, pValues = pValuesList))
}
