my.plot.fun <- function(forecastList){
  
  # Initialize an empty list to store the plots for each list item
  all_plots <- vector("list", length = length(forecastList))
  
  # Loop through each data frame in the list
  for (i in 1:length(forecastList)) {
    
    # Find the common y-axis range based on the minimum and maximum of time_series_test
    ymin <- min(forecastList[[i]]$time_series_train * 0.5) # post, train * 0.5 or pre, train
    ymax <- max(forecastList[[i]]$time_series_test * 1.5) # post, test * 1.5 or pre, train
    
    # Set a smaller range for x so the pre-COVID plots are legible
    # xmin <- c(2010) #pre
    # xmax <- c(2020+2/12) #pre
    xmin <- c(2020) #full
    xmax <- c(2023+8/12) #full
    
    # Extract the forecasts and testing data from the current list
    fets <- forecastList[[i]]$ets.forecast
    farima <- forecastList[[i]]$arima.forecast
    fstlets <- forecastList[[i]]$stlets.forecast
    fstlarima <- forecastList[[i]]$stlarima.forecast
    ftbats <- forecastList[[i]]$tbats.forecast
    fnnet <- forecastList[[i]]$nnet.forecast
    fhybrid <- forecastList[[i]]$hybrid.forecast
    time_series_test <- forecastList[[i]]$time_series_test
    
    # Create and display the plots for each forecast type (remove xlim = c(xmin, xmax), for post)
    plot_ets <- autoplot(fets, main = "ETS", series = "Forecast", xlab = "Date", ylab = "Ridership (UPT)") +
      autolayer(fets$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      guides(colour=guide_legend(title=paste(names(forecastList[i])))) +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_arima <- autoplot(farima, main = "ARIMA", xlab = "Date", ylab = element_blank()) +
      autolayer(farima$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_stlets <- autoplot(fstlets, main = "STLETS", xlab = "Date", ylab = element_blank()) +
      autolayer(fstlets$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_stlarima <- autoplot(fstlarima, main = "STLARIMA", xlab = "Date", ylab = element_blank()) +
      autolayer(fstlarima$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_tbats <- autoplot(ftbats, main = "TBATS", xlab = "Date", ylab = "Ridership (UPT)") +
      autolayer(ftbats$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_nnet <- autoplot(fnnet, main = "NNET", xlab = "Date", ylab = element_blank()) +
      autolayer(fnnet$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    plot_hybrid <- autoplot(fhybrid, main = "Hybrid (ANST)", xlab = "Date", ylab = element_blank()) +
      autolayer(fhybrid$mean,series = "Forecast") +
      autolayer(time_series_test, series = "Testing Data") +
      scale_colour_manual(values = c("blue","red")) + 
      coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
      scale_x_yearmon(format="%m/%y",n=3)
    
    # Create a single plot containing all sub-plots in one row
    nt <- theme(legend.position='none')
    mt <- theme(axis.text.y = element_blank())
    combined_plots <- grid_arrange_shared_legend(plot_ets+nt, plot_arima+nt+mt, plot_stlets+nt+mt, plot_stlarima+nt+mt, plot_tbats+nt, plot_nnet+nt+mt, plot_hybrid+mt, ncol=4, nrow = 2, widths = c(2.4, 2, 2, 2))
    
    # Capture the current set of plots
    all_plots[[i]] <- combined_plots
    
    # Save the combined plot as an image
    output_filename <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/full series plots right", paste0("forecast_plot_", names(forecastList[i]), ".png"))
    ggsave(output_filename, combined_plots, width = 10)
    
  }
  
  # Print or display the combined plots
  print(all_plots)
}
