my.seasonal.fun <- function(forecastList){
  
  # Initialize an empty list to store the plots for each list item
  all_plots <- vector("list", length = length(forecastList))
  
  # Loop through each data frame in the list
  for (i in 1:length(forecastList)) {
    
    # Extract the data from the current list
    time_series <- forecastList[[i]]$time_series
    
    # Filter the time series data to only include data after 2008
    time_series <- window(time_series, start = 2008)
    
    # Create and display the plots
   plots <- ggseasonplot(time_series, year.labels = TRUE, year.labels.left = TRUE) + ylab("Ridership (UPT)") + ggtitle("Seasonal Plot of",paste(names(forecastList[i]),"Ridership"))
   
   plots2 <- ggseasonplot(time_series, polar = TRUE) + ylab("Ridership (UPT)") + ggtitle("Seasonal Plot of",paste(names(forecastList[i]),"Ridership"))
    
    # Capture the current set of plots
    all_plots[[i]] <- list(plots, plots2)
    
    output_filename1 <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/Dec polar plots", paste0("seasonal_plot_", names(forecastList[i]), ".png"))
    ggsave(output_filename1, plots2, width = 5)
    output_filename2 <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/Dec seasonal plots", paste0("seasonal_plot_", names(forecastList[i]), ".png"))
    ggsave(output_filename2, plots, width = 5)
    
  }
  
  # Print or display the combined plots
  print(all_plots)
  
}
