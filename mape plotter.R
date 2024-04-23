my.performancePlotter.fun <- function(df, forecastList) {
  # This function takes the output Excel sheets from the performance functions and plots them alongside the ridership series. I'm sure this could be done MUCH more efficiently, but this works.
  
  # library(ggplot2)
  # library(tidyr)
  # library(zoo)
  # library(ggsci)
  # library(gridExtra)
  # library(scales)
  # library(cowplot)
  
  # Make an empty list to hold all the plots
  all_plots <- vector("list", length = 14)
  
  # If file uploaded without column names, set column names and remove the row they're in
  # colnames(df) <- df[1, ]
  # df <- df[-1, ]
  
  # Split data frame into list of data frames for each individual agency
  myList <- split(df, df$Agency)
  
  # Loop through list to plot each agency' separately's performance measures over time
  for (i in 1:length(myList)) {
    
    # Transpose data frame (make sure to grab the same agency, since splitting the data frame earlier may have changed the order of agencies)
    df <- t(myList[[paste(names(forecastList[i]))]])
    
    # Set column names as the model name and remove model name row and agency name row
    colnames(df) <- df[2, ]
    df <- df[-c(1, 2), ]
    
    # Convert the array back into a data frame
    df <- as.data.frame(df)
    
    # Add dates in as yearmon objects
    df$Dates <- as.yearmon(rownames(df), "%b %Y")
    
    # Convert data frame into long format
    df <- pivot_longer(df, cols = -Dates, names_to = "Model", values_to = "MAPE") # change according to if you're plotting MAPE/MASE
    
    # Convert MAPE/MASE into a numeric
    df$MAPE <- as.numeric(df$MAPE) # change according to if you're plotting MAPE/MASE
    
    # Plot the performance measures over time (date is first observation of testing data)
    performancePlot <- ggplot(df, aes(x = as.Date(Dates), y = MAPE, color = Model)) +
      geom_line() +
      labs(x = "Testing Window Start Date", y = "MAPE", title = "Change in MAPE with Change in Testing Window Start Date") +
      scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") +
      scale_color_lancet() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8), plot.title = element_text(size = 12))
    
    # Select window for time series
    ts_window <- window(forecastList[[i]]$time_series, start = c(2020, 4))
    
    # Convert ts object to dataframe
    ts_window <- data.frame(trips = as.matrix(ts_window), dates = as.Date(as.yearmon(time(ts_window))))
    
    # Plot the time series
    tsPlot <- ggplot(ts_window, aes(x = dates, y = trips, color = "Ridership")) +
      geom_line() +
      labs(x = "Date", y = "Ridership (UPT)", title = paste("Ridership at", names(forecastList[i]))) +
      scale_x_date(labels = date_format("%b %Y"), date_breaks = "2 months") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.text = element_text(size = 9), legend.title = element_text(size = 9), plot.title = element_text(size = 13)) +
      labs(color = "Series") +
      geom_vline(xintercept = as.numeric(as.Date("2022-05-01")), linetype = "dashed", color = "gray") +
      annotate("text", x = as.Date("2022-05-01"), y = max(ts_window$trips), label = "Begin Testing Windows", vjust = 1, hjust = 0, color = "black", size = 3)
    
    # Combine the two plots onto one image making sure they're aligned
    alignment <- align_plots(performancePlot, tsPlot, align = "v")
    combinePlot <- plot_grid(performancePlot, tsPlot, ncol = 1, align = "v")
    
    # Add plot to list
    all_plots[[i]] <- combinePlot
    
    # Save the plots as an image
    output_filename <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/slidingPostMAPE", paste0("sliding_MAPE_", names(forecastList[i]), ".png"))
    ggsave(output_filename, combinePlot, width = 10, height = 6)
    
  }
  
  # Print plots
  print(all_plots)
  
}
