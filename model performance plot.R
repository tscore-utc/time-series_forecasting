my.modelPerfPlotter.fun <- function(my.df) {
  # This function takes the output Excel sheets from the performance functions and plots by method instead of by agency. This will show if any one method outperformed in general.
  
  # library(ggplot2)
  # library(tidyr)
  # library(zoo)
  # library(ggsci)
  # library(gridExtra)
  # library(scales)
  # library(cowplot)
  
  # Make an empty list to hold all the plots
  all_plots <- vector("list", length = 7)
  
  # Split data frame into list of data frames for each forecasting method
  myList <- split(my.df, my.df$Model)
  
  # Loop through list to plot each model's performance measures over time
  for (i in 1:length(myList)) {
    
    # Pull out a method and put into a data frame
    df <- as.data.frame(myList[[i]])
    
    # Set agency name as row name and remove agency and method columns
    rownames(df) <- df[,1]
    df <- df[,-c(1, 2)]
    
    # Transpose data frame
    df <- t(df)
    
    # Convert the array back into a data frame
    df <- as.data.frame(df)
    
    # Add dates in as yearmon objects
    df$Dates <- as.yearmon(rownames(df), "%b %Y")
    
    # Convert data frame to long format
    df <- pivot_longer(df, cols = -Dates, names_to = "Agency", values_to = "MASE")
    
    # Plot the performance measures over time (date is first observation of testing data)
    performancePlot <- ggplot(df, aes(x = as.Date(Dates), y = MASE, color = Agency)) +
      geom_line() +
      labs(x = "Testing Window Start Date", y = paste(names(myList)[i], "Method"), title = "Change in MASE with Change in Testing Window Start Date") +
      scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8), plot.title = element_text(size = 12))
    
    # Add plot to list
    all_plots[[i]] <- performancePlot
    
    # Save the plots as an image
    output_filename <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/slidingPostMASEMethod", paste0("sliding_MASE_", names(myList)[i], ".png"))
    
    ggsave(output_filename, performancePlot, width = 10, height = 6)
    
  }
  
  # Print plots
  print(all_plots)
  
}
