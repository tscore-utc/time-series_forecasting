my.bigplots.fun <- function(tsData) {
  # Initialize ggplots for the plots
  bigplotNY <- ggplot()
  bigplotLarge <- ggplot()
  bigplotSmall <- ggplot()
  
  # Initialize the series vector
  series <- vector("list", length = length(tsData))
  
  for (i in 1:length(tsData)) {
    series[[i]] <- tsData[[i]]$time_series
    
    if (i %in% c(1, 3, 4, 6, 7, 9, 12, 13, 14)) {
      bigplotLarge <- bigplotLarge + autolayer(series[[i]], series = names(tsData)[i]) + 
        ylab("Ridership (UPT)") +
        xlab("Date") + 
        labs(color = "Agency") +
        geom_vline(xintercept = 2020+3/12, size = 0.2) + 
        geom_vline(xintercept = 2021-2/12, size = 0.2) + 
        geom_vline(xintercept = 2021+11/12, size = 0.2) + 
        annotate("text", x = 2020+3/12, y = 22000000, label = "Stay-at-Home", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021-2/12, y = 22000000, label = "Delta", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021+11/12, y = 22000000, label = "Omnicron", hjust = 0, vjust = 0, angle = 90, cex = 3)
    } 
    
    else if (i == 2) {
      bigplotNY <- bigplotNY + autolayer(series[[i]], series = names(tsData)[i]) + 
        ylab("Ridership (UPT)") +
        xlab("Date") + 
        labs(color = "Agency") +
        geom_vline(xintercept = 2020+3/12, size = 0.2) + 
        geom_vline(xintercept = 2021-2/12, size = 0.2) + 
        geom_vline(xintercept = 2021+11/12, size = 0.2) + 
        annotate("text", x = 2020+3/12, y = 200000000, label = "Stay-at-Home", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021-2/12, y = 200000000, label = "Delta", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021+11/12, y = 200000000, label = "Omnicron", hjust = 0, vjust = 0, angle = 90, cex = 3)
    } 
    
    else {
      bigplotSmall <- bigplotSmall + autolayer(series[[i]], series = names(tsData)[i]) +  
        ylab("Ridership (UPT)") +
        xlab("Date") + 
        labs(color = "Agency") +
        geom_vline(xintercept = 2020+3/12, size = 0.2) +
        geom_vline(xintercept = 2021-2/12, size = 0.2) + 
        geom_vline(xintercept = 2021+11/12, size = 0.2) + 
        annotate("text", x = 2020+3/12, y = 1600000, label = "Stay-at-Home", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021-2/12, y = 1600000, label = "Delta", hjust = 0, vjust = 0, angle = 90, cex = 3) +
        annotate("text", x = 2021+11/12, y = 1600000, label = "Omnicron", hjust = 0, vjust = 0, angle = 90, cex = 3)
    }
    
    
  }
  
  bigplotLarge <- bigplotLarge + scale_color_viridis_d(option = "turbo") + theme(legend.text = element_text(size = 7)) + guides(color = guide_legend(label.wrap = TRUE))
  bigplotSmall <- bigplotSmall + scale_color_viridis_d() + theme(legend.text = element_text(size = 7))
  
  # Save the plots to this location
  output_dir <- "/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/big plots"
  
  # Save the first plot as an image
  output_filename1 <- file.path(output_dir, "Big_Plot_NY.png")
  ggsave(output_filename1, plot = bigplotNY, width = 10)
  
  # Save the second plot as an image
  output_filename2 <- file.path(output_dir, "Big_Plot_Large.png")
  ggsave(output_filename2, plot = bigplotLarge, width = 10)
  
  # Save the third plot as an image
  output_filename3 <- file.path(output_dir, "Big_Plot_Small.png")
  ggsave(output_filename3, plot = bigplotSmall, width = 10)
  
  
  print(bigplotLarge)
  print(bigplotNY)
  print(bigplotSmall)
}
