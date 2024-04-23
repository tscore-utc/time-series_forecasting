my.plotStitch.fun <- function(listMAPE, listMASE){
  
  # Make empty list for end result
  stitched_plots <- list()
  
  # Loop through each list to grab corresponding plots
  for (i in 1:length(listMAPE)) {
    
    # Extract method plots
    plotMAPE <- listMAPE[[i]]
    plotMASE <- listMASE[[i]]
    
    # Remove x-axis labels and tick marks from the upper plot
    plotMAPE <- plotMAPE + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + xlab("")
    
    # Extract y-axis label of the top plot and set it as the plot title
    y_label <- plotMAPE$labels$y
    plotMAPE <- plotMAPE + labs(title = y_label)
    
    # Change y-axis labels
    plotMAPE <- plotMAPE + ylab("MAPE")
    plotMASE <- plotMASE + ylab("MASE")
    
    # Remove title from the lower plot
    plotMASE <- plotMASE + labs(title = "")
    
    # Extract legend from the second plot
    legendMASE <- ggplot_gtable(ggplot_build(listMASE[[i]]))$grobs[[which(grepl("guide-box", ggplot_gtable(ggplot_build(listMASE[[i]]))$layout$name))]]
    
    # Remove legend from the plots
    plotMAPE <- plotMAPE + theme(legend.position = "none")
    plotMASE <- plotMASE + theme(legend.position = "none")
    
    # Stitch the plots together with shared legend
    stitched_plot <- grid.arrange(plotMAPE, plotMASE, ncol = 1, right = legendMASE)
    
    stitched_plots[[i]] <- stitched_plot
    
    # Save the stitched plot as an image
    output_filename <- file.path("/Users/ashleymorgan/Documents/previous research/forecasting project/major revision/plots/stitchPost", paste0("stitch_plot_", paste(y_label), ".png"))
    ggsave(output_filename, stitched_plot, width = 10)
    
  }
  
}
