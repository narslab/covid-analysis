library(BGVAR)
library(ggplot2)
library(RColorBrewer)

# load week 1 flights
flights_week1 <- readRDS("../../results/flights_week1.RDS")

brew_colors <- brewer.pal(6, "RdYlBu")

# flights week 1 heatmap
plot_flights_week1_heatmap <- function(flights_mtx, normalized = FALSE, save_plot = FALSE) {
  mtx_melt <- reshape2::melt(flights_mtx)
  colnames(mtx_melt)[1] <- "destination_origin"
  if(normalized) {
    # Convert "value" column to numeric
    mtx_melt$value <- as.numeric(as.character(mtx_melt$value))
    # Remove NA values
    mtx_melt <- na.omit(mtx_melt)
    # Normalize "value" column
    mtx_melt$value <- scale(mtx_melt$value)
    # Verify that the values are now normalized
    print(head(mtx_melt))
  }
  flights.heatmap <-
    ggplot(mtx_melt, aes(destination_origin, variable, fill= value)) + 
    geom_tile(color = "white") +
    scale_fill_gradientn(colors = brew_colors, na.value = "white", name = "Number of flights") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.key.height = unit(1, "cm"), # adjust height of legend key
          legend.key.width = unit(1, "cm"), # adjust width of legend key
          legend.key.size = unit(0.5, "cm"), # adjust overall size of legend key
          legend.text.align = 0.5,
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 18), # increase font size of axis titles
          axis.text = element_text(size = 12), # increase font size of axis labels
          legend.title = element_text(size = 14), # increase font size of legend title
          legend.text = element_text(size = 12), # increase font size of legend labels
          legend.position = "bottom",
          text = element_text(size = 20), # increase overall font size
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    labs(#title = "Origin-Destination Heatmap of Week 1 Flights", 
      x = "Origin", 
      y = "Destination")
  
  if(save_plot){
    ggsave("../../figures/week1_flights_heatmap.png", flights.heatmap, width = 20, height = 15, device = "png", dpi = "retina") 
  }
  
  plot(flights.heatmap)
}

plot_flights_week1_heatmap(flights_week1, save_plot = TRUE)