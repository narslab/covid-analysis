library(BGVAR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(latticeExtra)

model_opt <- readRDS("../../models/model7_9.RDS")
countries <- c("DE", "KR", "US", "ZA")
brew_colors <- brewer.pal(6, "RdYlBu")
#brew_colors <- colorRampPalette(c("red","blue"))

#brew_colors <- brewer.pal(12, "Set3")
# load week 1 flights
flights_week1 <- readRDS("../../results/flights_week1.RDS")

# Define the low and high hues of the color scale
low_hue <- 240  # blue
high_hue <- 0   # red

# Define the chroma and lightness of the colors
chroma <- 50
lightness <- seq(75, 35, length.out = 100)

# Create a custom blue-to-red color palette in the hcl color space
my_palette <- hcl(seq(low_hue, high_hue, length.out = 100), chroma, lightness)

# Display the color palette
my_palette

plot_model_pred <- function(model, country, save_file = FALSE) {
  c <- model$args$Data[[country]]
  df <- as.data.frame(c)
  df$day <- 1:nrow(df)
  vals <- c("cases" = "red",
            "residential" = "blue", 
            "workplaces" = "green", 
            "transit" = "purple", 
            "grocery" = "orange")
  data_long <- gather(df, variable, value, -day)
  file_name <- paste0("../../figures/model_predictions_",country,".png")
  axis_labels <- function(x) {
    if (x == "cases") {
      return("Change in daily COVID-19 cases")
    } else {
      return("Percent change from baseline")
    }
  }
  p <- ggplot(data_long, aes(x = day, y = value, color = variable)) +
    geom_line() +
    ggtitle(country) +
    labs(x = "Day", y = "") +
    scale_color_manual(values = vals) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~ variable, 
               nrow = 5,
               scales = "free_y") 
  if(save_file){
    ggsave(file_name, p, width = 8, height = 10, device = "png") 
  }
  plot(p)
}

for(c in countries) {
  plot_model_pred(model_opt, c, save_file = FALSE)  
}

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



