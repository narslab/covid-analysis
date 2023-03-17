library(BGVAR)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

model_opt <- readRDS("../../models/model7_9.RDS")
countries <- c("DE", "KR", "US", "ZA")
brew_colors <- brewer.pal(12, "Set3")
# load week 1 flights
flights_week1 <- readRDS("../../results/flights_week1.RDS")


plot_model_pred <- function(model, country) {
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
  ggsave(file_name, p, width = 8, height = 10, device = "png")
}

for(c in countries) {
  plot_model_pred(model_opt, c)  
}

# flights week 1 heatmap
mtx_melt <- reshape2::melt(flights_week1)
colnames(mtx_melt)[1] <- "destination_origin"

flights.heatmap <- ggplot(mtx_melt, aes(destination_origin, variable, fill= value)) + 
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = brew_colors, na.value = "white", name = "Number of flights") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.height = unit(1.5, "cm"), # adjust height of legend key
        legend.key.width = unit(0.5, "cm"), # adjust width of legend key
        legend.key.size = unit(0.5, "cm"), # adjust overall size of legend key
        legend.text.align = 0.5) # center legend text) +
  labs(title = "Origin-Destination Heatmap of Week 1 Flights", 
       x = "Origin", 
       y = "Destination")
ggsave("../../figures/week1_flights_heatmap.png", flights.heatmap, width = 14, height = 14, device = "png")



