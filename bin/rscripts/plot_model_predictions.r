library(BGVAR)
library(tidyr)
library(dplyr)
library(ggplot2)

model_opt <- readRDS("../../models/model7_9.RDS")
countries <- c("DE", "KR", "US", "ZA")

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
  file_name <- paste("../../figures/model_predictions_",country,".png")
  axis_labels <- function(x) {
    if (x == "cases") {
      return("Change in daily COVID-19 cases")
    } else {
      return("Percent change from baseline")
    }
  }
  my_plot <- ggplot(data_long, aes(x = day, y = value, color = variable)) +
    geom_line() +
    ggtitle(country) +
    labs(x = "Day", y = "") +
    scale_color_manual(values = vals) +
    theme(plot.title = element_text(hjust = 0.5)) +
    facet_wrap(~ variable, 
               nrow = 5,
               scales = "free_y") 
  ggsave(file_name, my_plot, width = 8, height = 10, device = "png")
}



for(c in countries) {
  plot_model_pred(model_opt, c)  
}



