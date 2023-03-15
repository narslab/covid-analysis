library(BGVAR)
library(tidyr)
library(dplyr)
library(ggplot2)

model_opt <- readRDS("../../models/model7_9.RDS")
countries <- c("DE", "KR", "US", "ZA")

plot_model_pred <- function(model, country, cases=FALSE) {
  c <- model$args$Data[[country]]
  df <- as.data.frame(c)
  df$day <- 1:nrow(df)
  if (cases == TRUE) { 
    df_filtered <- df %>% select(cases, day)
    y_val <- "Daily COVID-19 counts"
    vals <- c("cases" = "red")    
  } else {
    df_filtered <- df %>% select(-cases)
    y_val <- "Percent change from baseline"
    vals <- c("residential" = "blue", 
              "workplaces" = "green", "transit" = "purple", 
              "grocery" = "orange")
  }
  data_long <- gather(df_filtered, variable, value, -day)
  file_name <- paste("../../figures/model_predictions_",country,"_",y_val,".png")
  my_plot <- ggplot(data_long, aes(x = day, y = value, color = variable)) +
    geom_line() +
    ggtitle(country) +
    labs(x = "Day", y = y_val) +
    scale_color_manual(values = vals) +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(file_name, my_plot, device = "png")
}

for(c in countries) {
  plot_model_pred(model_opt, c)  
  plot_model_pred(model_opt, c, TRUE)  
}
