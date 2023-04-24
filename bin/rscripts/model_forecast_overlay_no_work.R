library(BGVAR)
library(reshape2)
library(ggplot2)
library(ggallin)

model7_9 <- readRDS("../../models/no_workplaces/model7_9.RDS")
fcast7_9 <- readRDS("../../models/no_workplaces/7_9_forecast_n30.RDS")

endo_vars <- c("cases", "residential", "transit", "grocery")

generate_overlay_no_work <- function(bgvar_model, bgvar_forecast, var, save=FALSE) {
  obs  <- bgvar_model$args$Data
  pred <- bgvar_forecast$fcast
  
  if(var == "cases") {
    index <- seq(1, 208, 4)
  } else if (var == "residential") {
    index <- seq(2, 208, 4)
  } else if (var == "transit") {
    index <- seq(3, 208, 4)
  } else if (var == "grocery") {
    index <- seq(4, 208, 4)
  } else {
    print("Endo variable not recognized...using 'cases' instead.")
    index <- seq(1, 208, 4)
  }
  
  forecast <- pred[index,,]
  
  forecast_q5  = reshape2::melt(forecast[,,1])
  forecast_q10 = reshape2::melt(forecast[,,2])
  forecast_q16 = reshape2::melt(forecast[,,3])
  forecast_q50 = reshape2::melt(forecast[,,4])
  forecast_q84 = reshape2::melt(forecast[,,5])
  forecast_q90 = reshape2::melt(forecast[,,6])
  forecast_q95 = reshape2::melt(forecast[,,7])
  
  melted_forecast = cbind(forecast_q5, 
                          forecast_q10$value, 
                          forecast_q16$value,
                          forecast_q50$value,
                          forecast_q84$value, 
                          forecast_q90$value,
                          forecast_q95$value)
  colnames(melted_forecast) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )
  
  if(var == "cases") {
    melted_forecast$Country <- gsub(".cases", "", as.character(melted_forecast$Country))
  } else if (var == "residential") {
    melted_forecast$Country <- gsub(".residential", "", as.character(melted_forecast$Country))
  } else if (var == "transit") {
    melted_forecast$Country <- gsub(".transit", "", as.character(melted_forecast$Country))
  } else if (var == "grocery") {
    melted_forecast$Country <- gsub(".grocery", "", as.character(melted_forecast$Country))
  } 
  
  # Get a list of all country names
  countries <- names(obs)
  # Use lapply() to extract observed data for the last 30 days for each country
  if(var == "cases") {
    last_30_days <- lapply(obs[countries], function(x) x[170:199, 1])
  } else if (var == "residential") {
    last_30_days <- lapply(obs[countries], function(x) x[170:199, 2])
  } else if (var == "transit") {
    last_30_days <- lapply(obs[countries], function(x) x[170:199, 3])
  } else if (var == "grocery") {
    last_30_days <- lapply(obs[countries], function(x) x[170:199, 4])
  } 
  
  
  # Combine variable into a matrix
  #last_30_days_matrix <- do.call(cbind, last_30_days)
  last_30_days_df <- as.data.frame(last_30_days)
  last_30_days_df$Day <- 1:nrow(last_30_days_df)
  last_30_days_melted_df <- reshape2::melt(last_30_days_df, id.vars = "Day")
  
  colnames(last_30_days_melted_df)[colnames(last_30_days_melted_df) == "variable"] <- "Country"
  
  # Merge melted predictions and melted observations by Country and Day
  merged_data <- merge(last_30_days_melted_df, melted_forecast, by = c("Country", "Day"))
  names(merged_data)[names(merged_data) == "value"] <- "Observed"
  
  if(var == "cases") {
    y_axis_label <- "Daily COVID-19 counts"
  } else {
    y_axis_label <- "Percent change from baseline"
  }
  plot_title <- paste0(var, ": p_lag = ", bgvar_model$args$plag[[1]], ", q_lag = ", bgvar_model$args$plag[[2]])
  
  predicted_observed_overlay <- ggplot(merged_data, aes(Day)) + 
    geom_line(aes(y=Q50, color = "Median", linetype = "Median"), size = 0.5) + # changed linetype and size
    geom_line(aes(y=Observed, color = "Observed", linetype = "Observed"), size = 0.5, linetype = "dashed") + # changed color, linetype and size
    scale_y_continuous(trans = pseudolog10_trans, 
                       expand = expansion(mult = c(0, 0.003))) +
    geom_ribbon(aes(ymin=Q5, ymax=Q95, fill = "95% Credible Interval"), alpha=0.1) +
    facet_wrap(vars(Country), nrow = 4, ncol = 13) +
    ylab(y_axis_label) +
    labs(color = "", fill = "", title = plot_title) +
    scale_color_manual(values = c("Observed" = "blue", "Median" = "red", "95% Credible Interval" = "#FFB2B2")) + # modified colors
    theme(legend.margin = margin(l = 0, r = 0, t = 0, b = 0),
          legend.spacing = unit(0.05, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(linetype = FALSE)
  
  if(save) {
    fname <- paste0("../../figures/",var,"_observed_predicted_",bgvar_model$args$plag[[1]],"_",bgvar_model$args$plag[[2]],".png")
    print(fname)
    ggsave(filename = fname, plot = predicted_observed_overlay, width = 20, height=15, device = "png")  
  }
  
  plot(predicted_observed_overlay)
}

for(v in endo_vars) {
  generate_overlay_no_work(model7_9, fcast7_9, v)  
}
