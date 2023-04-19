# set working directory (setwd) to ../covid-analysis/bin/rscripts
# load libraries
library(BGVAR) 
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)


# load forecast with lag params p=7, q=9 based on optimal model
#fcast7.9 <- readRDS("../../models/predictions/model7_9_forecast_n30.RDS")


generate_rmse_plot <- function(bgvar_forecast, pq_lag='p_q_lag', save = FALSE) {
  rmse <- rmse(bgvar_forecast)
  
  rmse.df <- as.data.frame(rmse[30, 1:260])
  rmse.convert.index <- rownames_to_column(rmse.df, var="index")
  
  # separate the country and category from the index
  rmse.convert.index$country <- sub("\\..*", "", rmse.convert.index$index)
  rmse.convert.index$category <- sub(".*\\.", "", rmse.convert.index$index)
  
  # pivot the data so that the index becomes separate columns
  rmse.wide <- pivot_wider(rmse.convert.index, names_from = category, values_from = `rmse[30, 1:260]`)
  
  # select and rename the columns
  rmse.final <- rmse.wide %>% select(country, cases, grocery, residential, transit, workplaces)
  names(rmse.final)[2:6] <- c("cases", "grocery", "residential", "transit", "workplaces")
  
  # consolidate rows into a single row
  rmse.final <- rmse.final %>% 
    group_by(country) %>% 
    summarize_all(funs(max(., na.rm = TRUE))) %>% 
    ungroup()
  
  # Calculate the mean RMSE value of each endogenous variable
  avg_values <- sapply(rmse.final[,2:6], mean)
  # Print the results
  print(avg_values)
  
  write.csv(rmse.final, file="../../results/rmse.csv")
  
  # convert the data frame from wide to long format, 
  df_long <- gather(rmse.final, variable, value, -country)
  
  # create a labeller function that returns custom labels
  my_labeller <- function(variable, value) {
    if (variable == "variable") {  
      if (value == "cases") {
        return("Daily COVID counts")
      } else {
        return("Change from 100% baseline")
      }
    } else {
      return("test")
    }
  }
  
  if(pq_lag == "p_q_lag") {
    parsed_title <- ""  
  } else {
    # Split the string into two parts using the underscore as the delimiter
    parts <- strsplit(pq_lag, "_")[[1]]
    
    # Extract the values for p_lag and q_lag from the parts
    p_lag <- parts[2]
    q_lag <- parts[3]
    
    # Create the output string
    parsed_title <- paste0("p_lag = ", p_lag, ", q_lag = ", q_lag)
  }
  
  
  # create a bar plot with country on the x-axis and value on the y-axis
  rmse_facet <- ggplot(df_long, aes(x = country, y = value)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ variable, nrow = 5, scales = "free_y") +
    labs(color = "", fill = "", title = parsed_title) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          plot.title = element_text(hjust = 0.5))
  
  plot(rmse_facet)
  
  
  if(save) {
    save_as <- paste0("../../figures/rmse_facet",parsed_title,".png")
    ggsave(filename = save_as,
           plot = rmse_facet,
           width = 10, height = 10,
           device = "png")  
  }
  
}

#generate_rmse_plot(fcast7.9)
