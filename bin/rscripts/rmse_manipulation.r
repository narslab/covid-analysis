# set working directory (setwd) to ../covid-analysis/bin/rscripts
# load libraries
library(BGVAR) 
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)
library(scales)

# load forecast with lag params p=7, q=9 based on optimal model
fcast7.9 <- readRDS("../../models/predictions/model7_9_forecast_n30.RDS")
rmse <- rmse(fcast7.9)

log_likelihood_df <- read.csv("../../results/3d_scatterplot_input.csv")

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

write.csv(rmse.final, file="../../results/rmse.csv")

# filter the data for the specified countries
df_countries <- rmse.final[rmse.final$country %in% c("US", "ZA", "KR", "DE"), ]


# convert the data frame from wide to long format, 
df_long <- gather(rmse.final, variable, value, -country)

# create a labeller function that returns custom labels
my_labeller <- function(variable, value) {
  if (variable == "variable") {  # use '==' instead of '='
    if (value == "cases") {
      return("Daily COVID counts")
    } else {
      return("Change from 100% baseline")
    }
  } else {
    return("test")
  }
}

# create a bar plot with country on the x-axis and value on the y-axis
ggplot(df_long, aes(x = country, y = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, nrow = 5, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# treat p and q lags as discrete variables
log_likelihood_df$p_lag <- factor(log_likelihood_df$p_lag)
log_likelihood_df$q_lag <- factor(log_likelihood_df$q_lag)

log_likelihood_df_filtered <- log_likelihood_df %>%
  filter(log_likelihood < 1e+19)

ggplot(log_likelihood_df,aes(x = p_lag, y = log_likelihood + abs(min(log_likelihood)) + 1,
                            color = q_lag)) + 
  theme_bw() +
  geom_point(size = 2, alpha =  0.5) +
  geom_line(aes(group = q_lag),size = 1.5) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs( x = "p lag", 
        y = "Log likelihood", color = "q lag") +
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.key.size = unit(2, 'cm'),
        legend.text = element_text(size = 25))

