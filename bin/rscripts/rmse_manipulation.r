# set working directory (setwd) to ../covid-analysis/bin/rscripts
# load libraries
library(BGVAR) 
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)

# load forecast with lag params p=7, q=9 based on optimal model
fcast7.9 <- readRDS("../../models/predictions/model7_9_forecast_n30.RDS")
rmse <- rmse(fcast7.9)

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

# melt the dataframe
rmse_df_melted <- reshape2::melt(df_countries, id.vars = "country", variable.name = "category")

# create the facet plot
ggplot(rmse_df_melted, aes(x = country, y = value)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") + 
  facet_wrap(~category, scales = "free_y") +
  labs(x = "Country", y = "RMSE") +
  theme_bw()
