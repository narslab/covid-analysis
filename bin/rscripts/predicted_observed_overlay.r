library(BGVAR)
library(reshape2)
library(ggplot2)
library(data.table)
library(ggallin)


# load optimal model and forecast
model_opt <- readRDS("../../models/model7_9.RDS")
observed7.9 <- model_opt$args$Data
fcast <- readRDS("../../models/predictions/model7_9_forecast_n30.RDS")
forecast7.9 <- fcast$fcast


# Choose all cases
cases_index = seq(1,260,5)

# Choose all transit
transit_index = seq(4,260,5)

# first dim
names(forecast7.9[1:5,1,1])
# "AE.cases"       "AE.residential" "AE.workplaces"  "AE.transit"     "AE.grocery"   

# quantiles
names(forecast7.9[1,1,])
# "Q5"  "Q10" "Q16" "Q50" "Q84" "Q90" "Q95"

cases_forecast <- forecast7.9[cases_index,,]
transit_forecast <- forecast7.9[transit_index,,]


cases_forecast_q5  = melt(cases_forecast[,,1])
cases_forecast_q10 = melt(cases_forecast[,,2])
cases_forecast_q16 = melt(cases_forecast[,,3])
cases_forecast_q50 = melt(cases_forecast[,,4])
cases_forecast_q84 = melt(cases_forecast[,,5])
cases_forecast_q90 = melt(cases_forecast[,,6])
cases_forecast_q95 = melt(cases_forecast[,,7])

transit_forecast_q5  = melt(transit_forecast[,,1])
transit_forecast_q10 = melt(transit_forecast[,,2])
transit_forecast_q16 = melt(transit_forecast[,,3])
transit_forecast_q50 = melt(transit_forecast[,,4])
transit_forecast_q84 = melt(transit_forecast[,,5])
transit_forecast_q90 = melt(transit_forecast[,,6])
transit_forecast_q95 = melt(transit_forecast[,,7])

melted_cases_forecast  = cbind(cases_forecast_q5, 
                               cases_forecast_q10$value, 
                               cases_forecast_q16$value,
                               cases_forecast_q50$value,
                               cases_forecast_q84$value, 
                               cases_forecast_q90$value,
                               cases_forecast_q95$value)

melted_transit_forecast = cbind(transit_forecast_q5, 
                                transit_forecast_q10$value, 
                                transit_forecast_q16$value,
                                transit_forecast_q50$value, 
                                transit_forecast_q84$value, 
                                transit_forecast_q90$value,
                                transit_forecast_q95$value)
colnames(melted_cases_forecast) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )
colnames(melted_transit_forecast) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )

melted_cases_forecast$Country <- gsub(".cases", "", as.character(melted_cases_forecast$Country))
melted_transit_forecast$Country <- gsub(".transit", "", as.character(melted_transit_forecast$Country))




# Get a list of all country names
countries <- names(model_opt$args$Data)
# Use lapply() to extract cases data for the last 30 days for each country
cases_last_30_days <- lapply(model_opt$args$Data[countries], function(x) x[170:199, 1])
# Combine the results into a matrix
cases_last_30_days_matrix <- do.call(cbind, cases_last_30_days)
cases_last_30_days_df <- as.data.frame(cases_last_30_days_matrix)
cases_last_30_days_df$Day <- 1:nrow(cases_last_30_days_df)

melted_cases_last_30_days_df <- melt(cases_last_30_days_df, id.vars = "Day")
colnames(melted_cases_last_30_days_df)[colnames(melted_cases_last_30_days_df) == "variable"] <- "Country"



# Merge melted_pred and melted_obs by Country and Day
merged_data <- merge(melted_cases_last_30_days_df, melted_cases_forecast, by = c("Country", "Day"))
# Rename the 'value' column from melted_obs to 'Observed'
names(merged_data)[names(merged_data) == "value"] <- "Observed"



cases_forecast_facet <- ggplot(merged_data, aes(Day)) + 
  geom_line(aes(y=Q50), colour="blue") + 
  geom_line(aes(y=Observed), colour="green") + 
  scale_y_continuous(trans = pseudolog10_trans) +
  ggtitle("Forecast - Cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red') +
  facet_wrap(vars(Country), nrow = 4, ncol = 13)

ggsave(filename = "../../figures/observed_predicted_overlay.png",plot = cases_forecast_facet,width = 16,height=10,device = "png")

transit_forecast_facet <- ggplot(melted_transit_forecast, aes(Day)) + 
  geom_line(aes(y=Q50), colour="blue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Forecast - Transit") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red') 

transit_forecast_facet + facet_wrap(vars(Country), nrow = 4, ncol = 13)


