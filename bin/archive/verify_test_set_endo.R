library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape the data to long format
df_long <- df_endo %>%
  pivot_longer(cols = -c(iso, variable),
               names_to = "date",
               values_to = "cases") %>%
  mutate(date = as.Date(gsub("\\.", "-", date), format = "%m-%d-%Y"))

# Filter the dataframe to include the last 30 days
df_filtered <- df_long %>%
  filter(date >= as.Date("2020-08-01"),
         date <= as.Date("2020-08-31"))

# Plot the cases using ggplot
ggplot(df_filtered, aes(x = date, y = cases, color = iso)) +
  geom_line() +
  labs(title = "COVID-19 cases in the test set",
       x = "Date",
       y = "Cases") +
  scale_x_date(date_labels = "%m-%d-%Y", date_breaks = "1 week") +
  theme_bw() +
  theme(legend.position = "bottom")


# Define the start and end dates
start_date <- as.Date("2020-02-15")
end_date <- as.Date("2020-08-31")

# Calculate the difference in days
num_days <- difftime(end_date, start_date, units = "days")

# Print the result
print(num_days)
