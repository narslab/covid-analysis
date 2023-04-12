library(BGVAR)

# Load the models
model6_4 <- readRDS("../../models/testing/model6_4.RDS")
model7_1 <- readRDS("../../models/testing/model7_1.RDS")

# Generate predictions
fcast6_4 <- predict(model6_4, n.ahead=30)
fcast7_1 <- predict(model7_1, n.ahead=30)

# Save model predictions
saveRDS(fcast6_4, "../../models/predictions/model6_4_forecast_n30.RDS")
saveRDS(fcast7_1, "../../models/predictions/model7_1_forecast_n30.RDS")
