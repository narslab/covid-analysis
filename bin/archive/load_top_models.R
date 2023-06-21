library(BGVAR)

#model4_8 <- readRDS("../../models/no_workplaces/model4_8.RDS")
model7_9 <- readRDS("../../models/no_workplaces/model7_9.RDS")

#fcast4_8 <- readRDS("../../models/no_workplaces/4_8_forecast_n30.RDS")
fcast7_9 <- readRDS("../../models/no_workplaces/7_9_forecast_n30.RDS")

mod_pred <- list(#list(model4_8, fcast4_8),
                 list(model7_9, fcast7_9))

