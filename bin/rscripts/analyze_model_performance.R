source("model_forecast_overlay.r")
library(BGVAR)

model4_8 <- readRDS("../../models/testing/model4_8.RDS")
model5_7 <- readRDS("../../models/testing/model5_7.RDS")
model6_4 <- readRDS("../../models/testing/model6_4.RDS")
model7_1 <- readRDS("../../models/testing/model7_1.RDS")
model7_9 <- readRDS("../../models/testing/model7_9.RDS")

fcast4_8 <- readRDS("../../models/testing/predictions/4_8_forecast_n30.RDS")
fcast5_7 <- readRDS("../../models/testing/predictions/5_7_forecast_n30.RDS")
fcast6_4 <- readRDS("../../models/testing/predictions/6_4_forecast_n30.RDS")
fcast7_1 <- readRDS("../../models/testing/predictions/7_1_forecast_n30.RDS")
fcast7_9 <- readRDS("../../models/testing/predictions/7_9_forecast_n30.RDS")

mod_pred <- list(list(model4_8, fcast4_8),
              list(model5_7, fcast5_7),
              list(model6_4, fcast6_4),
              list(model7_1, fcast7_1),
              list(model7_9, fcast7_9))

# Generate overlay for each potential candidate for optimal model
for (i in 1:length(mod_pred)) {
  model    <- mod_pred[[i]][[1]]
  forecast <- mod_pred[[i]][[2]]
  print(paste0("p lag: ", model$args$plag[[1]], ", q lag: ", model$args$plag[[2]]))
  for(v in endo_vars) {
    generate_overlay(model, forecast, v, TRUE)
  }
}


