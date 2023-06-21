source("load_top_models.R")
source("model_forecast_overlay_no_work.R")
source("rmse_manipulation_no_work.R")

# Generate forecast overlay and RMSE plot for each potential candidate for optimal model
for (i in 1:length(mod_pred)) {
  model    <- mod_pred[[i]][[1]]
  forecast <- mod_pred[[i]][[2]]
  rmse_lag <- paste0("_",model$args$plag[[1]],"_",model$args$plag[[2]])
  print(paste0("p lag: ", model$args$plag[[1]], ", q lag: ", model$args$plag[[2]]))
  generate_rmse_plot(forecast, rmse_lag)
  for(v in endo_vars) {
    generate_overlay_no_work(model, forecast, v)
  }
}
