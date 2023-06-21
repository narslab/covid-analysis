# This script generates the impulse response function to specific shocks.  
# The shock_origin list can be expanded to included additional variables.
# The existing country/shocks have been selected to represent members of each
# cluster identified through dynamic time warping (dtw).

library(BGVAR)

model7_9 <- readRDS("../../models/model7_9.RDS")
shock_origin <- c("US.cases", "US.transit", 
                  "ZA.cases", "ZA.transit", 
                  "DE.cases", "DE.transit", 
                  "KR.cases", "KR.transit", 
                  "IN.cases", "IN.transit")

generate_irf <- function(model, shocks) {
  shockinfo_girf <- get_shockinfo("girf", nr_rows = 10)
  shockinfo_girf$shock <- shocks
  shockinfo_girf$scale <- 1
  
  irf <- irf(model, n.ahead=30, shockinfo=shockinfo_girf, 
             expert=list(save.store=TRUE,cores=16))
  plag <- model$args$plag
  file_name <- paste0("../../models/irf/irf", plag[1], "_", plag[2], ".RDS")
  saveRDS(irf, file = file_name)
}

generate_irf(model7_9, shock_origin)
