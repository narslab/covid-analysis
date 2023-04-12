library(BGVAR)

# Load the data
bwList <- readRDS("../../data/tidy/bwList.RDS")
endoList <- readRDS("../../data/tidy/endoList.RDS")
exoList <- readRDS("../../data/tidy/exoList.RDS")
var.list <- readRDS("../../data/tidy/var_list.RDS")

# Set up the parameters for the four models
params <- list(
  list(p = 6, q = 4),
  list(p = 7, q = 1),
  list(p = 5, q = 7),
  list(p = 4, q = 8)
)

## Loop over the parameters and estimate and save each model
for (i in seq_along(params)) {
  # Extract the lag parameters for this iteration
  p <- params[[i]]$p
  q <- params[[i]]$q
  
  # Estimate the model with the specified lag parameters
  model <- bgvar(Data = endoList, #endogenous variables
                   Ex = exoList, # exogenous variables
                   W = bwList,
                   plag = c(p, q),
                   draws=100, burnin=100, prior="SSVS", SV=TRUE, 
                   hold.out = 30,
                   eigen = 1,
                   expert = list(cores=4,
                                 variable.list = var.list) #specifies which variable is weakly exogenous
  )
  
  # Save the model as an RDS file
  saveRDS(model, file = paste0("../../models/testing/model", p, "_", q, ".RDS"))
}



