library(foreach)
library(doParallel)
library(BGVAR)

# Load the data
bwList <- readRDS("../../data/tidy/bwList.RDS")
endoList <- readRDS("../../data/tidy/endoList.RDS")
exoList <- readRDS("../../data/tidy/exoList.RDS")
var.list <- readRDS("../../data/tidy/var_list.RDS")

params <- list(list(p=4, q=8), list(p=5, q=7), list(p=7, q=9))

# Set up a parallel backend with the desired number of cores
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)


# Define the function to estimate the model
estModel <- function(p, q, endogenous, exogenous, weights, variables) {
  model <- bgvar(Data = endogenous, 
                 Ex = exogenous,
                 W = weights,
                 plag = c(p, q),
                 draws=100, burnin=100, prior="SSVS", SV=TRUE, 
                 hold.out = 30,
                 eigen = 1,
                 expert = list(cores=4,
                               variable.list = variables) #specifies which variable is weakly exogenous
  )
  return(model)
}

# Initialize the progress bar
pb <- txtProgressBar(min = 0, max = length(params), style = 3)

# Run the loop in parallel using foreach
results <- foreach(i = 1:length(params), .combine = "list", .packages = "BGVAR") %dopar% {
  p_lag <- params[[i]]$p
  q_lag <- params[[i]]$q
  
  # Update the progress bar with the current iteration number
  setTxtProgressBar(pb, i)
  estModel(p_lag, q_lag, endoList, exoList, bwList, var.list)
}


# Save the models
for (i in seq_along(results)) {
  p_lag <- params[[i]]$p
  q_lag <- params[[i]]$q
  filename <- paste0("../../models/testing/model", p_lag, "_", q_lag, ".RDS")
  
  print(paste("Saving model with parameters p=", p_lag, " and q=", q_lag, " to ", filename, sep=""))
  saveRDS(results[[i]], file = filename)
}

# Stop the parallel backend
stopCluster(cl)