library(ggplot2)
library(data.table)
library(tidyverse)
library(BGVAR)

irf = readRDS('../../results/global_irf_cases.RDS')
us_cases <- c("US", "cases")
us_transit <- c("US", "transit")

kr_cases <- c("KR", "cases")
de_cases <- c("DE", "cases")

generate_irf_plots <- function(irf_object, origin_type=c("ZA", "cases")){
  irf_post <- irf_object$posterior
  cases_index = seq(1,260,5)
  transit_index = seq(4,260,5)
  
  if(identical(origin_type, c("ZA", "cases"))) {
    shock_irf <- irf_post[cases_index, , 1, ]
    relevant_subset <- c("AE", "BR", "US")
  } else if(identical(origin_type, c("ZA", "transit"))) {
    shock_irf <- irf_post[transit_index, , 2, ]
    relevant_subset <- c("ZA")
  } else if(identical(origin_type, c("US", "cases"))) {
    shock_irf <- irf_post[cases_index, , 3, ]
    relevant_subset <- c("BR", "FR", "IN")
  } else if(identical(origin_type, c("US", "transit"))) {
    shock_irf <- irf_post[transit_index, , 4, ]
    relevant_subset <- c("US")
  } else if(identical(origin_type, c("KR", "cases"))) {
    shock_irf <- irf_post[cases_index, , 5, ]
    relevant_subset <- c("PH", "US")
  } else if(identical(origin_type, c("KR", "transit"))) {
    shock_irf <- irf_post[transit_index, , 6, ]
    relevant_subset <- c("KR")
  } else if(identical(origin_type, c("DE", "cases"))) {
    shock_irf <- irf_post[cases_index, , 7, ]
    relevant_subset <- c("ES", "FR", "US")
  } else if(identical(origin_type, c("DE", "transit"))) {
    shock_irf <- irf_post[transit_index, , 8, ]
    relevant_subset <-c("DE")
  } else {
    print("Origin type not recognized. Using ZA.cases instead.")
    shock_irf <- irf_post[cases_index, , 1, ]
    relevant_subset <- c("AE", "BR", "US")
  }
  
  shock_irf_q5  = reshape2::melt(shock_irf[,,1])
  shock_irf_q10 = reshape2::melt(shock_irf[,,2])
  shock_irf_q16 = reshape2::melt(shock_irf[,,3])
  shock_irf_q50 = reshape2::melt(shock_irf[,,4])
  shock_irf_q84 = reshape2::melt(shock_irf[,,5])
  shock_irf_q90 = reshape2::melt(shock_irf[,,6])
  shock_irf_q95 = reshape2::melt(shock_irf[,,7])
  
  melted_shock_irf = cbind(shock_irf_q5, 
                              shock_irf_q10$value, 
                              shock_irf_q16$value,
                              shock_irf_q50$value,
                              shock_irf_q84$value, 
                              shock_irf_q90$value,
                              shock_irf_q95$value)
  colnames(melted_shock_irf) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )
  if(origin_type[2] == "cases"){
    melted_shock_irf$Country <- gsub(".cases", "", as.character(melted_shock_irf$Country))  
  } else {
    melted_shock_irf$Country <- gsub(".transit", "", as.character(melted_shock_irf$Country))  
  }
  
  response_countries <- subset(melted_shock_irf, Country %in% relevant_subset )
  
  irf_plot <- ggplot(response_countries, aes(Day)) + 
    geom_line(aes(y=Q50), colour="blue") + 
    geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red')
  
  irf_plot + facet_wrap(vars(Country))
}

generate_irf_plots(irf)
