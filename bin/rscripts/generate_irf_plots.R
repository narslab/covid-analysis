# This script generates the irf plots for a shock originating in a country
# representative of each cluster identified through dtw over the test set horizon
# (30 days). The shocks are specified in generate_irf.R; please confirm shock
# info for a particular country of interest has been generated before sourcing
# this file. 

library(ggplot2)
library(data.table)
library(tidyverse)
library(BGVAR)

irf = readRDS('../../models/irf/irf7_9.RDS')

# Shocks in cases and transit originating in each of the following countries:
# DE, IN, KR, US, ZA
kr_cases <- c("KR", 'cases')
kr_transit <- c("KR", "transit")

za_cases <- c("ZA", 'cases')
za_transit <- c("ZA", "transit")

us_cases <- c("US", "cases")
us_transit <- c("US", "transit")

in_cases <- c("IN", "cases")
in_transit <- c("IN", "transit")

de_cases <- c("DE", "cases")
de_transit <- c("DE", "transit")


generate_irf_plots <- function(irf_object, origin_type=c("ZA", "cases"), all_countries = FALSE, save_file = FALSE){
  irf_post <- irf_object$posterior
  cases_index = seq(1,260,5)
  transit_index = seq(4,260,5)
  
  if(identical(origin_type, c("US", "cases"))) {
    shock_irf <- irf_post[cases_index, , 1, ]
    relevant_subset <- c("BR", "FR","IN")
  } else if(identical(origin_type, c("US", "transit"))) {
    shock_irf <- irf_post[transit_index, , 2, ]
    relevant_subset <- c("US")
  } else if(identical(origin_type, c("ZA", "cases"))) {
    shock_irf <- irf_post[cases_index, , 3, ]
    relevant_subset <- c("AE", "BR", "US")
  } else if(identical(origin_type, c("ZA", "transit"))) {
    shock_irf <- irf_post[transit_index, , 4, ]
    relevant_subset <- c("ZA")
  } else if(identical(origin_type, c("DE", "cases"))) {
    shock_irf <- irf_post[cases_index, , 5, ]
    relevant_subset <- c("ES", "FR", "US")
  } else if(identical(origin_type, c("DE", "transit"))) {
    shock_irf <- irf_post[transit_index, , 6, ]
    relevant_subset <- c("DE")
  } else if(identical(origin_type, c("KR", "cases"))) {
    shock_irf <- irf_post[cases_index, , 7, ]
    relevant_subset <- c("PH", "US")
  } else if(identical(origin_type, c("KR", "transit"))) {
    shock_irf <- irf_post[transit_index, , 8, ]
    relevant_subset <-c("KR")
  } else if(identical(origin_type, c("IN", "cases"))) {
    shock_irf <- irf_post[cases_index, , 9, ]
    relevant_subset <- c("US")
  } else if(identical(origin_type, c("IN", "transit"))) {
    shock_irf <- irf_post[transit_index, , 10, ]
    relevant_subset <-c("IN")
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
  
  if(all_countries) {
    irf_final_df <- melted_shock_irf
    all_tag <- "_all"
  } else {
    irf_final_df <- subset(melted_shock_irf, Country %in% relevant_subset ) #response countries only
  }
  
  irf_plot <- ggplot(irf_final_df, aes(Day)) + 
    geom_line(aes(y=Q50), colour="blue") + 
    geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red') +
    facet_wrap(vars(Country)) +
    ylab("Impulse Response") +
    theme(axis.text.x = element_text(size = 32),
          axis.text.y = element_text(size = 32),
          axis.title.x = element_text(size = 36),
          axis.title.y = element_text(size = 36),
          strip.text = element_text(size = 32))
  
  if(save_file) {
    origin_type[1] <- tolower(origin_type[1])
    plot_title <- paste(origin_type, collapse = "_")
    if(all_countries){
      fname <- paste0("../../figures/irf_origin_", plot_title, all_tag,".png")
      w_h <- c(24, 16)
    } else {
      fname <- paste0("../../figures/irf_origin_", plot_title,".png")  
      w_h <- c(20, 15)
    }
    ggsave(filename = fname, plot = irf_plot, width = w_h[1], height = w_h[2], device = "png", dpi = "retina")
  }
  
  plot(irf_plot)
}

generate_irf_plots(irf, de_cases, save_file = TRUE)
generate_irf_plots(irf, za_cases, save_file = TRUE) 
generate_irf_plots(irf, in_cases, save_file = TRUE) 
generate_irf_plots(irf, us_cases, save_file = TRUE) 
generate_irf_plots(irf, kr_cases, save_file = TRUE) 
  