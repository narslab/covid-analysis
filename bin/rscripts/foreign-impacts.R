library(ggplot2)
library(data.table)
library(BGVAR)
library(tidyverse)

#df <- read.csv('../../results/cases_lag.csv')
#names(df)[2:11] =  seq(0,9)
##row.names(df) = df$Country
##df <- subset(df, select = -Country)
#melted.df <- melt(as.data.table(df), id.vars = 'country')

#dev.new(width=5, height=10, unit="in")
#ggplot(melted.df, aes(variable, country, fill= value)) + 
#    geom_tile() +
#    scale_fill_distiller(palette = "Spectral",direction=1) 


estimate_girf <- function(impact="cases") {
  model_opt <- readRDS("../../models/model7_9.RDS")
  endo_cases <- readLines("../../data/tidy/endolist_cases.txt")
  endo_transit <- readLines("../../data/tidy/endolist_transit.txt")
  shockinfo_girf <- get_shockinfo("girf", nr_rows = 8)
  shockinfo_girf$shock <- c("ZA.cases", "ZA.transit", "US.cases", "US.transit", "KR.cases", "KR.transit", "DE.cases", "DE.transit")
  shockinfo_girf$scale <- 1
  
  irf.girf.cases.transit <- irf(model_opt, n.ahead=30, shockinfo=shockinfo_girf, expert=list(save.store=TRUE,cores=4))
  
  ifelse(impact == "cases", 
         plot(irf.girf.cases.transit, resp=endo_cases, shock="US.cases"),
         plot(irf.girf.cases.transit, resp=endo_transit, shock="US.transit"))
  return(irf.girf.cases.transit)
}

estimate_foreign_impact <- function(exog_var) {
  model_opt <- readRDS("../../models/model7_9.RDS")
  idx <- ifelse(exog_var=="cases", 1, 
                ifelse(exog_var=="residential", 2, 
                       ifelse(exog_var=="work", 3, 
                              ifelse(exog_var=="transit", 4, 5))))
  foreign_var <- c("cases*","cases*_lag1","cases*_lag2","cases*_lag3","cases*_lag4","cases*_lag5","cases*_lag6","cases*_lag7","cases*_lag8","cases*_lag9","sd")
  cntr <- c()
  coef <- c()
  for(i in 1:52)
  {
    cntr <- c(cntr,strsplit(colnames(model_opt$cc.results$coeffs[[i]])[1],split="\\.")[[1]][1])
    coef <- c(coef,model_opt$cc.results$coeffs[[i]][foreign_var,idx])
  }
  m <- cbind(cntr<-rep(cntr,each=length(foreign_var)),coef)
  m <- cbind(var_name=rownames(m),m)
  rownames(m) <- 1:nrow(m)
  colnames(m) <- c("foreign_variable","country","coefficient")
  m1_wider <- as.data.frame(m) %>% pivot_wider(names_from=foreign_variable,values_from = coefficient)
  m1_wider
}


#jpeg(file="../../figures/test_irf1.jpeg")#,width=256, height=256, res=720)
#plot(girf_obj, resp=readLines("../../data/tidy/endolist_transit.txt"), shock="KR.transit", ylim(min,max))
#dev.off()

cases <- estimate_foreign_impact("cases")
home <- estimate_foreign_impact("residential")
work <- estimate_foreign_impact("work")
transit <- estimate_foreign_impact("transit")
grocery <- estimate_foreign_impact("grocery")

# create a new type for each variable
cases$type <- "cases"
home$type <- "home"
work$type <- "work"
transit$type <- "transit"
grocery$type <- "grocery"

combined_data <- rbind(cases, home, work, grocery, transit)


plot_foreign_impact <- function(wide_matrix) {
  wide_matrix <- wide_matrix[1:11]
  names(wide_matrix)[2:11] =  c("cases",paste0("lag_",seq(1,9)))#seq(0,9)  
  melted.mtx <- melt(as.data.table(wide_matrix), id.vars = 'country')
  melted.mtx$value <- as.numeric(melted.mtx$value)
  ggplot(melted.mtx, aes(variable, country, fill= value)) + 
    geom_tile() +
    scale_fill_distiller(palette = "Spectral",direction=1)
  }

plot_social_distancing <- function(wide_matrix, v="cases") {
  wide_matrix <- wide_matrix[,c(1,12)]
  wide_matrix$sd <- as.double(wide_matrix$sd)
  plot_color <- ifelse(v == "cases", "darkblue", 
                       ifelse(v == "work", "darkred", 
                              ifelse(v == "grocery", "lightblue",
                                     ifelse(v == "transit", "pink", 
                                            "darkgreen"))))
  ggplot(wide_matrix, aes(sd, country)) + #reorder(country,sd)
    geom_bar(stat="identity", fill=plot_color) +
    xlab("Social distancing coefficient") +
    ylab("Country")
}


plot_social_distancing_new <- function(wide_matrix) {
  wide_matrix <- wide_matrix[, c(1, 12, 13)]
  wide_matrix$sd <- as.double(wide_matrix$sd)
  wide_matrix$type <- factor(wide_matrix$type, levels = c("cases", "work", "grocery", "transit", "home"))
  
  plot_color <- ifelse(wide_matrix$type == "cases", "darkblue",
                       ifelse(wide_matrix$type == "work", "darkred",
                              ifelse(wide_matrix$type == "grocery", "lightblue",
                                     ifelse(wide_matrix$type == "transit", "pink",
                                            "darkgreen"))))
  
  ggplot(wide_matrix, aes(x = country, y = sd, fill = type)) +
    geom_bar(stat = "identity") +
    xlab("Country") +
    ylab("Social distancing coefficient") +
    facet_wrap(~ type, nrow = 5, scales = "free_y") +
    scale_fill_manual(values=c("darkblue","darkred","lightblue","pink","darkgreen")) +
    theme_gray() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#F0F0F0")) ##F0F0F0
}

plot_social_distancing_new(combined_data)

#######
function(wide_matrix, v="cases") {
  wide_matrix <- wide_matrix[,c(1,13)]
  wide_matrix$sd <- as.double(wide_matrix$sd)
  plot_color <- ifelse(v == "cases", "darkblue", 
                       ifelse(v == "work", "darkred", 
                              ifelse(v == "grocery", "lightblue",
                                     ifelse(v == "transit", "pink", 
                                            "darkgreen"))))
}

p<-ggplot(combined_data, aes(x=sd, y=country, fill = type))+
  geom_col(alpha= 3.2, position = "dodge", color= "grey")

#######

ggplot(combined_data, aes(sd, country, fill=type, color=type, group=type)) + #reorder(country,sd)
  geom_bar(stat="identity", fill=type) +
  xlab("Social distancing coefficient") +
  ylab("Country")

plot_social_distancing_new(combined_data) + coord_flip()

get_coeff_foreign_var <- function() {
  country_codes <- read.csv("../../data/raw/iso3_iso2_country_codes.csv")
  contemp_effects <- read.csv("../../results/contemporaneous_effects_foreign_cases.csv")
  full_name <- c()
  for(i in 1:length(contemp_effects$country_name))
  {
    for(j in 1:length(country_codes$iso_2))
    {
      ifelse(contemp_effects$country_name[i] == country_codes$iso_2[j],
              full_name <- c(full_name,country_codes$Definition[j]), 1)
    }
  }
  contemp_effects$full_name <- full_name
  contemp_effects <- contemp_effects[,c(1,3,2)]
  colnames(contemp_effects) <- c("country_code","country_name","contemporaneous_cases_coefficient")
  contemp_effects <- contemp_effects[order(contemp_effects$country_name),]
  contemp_effects$contemporaneous_cases_coefficient <- as.double(format(contemp_effects$contemporaneous_cases_coefficient, scientific = F))
  write.csv(contemp_effects[2:3],"../../results/coefficients_foreign_variable_alpha.csv",row.names = F)
  contemp_effects
}

head(df_contour,15) %>%
 ggplot(aes(p_lag,q_lag,color=log_likelihood)) +
 geom_point(alpha=0.5,size=2) +
 labs(x="p lag",y="q lag") +
 scale_y_continuous(breaks= pretty_breaks())

plot_foreign_impact(cases)
plot_social_distancing(cases)

plot_foreign_impact(home)
plot_social_distancing(home)

plot_foreign_impact(work)
plot_social_distancing(work) + coord_flip()

plot_foreign_impact(transit)
plot_social_distancing(transit)

plot_foreign_impact(grocery)
plot_social_distancing(grocery)