library(ggplot2)
library(data.table)
library(BGVAR)

#df <- read.csv('../../results/cases_lag.csv')
#names(df)[2:11] =  seq(0,9)
##row.names(df) = df$Country
##df <- subset(df, select = -Country)
#melted.df <- melt(as.data.table(df), id.vars = 'country')

#dev.new(width=5, height=10, unit="in")
#ggplot(melted.df, aes(variable, country, fill= value)) + 
#    geom_tile() +
#    scale_fill_distiller(palette = "Spectral",direction=1) 

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

cases <- estimate_foreign_impact("cases")
home <- estimate_foreign_impact("residential")
work <- estimate_foreign_impact("work")
transit <- estimate_foreign_impact("transit")
grocery <- estimate_foreign_impact("grocery")

plot_foreign_impact <- function(wide_matrix) {
  wide_matrix <- wide_matrix[1:11]
  names(wide_matrix)[2:11] =  seq(0,9)  
  melted.mtx <- melt(as.data.table(wide_matrix), id.vars = 'country')
  melted.mtx$value <- as.numeric(melted.mtx$value)
  ggplot(melted.mtx, aes(variable, country, fill= value)) + 
    geom_tile() +
    scale_fill_distiller(palette = "Spectral",direction=1)
  }

plot_foreign_impact(cases)
plot_foreign_impact(home)
plot_foreign_impact(work)
plot_foreign_impact(transit)
plot_foreign_impact(grocery)
