#library(BGVAR)
library(reshape2)
library(panelvar)
#library(pacman)
#p_load(tidyverse,panelvar)

setwd('Projects/covid-analysis/')
df <- read.csv('mobility/cases_mobility_activity.csv')
df <- df[, c(2,3,36:95)] #7:190
df[df$transportation_type=='driving','X5.11.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='driving','X5.12.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='walking','X5.11.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='walking','X5.12.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='transit','X5.11.2020'] = df[df$transportation_type=='transit','X5.10.2020']
df[df$transportation_type=='transit','X5.12.2020'] = df[df$transportation_type=='transit','X5.10.2020']



melted.df <- melt(df, id.vars = c('region', 'transportation_type'))
m.data <- dcast(melted.df, region + variable~transportation_type)
colnames(m.data) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )
endovars <-  c('cov', 'car', 'home', 'tstop', 'work' )
m.data <- subset(m.data, select = c('Country', 'Date', endovars) )
m.data <- na.omit(m.data)

m.data$Country <- as.factor(m.data$Country)
for (i in seq(3, length(c('Country', 'Date', endovars)) )) {
  m.data[,i] = as.numeric(m.data[,i])
}
#ggplot(m.data, aes(x=Date,y=car,color=Country,group=Country)) + geom_point() + geom_line()
#ggplot(m.data, aes(x=Date,y=home,color=Country,group=Country)) + geom_point() + geom_line()

mod.pvar1 <- pvargmm(
  dependent_vars = endovars,
  lags = 1,
  transformation = "fd",
  data = m.data,
  panel_identifier=c("Country", "Date"),
  steps = c("twostep"),
  system_instruments = FALSE,
  collapse = FALSE
)


  #m.data <- subset(m.data, select = c(Country, Data, cov, car, groc, parks, home, reta, reta))
#l.data <- vector(mode = "list", length = length(unique(m.data$Country)))
# for (i in unique(m.data$Country)) {
#   l.data[[i]] <- subset(m.data, Country==i)
#   l.data[[i]] <- l.data[[i]][,3:12]
#   l.data[[i]] <- data.matrix(l.data[[i]], rownames.force = NA)
# }

#w.data <- read.csv('963652501_T_T100I_MARKET_ALL_CARRIER_2019_YEAR.csv')
#acast(w.data, ORIGIN_COUNTRY~DEST_COUNTRY, sum, value.var="PASSENGERS")
# 
# model.1<-bgvar(Data=df,
#                     W=W.trade0012,
#                     saves=100,
#                     burns=100,
#                     plag=2,
#                     prior="NG",
#                     SV=TRUE,
#                     thin=1,
#                     trend=TRUE,
#                     h=0,
#                     save.country.store=FALSE,
#                     multithread=TRUE,
#                     eigen=1.05
#                     )

