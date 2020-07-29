library(BGVAR)
library(reshape2)

setwd('Projects/covid-analysis/')
df <- read.csv('mobility/cases_mobility_activity.csv')
df <- df[, c(2,3,7:190)]
melted.df <- melt(df, id.vars = c('region', 'transportation_type'))
melted.df <- na.omit(melted.df) #data.frame(lapply(melted.df, function(x) {gsub("<NA>", NA, x)}))
m.data <- dcast(melted.df, region + variable~transportation_type)
colnames(m.data) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )
w.data <- read.csv('963652501_T_T100I_MARKET_ALL_CARRIER_2019_YEAR.csv')


acast(w.data, ORIGIN_COUNTRY~DEST_COUNTRY, sum, value.var="PASSENGERS")

model.1<-bgvar(Data=df,
                    W=W.trade0012,
                    saves=100,
                    burns=100,
                    plag=2,
                    prior="NG",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=1,
                    trend=TRUE,
                    h=0,
                    save.country.store=FALSE,
                    multithread=TRUE,
                    eigen=1.05
                    )
