library(ggplot2)
library(data.table)

df <- read.csv('../../results/cases_lag.csv')
names(df)[2:11] =  seq(0,9)
#row.names(df) = df$Country
#df <- subset(df, select = -Country)
melted.df <- melt(as.data.table(df), id.vars = 'country')

#dev.new(width=5, height=10, unit="in")
ggplot(melted.df, aes(variable, country, fill= value)) + 
    geom_tile() +
    scale_fill_distiller(palette = "Spectral",direction=1) 
