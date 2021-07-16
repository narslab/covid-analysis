#library(BGVAR)
library(reshape2)
library(panelvar)
library(dtw)
#library(pacman)
#p_load(tidyverse,panelvar)

#setwd('Projects/covid-analysis/')
df <- read.csv('../../data/tidy/cases_mobility_activity.csv')
df <- df[, c(2,4,8:186)] #36:95

# Infer missing Apple data
df[df$transportation_type=='driving','X5.11.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='driving','X5.12.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='walking','X5.11.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='walking','X5.12.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='transit','X5.11.2020'] = df[df$transportation_type=='transit','X5.10.2020']
df[df$transportation_type=='transit','X5.12.2020'] = df[df$transportation_type=='transit','X5.10.2020']


# Convert dataframe
melted.df <- melt(df, id.vars = c('region', 'transportation_type'))
m.data <- dcast(melted.df, region + variable~transportation_type)
colnames(m.data) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )

# Convert countries to factors
m.data$Country <- as.factor(m.data$Country)

# Convert numbers to numeric
for (i in seq(3, length(c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )  ) )) {
  m.data[,i] = as.numeric(m.data[,i], na.pass=TRUE)
}

# Correct Google (add 100 to baseline)
m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] = m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] + 100

# Remove Apple data
endovars <-  c('car', 'tran', 'walk', 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )
m.data <- subset(m.data, select = c('Country', 'Date', endovars) )
m.data <- na.omit(m.data)