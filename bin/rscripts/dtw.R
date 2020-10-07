library(reshape2)
library(dtw)

#setwd('Projects/covid-analysis/')
df <- read.csv('mobility/cases_mobility_activity.csv')
df <- df[, c(2,3,7:190)] #36:95

# Infer missing Apple data
df[df$transportation_type=='driving','X5.11.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='driving','X5.12.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='walking','X5.11.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='walking','X5.12.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='transit','X5.11.2020'] = df[df$transportation_type=='transit','X5.10.2020']
df[df$transportation_type=='transit','X5.12.2020'] = df[df$transportation_type=='transit','X5.10.2020']


# Convert dataframe
melted.df <- melt(df, id.vars = c('region', 'transportation_type'))
m.dat <- dcast(melted.df, region + variable~transportation_type)
colnames(m.dat) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )

# Convert countries to factors
m.dat$Country <- as.factor(m.dat$Country)

# Convert numbers to numeric
for (i in seq(3, length(c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )  ) )) {
  m.dat[,i] = as.numeric(m.dat[,i], na.pass=TRUE)
}

# Correct Google (add 100 to baseline)
m.dat[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] = m.dat[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] + 100

# Subset data if necessary (here we remove COVID)
endovars <-  c('car', 'tran', 'walk', 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )
m.data <- subset(m.dat, select = c('Country', endovars) )
#m.data <- na.omit(m.data) # remove NAs
#m.data <- m.data[-1,]

#m.data <- m.data[m.data$cov >= 1,] # remove all zero covid cases
# m.data['day'] = 1 # initialize day column
# for (i in unique(m.data$Country)) {
#   for (j in seq(1,nrow(m.data[m.data$Country==i,]) ) ) {
#     m.data[m.data$Country==i,][j, 'day'] = j
#   }
# }
#m.data <- m.data[m.data$day <= 60,]


# Drop date column
#m.data <- subset(m.data, select = -Date)


#Log and difference
#m.data[,endovars] = log(m.data[,endovars])
#diff(m.data[,endovars])

#ggplot(m.data, aes(x=Date,y=car,color=Country,group=Country)) + geom_point() + geom_line()
#ggplot(m.data, aes(x=Date,y=home,color=Country,group=Country)) + geom_point() + geom_line()

# Write m.data to file

write.csv(m.dat, file="results/melted-data.csv")
          
########################################
# DTW 
########################################
dm <- matrix(NA, nrow=length(unique(m.data$Country)), ncol =length(unique(m.data$Country))  )
diag(dm) <- 0
dm
rownames(dm) <- unique(m.data$Country)
colnames(dm) <- unique(m.data$Country)

ii = 0
for (i in unique(m.data$Country)) {
  ii = ii + 1
  jj = 0
  for (j in unique(m.data$Country)) {
    jj = jj + 1
    if (jj > ii) {
      dm[i,j] <- dtw(dist(m.data[m.data$Country==i,], m.data[m.data$Country==j,]), distance.only = T)$normalizedDistance
    }
  }
}

write.csv(dm,file='disimilarity-matrix-mobility.csv')
