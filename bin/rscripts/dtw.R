library(reshape2)
library(dtw)

#setwd('Projects/covid-analysis/')
df_google = read.csv('../../data/tidy/google_activity.csv')


# Here, we make all the names uniform
# Ideally, this should be done in the preprocessing script in python (make this a TODO)
names(df_google)[3:(ncol(df_google))] = names(df_covid)[3:(ncol(df_covid))]

#Standardize names of first 2 columns (This can/should also be done in python pre-processing script) (TODO)
#names(df_google)[1:2] = c('iso','variable') 

#sort all df's by iso: (TODO)
sort_df_by_iso <- function(df) {
  df <- df[order(df[['iso']]), ]
  rownames(df)<-1:nrow(df)
  return(df)
}

df_google = sort_df_by_iso(df_google)

iso3iso2 = read.csv('../../data/raw/iso3_iso2_country_codes.csv')
change_iso3_to_iso2 <- function(iso_code) {
  if (nchar(iso_code) == 3) {
    iso_code = subset(iso3iso2, iso_3==iso_code)$iso_2
  }
  return(iso_code)
}


df_google$iso = apply(df_google[1], 1, change_iso3_to_iso2)

# Convert dataframe
melted.df <- melt(df_google, id.vars = c('iso', 'activity'))
m.dat <- dcast(melted.df, iso + variable ~ activity)
colnames(m.dat)[1:2] = c('Country', 'Date') #, 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )

# Convert countries to factors
m.dat$iso <- as.factor(m.dat$iso)
m.dat <- subset(m.dat, select = -c(Date) )

m.dat <- na.omit(m.dat) # remove NAs

# Convert numbers to numeric
for (i in seq(3, ncol(m.dat))) {
  m.dat[,i] = as.numeric(m.dat[,i], na.pass=TRUE)
}

########################################
# DTW 
########################################
dm <- matrix(NA, nrow=length(unique(m.dat$Country)), ncol =length(unique(m.dat$Country))  )
diag(dm) <- 0
dm
rownames(dm) <- unique(m.dat$Country)
colnames(dm) <- unique(m.dat$Country)

ii = 0
for (i in unique(m.dat$Country)) {
  ii = ii + 1
  jj = 0
  for (j in unique(m.dat$Country)) {
    jj = jj + 1
    if (jj > ii) {
      dm[i,j] <- dtw(dist(m.dat[m.dat$Country==i,2:7], m.dat[m.dat$Country==j,2:7]), distance.only = T)$normalizedDistance
    }
  }
}

path_out = '../../results/'
write.csv(dm,file=paste(path_out,'disimilarity-matrix-mobility-google.csv',sep = ''))

### OLDER 


# Correct Google (add 100 to baseline)
# m.dat[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] = m.dat[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] + 100

# Subset data if necessary (here we remove COVID)
# endovars <-  c('car', 'tran', 'walk', 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )
# m.data <- subset(m.dat, select = c('Country', endovars) )
#m.data <- na.omit(m.data) # remove NAs
#m.data <- m.data[-1,]

# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# df <- read.csv('mobility/cases_mobility_activity.csv')
# df <- df[, c(2,3,7:190)] #36:95

# # Infer missing Apple data
# df[df$transportation_type=='driving','X5.11.2020'] = df[df$transportation_type=='driving','X5.10.2020']
# df[df$transportation_type=='driving','X5.12.2020'] = df[df$transportation_type=='driving','X5.10.2020']
# df[df$transportation_type=='walking','X5.11.2020'] = df[df$transportation_type=='walking','X5.10.2020']
# df[df$transportation_type=='walking','X5.12.2020'] = df[df$transportation_type=='walking','X5.10.2020']
# df[df$transportation_type=='transit','X5.11.2020'] = df[df$transportation_type=='transit','X5.10.2020']
# df[df$transportation_type=='transit','X5.12.2020'] = df[df$transporttaion_type=='tranist','X5.10.2020']
# 



#write.csv(m.dat, file="results/melted-data.csv")