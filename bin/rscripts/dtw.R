###
### Generates melted-data.csv which contains Google activity data.
### Additionally, it generates a matrix with information about the 4 clusters,
### specifically: disimilarity-matrix-mobility-google.csv
###


library(reshape2)
library(dtw)
library(dplyr)

#setwd('Projects/covid-analysis/')
df_google = read.csv('../../data/tidy/google_activity.csv')
path_out = '../../results/'

# Here, we make all the names uniform
# Ideally, this should be done in the preprocessing script in python (make this a TODO)
#names(df_google)[3:(ncol(df_google))] = names(df_covid)[3:(ncol(df_covid))]

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

countries_complete_data_bgvar = c("AE","AR","AU","AT","BE","BG","BR","CA","CL","CO","CZ","DE","DK","EG","ES","EE","FI","FR","GB","GR",
                                  "HR","HU","ID","IN","IE","IL","IT","JP","KH","KR","LT","LV","MA","MX","MY","NL","NZ","PH","PT","RU","SA","SG","SK","SI","SE","TH","TR","UA","UY","US","VN","ZA")
m.dat = subset(m.dat, (m.dat$Country %in% countries_complete_data_bgvar)) 

#Get complete country name
merged_dat <- merge(m.dat, iso3iso2, by.x = "Country", by.y = "iso_2", all.x = TRUE)
merged_dat <- merged_dat[, c("Country", "Definition", "Date", "grocery", "parks", "residential", "retail", "transit", "workplaces")]
colnames(merged_dat) <- c("Country", "full_country_name", "Date", "grocery", "parks", "residential", "retail", "transit", "workplaces")
merged_dat <- merged_dat %>% rename(iso2 = Country, Country = full_country_name)
merged_dat <- merged_dat %>%
  mutate(Country = recode(Country,
                          "Korea, Republic of" = "South Korea",
                          "Russia Federation" = "Russia",
                          "United Arab Emirates" = "UAE",
                          "United Kingdom" = "UK",
                          "United States" = "US"))


merged_dat <- subset(merged_dat, select = -c(parks, retail))
merged_dat <- na.omit(merged_dat) # remove NAs

unique_data <- unique(merged_dat[, c("iso2", "Country")])
write.csv(unique_data, "../../results/unique_data.csv", row.names = FALSE)

# Convert numbers to numeric
for (i in seq(4, ncol(merged_dat))) {
  merged_dat[,i] = as.numeric(merged_dat[,i], na.pass=TRUE)
}

merged_dat[,c( 'grocery', 'residential', 'transit', 'workplaces' )] = merged_dat[,c( 'grocery', 'residential', 'transit', 'workplaces' )] + 100 

# Convert countries to factors
#m.dat$iso <- as.factor(m.dat$iso)
write.csv(merged_dat, file=paste(path_out, 'melted-data.csv', sep = '')) # Save melted-data with Date column needed for activity plots
head(merged_dat)
merged_dat <- subset(merged_dat, select = -c(Date, iso2) ) # Remove Date as it is not needed for DTW

#m.dat <- na.omit(m.dat) # remove NAs

########################################
# DTW 
########################################
dm <- matrix(NA, nrow=length(unique(merged_dat$Country)), ncol =length(unique(merged_dat$Country))  )
diag(dm) <- 0
dm
rownames(dm) <- unique(merged_dat$Country)
colnames(dm) <- unique(merged_dat$Country)

ii = 0
for (i in unique(merged_dat$Country)) {
  ii = ii + 1
  jj = 0
  for (j in unique(merged_dat$Country)) {
    jj = jj + 1
    if (jj > ii) {
      dm[i,j] <- dtw(dist(merged_dat[merged_dat$Country==i,2:5], merged_dat[merged_dat$Country==j,2:5]), distance.only = T)$normalizedDistance
    }
  }
}

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