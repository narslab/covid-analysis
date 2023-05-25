df_covid = read.csv('../../data/tidy/cases_deaths_concat.csv')
iso3iso2 = read.csv('../../data/raw/iso3_iso2_country_codes.csv')

names(df_covid)[3:(ncol(df_covid))] = gsub(x = names(df_covid)[3:201], pattern = "X", replacement = "")
df_covid = cbind(df_covid[['iso']], df_covid[['X']], df_covid[,3:(ncol(df_covid))]) #reorder ISO and Variable columns TODO

names(df_covid)[1:2] = c('iso','variable')

sort_df_by_iso <- function(df) {
  df <- df[order(df[['iso']]), ]
  rownames(df)<-1:nrow(df)
  return(df)
}

df_covid = sort_df_by_iso(df_covid)

head(df_covid)

change_iso3_to_iso2 <- function(iso_code) {
  if (nchar(iso_code) == 3) {
    iso_code = subset(iso3iso2, iso_3==iso_code)$iso_2
  }
  return(iso_code)
}

df_covid$iso = apply(df_covid[1], 1, change_iso3_to_iso2)

# Appropriately difference COVID cases and Google activity data
df_covid[, 5:201] = matrixStats::rowDiffs(as.matrix(df_covid[, 3:201]), differences=2L) # convert to daily cases and then difference
names(df_covid)[names(df_covid) == "variable"] <- "cov"

head(df_covid)

melted.covid.df <- reshape2::melt(df_covid, id.vars = c('iso', 'cov'))
covid.dat <- reshape2::dcast(melted.covid.df, iso + variable ~ cov)
covid.dat <- subset(covid.dat, select = -c(deaths))

# Convert 'variable' column format
covid.dat$variable <- as.Date(covid.dat$variable, format = "%m.%d.%Y")

# Change column names
colnames(covid.dat)[names(covid.dat) == "cases"] <- "cov"
colnames(covid.dat)[colnames(covid.dat) == "iso"] <- "iso2"
colnames(covid.dat)[colnames(covid.dat) == "variable"] <- "Date"

head(covid.dat)

write.csv(covid.dat, file = "../../results/melted-covid-data.csv")
