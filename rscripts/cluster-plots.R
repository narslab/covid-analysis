library(ggplot2)
setwd('Projects/covid-analysis/')

cobs <- read.csv('results/melted-data.csv', row.names = 1)
clusters <- read.csv('results/country-clusters.csv',row.names = 1)

cobs$cluster <- NA
for (i in row.names(clusters)){
  cobs[cobs$Country==i, 'cluster'] = clusters[i,]
}

head(cobs)

colors = c( # '#ffff99', ##d8ac93', # '#ffff99', #or yellowversions 
  '#66c2a5',
  '#fc8d62',
  '#8da0cb',
  '#e78ac3',
  '#a6d854',
  '#ffd92f',
  '#e5c494')

#par(mfrow = c(2, 2)) # Create a 2 x 2 plotting matrix
ggplot(data = cobs[cobs$cluster==1,], aes(x = Date, y = work, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==1,], aes(x = Date, y = home, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==1,], aes(x = Date, y = tstop, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==1,], aes(x = Date, y = reta, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==1,], aes(x = Date, y = cov, color = Country, group = Country) )  + geom_point() + geom_line()


ggplot(data = cobs[cobs$cluster==2,], aes(x = Date, y = work, color = Country, group = Country) )  + geom_point() + geom_line() 
ggplot(data = cobs[cobs$cluster==2,], aes(x = Date, y = home, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==2,], aes(x = Date, y = tstop, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==2,], aes(x = Date, y = reta, color = Country, group = Country) )  + geom_point() + geom_line() 
ggplot(data = cobs[cobs$cluster==2,], aes(x = Date, y = cov, color = Country, group = Country) )  + geom_point() + geom_line()


ggplot(data = cobs[cobs$cluster==3,], aes(x = Date, y = work, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==3,], aes(x = Date, y = home, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==3,], aes(x = Date, y = tstop, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==3,], aes(x = Date, y = reta, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==3,], aes(x = Date, y = cov, color = Country, group = Country) )  + geom_point() + geom_line()


ggplot(data = cobs[cobs$cluster==4,], aes(x = Date, y = work, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==4,], aes(x = Date, y = home, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==4,], aes(x = Date, y = tstop, color = Country, group = Country) )  + geom_point() + geom_line()
ggplot(data = cobs[cobs$cluster==4,], aes(x = Date, y = reta, color = Country, group = Country) )  + geom_point() + geom_line()

dl.cobs <- cobs[cobs$cov>0,]
#dl.cobs[,'cov'] 
cc = log(diff(rollmean(dl.cobs[,'cov'], 3)))
dd = log(tail(rollmean(dl.cobs[,'cov'], 3), -4)) /log(rollmean(dl.cobs[,'cov'], 7))
plot(cc)

ggplot(data = diff(log(cobs[cobs$cluster==4,])), aes(x = Date, y = cov, color = Country, group = Country) )  + geom_point() + geom_line()



