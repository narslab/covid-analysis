library(ggplot2)
library(stringr)
library(ggpubr)
library(reshape2)
library(grid)
setwd('Projects/covid-analysis/')

cobs <- read.csv('../results/melted-data.csv', row.names = 1)
clusters <- read.csv('../results/country-clusters.csv',row.names = 1)

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

cobs$Date <- as.POSIXct(str_remove(cobs$Date, "X"),format="%m.%d.%Y")
#cobs$Date <- as.Date(cobs$Date)
#plotpars <-  

cobs1 <- subset(cobs, select= c('cluster','Country', 'Date','work','tstop','groc','reta','home','parks'))
cobs1 <- melt(cobs1, id.vars = c('cluster','Country', 'Date'))


cobs2 <- subset(cobs, select= c('cluster','Country', 'Date','car','tran','walk'))
cobs2 <- melt(cobs2, id.vars = c('cluster','Country', 'Date'))

cobs3 <- subset(cobs, select= c('cluster','Country', 'Date','cov'))
cobs3 <- melt(cobs3, id.vars = c('cluster','Country', 'Date'))

cobs4 <- subset(cobs, select= c('cluster','Country', 'Date','cov'))
cobs4[-1,'cov'] <- diff(log(cobs4$cov))
cobs4 <- cobs[-1,]
cobs4 <- melt(cobs4, id.vars = c('cluster','Country', 'Date'))

#plt.c1.work <- 

lims <- as.POSIXct(strptime(c("2020-02-15", "2020-07-15"), format = "%Y-%m-%d"))

lims2 <- as.POSIXct(strptime(c("2020-01-15", "2020-07-15"), format = "%Y-%m-%d"))


google.names <- c("Work", "Transit Stops", "Groceries", "Retail", "Home", "Parks")
names(google.names) <- c('work','tstop','groc','reta','home','parks')

apple.names <- c("Driving", "Transit", "Walking")
names(apple.names) <- c('car','tran','walk')


### GOOGLE
ggplot(data = cobs1[cobs1$cluster==1,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = google.names)) + ylim(25,175) +
  #theme() + 
  theme_bw(base_size=11) +
  labs(y="Change from 100% baseline", x="",color="Cluster 1") + 
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims) #breaks = date_breaks("1 month"), labels=date_format("%m"),

ggsave("../results/c1-activity.png", width = 14, height = 4, units="in", dpi="retina" )


ggplot(data = cobs1[cobs1$cluster==2,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = google.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 2") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims) #breaks = date_breaks("1 month"), labels=date_format("%m"),

ggsave("../results/c2-activity.png", width = 14, height = 4, units="in", dpi="retina" )

ggplot(data = cobs1[cobs1$cluster==3,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = google.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 3") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims) + 
  guides(col=guide_legend(ncol=1))

ggsave("../results/c3-activity.png", width = 14, height = 4, units="in", dpi="retina" )

ggplot(data = cobs1[cobs1$cluster==4,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = google.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 4") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims) + 
  guides(col=guide_legend(ncol=1))
ggsave("../results/c4-activity.png", width = 14, height = 4, units="in", dpi="retina" )


### APPLE
ggplot(data = cobs2[cobs2$cluster==1,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = apple.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 1") + 
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) #breaks = date_breaks("1 month"), labels=date_format("%m"),

ggsave("../results/c1-mobility.png", width = 14, height = 4, units="in", dpi="retina" )


ggplot(data = cobs2[cobs2$cluster==2,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = apple.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 2") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) #breaks = date_breaks("1 month"), labels=date_format("%m"),
ggsave("../results/c2-mobility.png", width = 14, height = 4, units="in", dpi="retina" )

ggplot(data = cobs2[cobs2$cluster==3,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = apple.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 3") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1))
ggsave("../results/c3-mobility.png", width = 14, height = 4, units="in", dpi="retina" )

ggplot(data = cobs2[cobs2$cluster==4,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=1, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  facet_grid(cols = vars(variable), labeller = labeller(.cols = apple.names)) + ylim(25,175) +
  theme_bw(base_size=10) +
  labs(y="Change from 100% baseline", x="",color="Cluster 4") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1))
ggsave("../results/c4-mobility.png", width = 14, height = 4, units="in", dpi="retina" )

### COVID
ggplot(data = cobs3[cobs3$cluster==1,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 1") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20)  +
  annotate("text", x=lims[1], y=19, label= "Cluster 1", size=10, color=colors[1]) 
ggsave("../results/c1-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==2,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 2") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20) +
  annotate("text", x=lims[1], y=19, label= "Cluster 1", size=10, color=colors[1]) 
ggsave("../results/c2-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==3,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 3") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20) +
  annotate("text", x=lims[1], y=19, label= "Cluster 1", size=10, color=colors[1]) 
ggsave("../results/c3-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==4,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 4") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.3, "cm"), strip.text.x = element_text(size = 14)) +
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20) +
  annotate("text", x=lims[1], y=19, label= "Cluster 1", size=10, color=colors[1]) 
ggsave("../results/c4-cov.png", width = 6, height = 4, units="in", dpi="retina" )

### DIFF
### COVID
ggplot(data = cobs4[cobs4$cluster==1,], aes(x = Date, y = value, color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 1") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.2, "cm")) + #legend.text = element_text(margin = margin(t = 2)) ) + 
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20)
ggsave("../results/c1-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==2,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 2") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.2, "cm")) + #legend.text = element_text(margin = margin(t = 2)) ) + 
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20)
ggsave("../results/c2-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==3,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 3") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.2, "cm")) + #legend.text = element_text(margin = margin(t = 2)) ) + 
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20)
ggsave("../results/c3-cov.png", width = 6, height = 4, units="in", dpi="retina" )

ggplot(data = cobs3[cobs3$cluster==4,], aes(x = Date, y = log(value), color = Country, group = Country)) +
  geom_point(size=.5, alpha=.4) + 
  geom_smooth(method = "gam", size=.5, alpha=.25) + # geom_smooth(size=.5)
  #geom_point() + geom_line()  + 
  #facet_grid(cols = vars(variable)) + ylim(15,175) +
  theme_bw(base_size=10) +
  labs(y="Log new confirmed cases", x="",color="Cluster 4") +
  theme(legend.key = element_rect(size = 7),  legend.key.height = unit(.2, "cm")) + #legend.text = element_text(margin = margin(t = 2)) ) + 
  scale_x_datetime(limits = lims2) + 
  guides(col=guide_legend(ncol=1)) + ylim(0,20)
ggsave("../results/c4-cov.png", width = 6, height = 4, units="in", dpi="retina" )

# dl.cobs <- cobs[cobs$cov>0,]
# #dl.cobs[,'cov'] 
# cc = log(diff(rollmean(dl.cobs[,'cov'], 3)))
# dd = log(tail(rollmean(dl.cobs[,'cov'], 3), -4)) /log(rollmean(dl.cobs[,'cov'], 7))
# plot(cc)
# 
# ggplot(data = diff(log(cobs[cobs$cluster==4,])), aes(x = Date, y = cov, color = Country, group = Country) )  + geom_point() + geom_line()



