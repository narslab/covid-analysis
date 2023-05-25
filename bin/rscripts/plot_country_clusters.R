library(readxl)
library(dplyr) # rename
library(ggplot2)
library(PerformanceAnalytics) #chart.Correlation
library(RColorBrewer) #brewer.pal
typology.colors = brewer.pal(n = 3, name = "Set2") # Best color scheme is Set2 for color blind friendly 

library(fmsb) #radarchart
library(rworldmap) #joinCountryData2Map
library(tidyr) #drop_na
require(mapproj) # required for coord_map function (otherwise map won't display)
library(sf)


library(reshape2)

### Data
iso.country.codes <- read_excel("../../data/raw/iso_2digit_alpha_country_codes.xls")
write.csv(iso.country.codes, file = "../../data/raw/iso_2digit.csv", row.names = FALSE)
iso.codes <- read.csv("../../data/raw/iso_2digit.csv")


df.clusters <- read.csv("../../results/country-clusters.csv")
colnames(df.clusters) = c('region', 'value')

head(iso.codes)
head(df.clusters)

# join df1 and df2 on region column
df3 <- merge(df.clusters, iso.codes, by.x = "region", by.y = "Code.Value")
df3
# select only the columns we need (value and Definition)
df3 <- select(df3, Definition, value)

# rename the columns to match the original names
colnames(df3) <- c("region", "value")
(df3)


plotTypologyWorldMap <- function(df){
  colnames(df) = c('region', 'value') # change column names for ggplot
  
  # This is to convert country names in df to match those in the worldMap dataframe
  coords <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "region")
  coords = data.frame(coords)
  colnames(coords) = tolower(colnames(coords))
  colnames(coords)
  coords = drop_na(coords, value)
  print(coords$name)
  setdiff(df$region,coords$name)
  
  # Correct the country names in DF
  df[df=='Timor-Leste'] = "East Timor"
  df[df=='Central African Republic'] = "Central African Rep."
  df[df=='Cote d\'Ivoire'] = "Ivory Coast"
  df[df=='Dominican Republic'] = "Dominican Rep."
  df[df=='Kyrgyz Republic'] = "Kyrgyzstan"
  df[df=='Eswatini'] = "Swaziland"
  df[df=='Congo'] = "Congo (Brazzaville)"
  df[df=='Congo Democratic Republic'] = "Congo (Kinshasa)"
  df[df=='Republic of Korea'] = "S. Korea"
  df[df=='Korea, Republic of'] = "S. Korea"
  df[df=='Russia Federation'] = "Russia"
  
  print(head(df))
  # Get world map data (lat/long)
  countrynames = df$region
  worldMap <- getMap()
  country_indices <- which(worldMap$NAME%in%countrynames)
  
  # Get coordinates for all countries to plot base map
  allCoords <- lapply(seq(1,243), function(i){
    dfn <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    dfn$region =as.character(worldMap$NAME[i])
    colnames(dfn) <- list("long", "lat", "region")
    return(dfn)
  })
  allCoords <- do.call("rbind", allCoords)
  
  # Get coordinates strictly for those in water accessibility dataset
  waterCoords <- lapply(country_indices, function(i){
    dfn <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    dfn$region =as.character(worldMap$NAME[i])
    colnames(dfn) <- list("long", "lat", "region")
    return(dfn)
  })
  
  waterCoords <- do.call("rbind", waterCoords)
  waterCoords$value <- df$value[match(waterCoords$region,df$region)]
  waterCoords$value <- factor(waterCoords$value)
  
  # Plot
  #options(repr.plot.width=10, repr.plot.height=7)
  par(mar = c(0,0,0,0))
  par(cex=1)
  library(ggplot2)
  
  # Original code
  
  m <- ggplot() +
    geom_sf(color = "black", fill= 'antiquewhite') +
    xlab("") + ylab("") +
    geom_polygon(data= allCoords, mapping = aes(x =long, y=lat, group = region), color="grey", fill=NA) + 
    geom_polygon( data = waterCoords, mapping = aes(x = long, y = lat, group = region, fill=value), linewidth=.3,color="black") +
    expand_limits(x = waterCoords$long, y = waterCoords$lat)  + 
    scale_fill_manual(values = c("#8da0cb", "#fc8d62", "#66c2a5", "#e78ac3"), name="", #"Water Accessibility Typologies", 'Set2'
                      na.value = "white",
                      labels = c("Cluster 1" , "Cluster 2", "Cluster 3", "Cluster 4"),
                      guide = guide_legend(label.position = "top"))  + # move labels to the center
    theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "white"),
      # Minimize margins and zoom in on the map
      plot.margin = unit(c(0, -10, 0, -1), "cm")
    ) + 
    theme(
      legend.position = "bottom", legend.direction = 'horizontal', legend.justification = c(.4, .4),#c(.8,-0.05)
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(), 
      axis.text=element_text(size=12),
      axis.title=element_text(size=12)
    ) +
    theme(
      legend.key.size = unit(.7, 'cm'), #change legend key size
      legend.title = element_text(size=12,face=),
      legend.text = element_text(size=12)
    ) +
    scale_x_continuous(limits = c(-145, 280)) +
    scale_y_continuous(limits = c(-55, 100)) #+  
  #guides(fill = guide_colorbar(barwidth = 10, barheight = .5))
  #pdf(file = "../../figures/typology-world-map.pdf",width=30, height=15)
  #print(m + coord_map(xlim = c(-180, 180),ylim = c(-50, 75))) #print required to generate pdf within function
  ggsave(filename = "../../figures/typology-world-map.png", plot = m, type = "png", dpi = "retina")
  
  plot(m)
}


plotTypologyWorldMap(df3)
