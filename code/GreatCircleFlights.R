#################################################################3
## Let's try to use this with ggplot2


### http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
### http://robinlovelace.net/2014/06/22/great-circles-in-rworldmap-ggplot2.html
### http://egallic.fr/maps-with-r/

library(maps)
library(geosphere)
library(plyr)  

library(sp)
#install.packages("rworldmap")
library(rworldmap)
#install.packages("rgdal")
library(rgdal) 
library(ggplot2) 
library(gpclib)


## get worldmap data
worldmap1 <- map_data ("world")

## Get another worldmap
worldmap <- getMap() # load the map data class(s) # what type of are we dealing with? 
worldmap.map <- ggplot(worldmap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="green", colour="black")
worldmap.map

## Reproject world map in Robinson
worldmap.proj1 <- spTransform(worldmap, CRSobj=CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
worldmap.proj1.map <- ggplot(worldmap.proj1 , aes(x=long, y=lat, group=group)) + geom_polygon(fill="green", colour="black")
worldmap.proj1.map


### Fortify WorldMap
world.points <- fortify(worldmap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]

worldmap.map2 <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)
worldmap.map2


###################################################################
### Theme map style Facebook https://github.com/ricardo-bion/medium_visualization

# ggplot2 map themes 


theme_map <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title      =   element_blank(),
      axis.text       =   element_blank(),
      axis.ticks.length   =   unit(0,"cm"),
      panel.grid.major= element_line(colour="black", size=0.3, linetype=1),
     # panel.grid.minor=element_blank(),
     # panel.grid      =   element_blank(),
     # panel.background = element_rect(fill = "grey10", colour=NA),
      panel.margin    =   unit(0,"lines"),
      plot.title=element_text(vjust=1),
     # strip.background=element_rect(fill="grey90", colour="black", size=0.3),
     # strip.text=element_text(),
      plot.margin     =   unit(c(0,0,0,0),"lines"),
      plot.background = element_blank(),
      legend.justification = c(0, 0),
      legend.position = c(0, 0),
      complete = TRUE
    )
}


#head(tail(world.df[world.df$region == "Syria",],10),4)

## playing with Orientation
worldmap.map3 <- ggplot() + 
  #geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), size = 0.3, fill="#E2C083", colour = "white") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(30, 30, 0))+
  theme_map()
worldmap.map3


#rcols <- terrain.colors(length(unique(s$REGION)))
#s$col <- as.numeric(factor(s$REGION)) par(bg = 'lightblue')
#plot(s, col = rcols[s$col], xlim = c(-180, 180)) 



######################################################################
##########
######################################################################



## Import data

# On this page, each row of data represents the information about UNHCR's populations of concern
# for a given year and country of residence and/or origin. Data is presented as a yearly 
# time series across the page. In the 2014 data, figures between 1 and 4 have been replaced 
# with an asterisk (*). These represent situations where the figures are being kept confidential 
# to protect the anonymity of individuals.
# Such figures are not included in any totals. 

data <- read.csv("data/unhcr_popstats_export_time_series_all_data.csv", comment.char="#",skip=3, header=TRUE, as.is=TRUE, fileEncoding="UTF8")
#names(data)
names(data)[2] <- "Asylum"
data$Value <- as.numeric(data$Value)
data$Year <- as.factor(data$Year)

ctry <- as.data.frame(unique(data$Asylum))

ctry2 <- as.data.frame(unique(data$Origin))
names(ctry)[1] <- "ctry"
names(ctry2)[1] <- "ctry"

ctryall <- unique(rbind(ctry, ctry2))
ctryall$ctry2 <- ctryall$ctry
#crtyall <- order(crtyall)

### Rewrite country name to get the centroid

ctryall$ctry <- as.character(ctryall$ctry)
ctryall$ctry[ctryall$ctry=="Bolivia (Plurinational State of)"] <-"Bolivia, Plurinational State of" 
ctryall$ctry[ctryall$ctry=="Bonaire"] <-"Bonaire, Sint Eustatius and Saba" 
ctryall$ctry[ctryall$ctry=="British Virgin Islands"] <-"Virgin Islands, British" 
ctryall$ctry[ctryall$ctry=="Cabo Verde"] <-"Cape Verde" 
ctryall$ctry[ctryall$ctry=="Central African Rep."] <-"Central African Republic" 
ctryall$ctry[ctryall$ctry=="China, Hong Kong SAR"] <-"Hong Kong" 
ctryall$ctry[ctryall$ctry=="China, Macao SAR"] <-"Macao" 
ctryall$ctry[ctryall$ctry=="Côte d'Ivoire"] <-"Cote d'Ivoire" 
ctryall$ctry[ctryall$ctry=="Curaçao"] <-"Curacao" 
ctryall$ctry[ctryall$ctry=="Czech Rep."] <-"Czech Republic" 
ctryall$ctry[ctryall$ctry=="Dem. Rep. of the Congo"] <-"Congo, The Democratic Republic of the" 
ctryall$ctry[ctryall$ctry=="Dominican Rep."] <-"Dominican Republic" 
ctryall$ctry[ctryall$ctry=="Iran (Islamic Rep. of)"] <-"Iran, Islamic Republic of" 
ctryall$ctry[ctryall$ctry=="Lao People's Dem. Rep."] <-"Lao People's Democratic Republic" 
ctryall$ctry[ctryall$ctry=="Libya"] <-"Libyan Arab Jamahiriya" 
ctryall$ctry[ctryall$ctry=="Micronesia (Federated States of)"] <-"Micronesia, Federated States of" 
ctryall$ctry[ctryall$ctry=="Rep. of Korea"] <-"Korea, Republic of" 
ctryall$ctry[ctryall$ctry=="Rep. of Moldova"] <-"Moldova, Republic of" 
ctryall$ctry[ctryall$ctry=="Saint Vincent and the Grenadines"] <-"Saint Vincent and The Grenadines" 
ctryall$ctry[ctryall$ctry=="Serbia and Kosovo (S/RES/1244 (1999))"] <-"Serbia" 
ctryall$ctry[ctryall$ctry=="State of Palestine"] <-"Occupied Palestinian Territory" 
ctryall$ctry[ctryall$ctry=="Syrian Arab Rep."] <-"Syrian Arab Republic" 
ctryall$ctry[ctryall$ctry=="The former Yugoslav Republic of Macedonia"] <-"Macedonia, The Former Yugoslav Republic of" 
ctryall$ctry[ctryall$ctry=="United Rep. of Tanzania"] <-"Tanzania, United Republic of" 
ctryall$ctry[ctryall$ctry=="United States of America"] <-"United States" 
## ctryall$ctry[ctryall$ctry=="Various/Unknown"] <-" " 
ctryall$ctry[ctryall$ctry=="Venezuela (Bolivarian Republic of)"] <-"Venezuela, Bolivarian Republic of" 
ctryall$ctry[ctryall$ctry=="Dem. People's Rep. of Korea"] <-"Korea, Democratic People's Republic of" 
ctryall$ctry[ctryall$ctry=="Holy See (the)"] <-"Holy See (Vatican City State)" 
ctryall$ctry[ctryall$ctry=="Palestinian"] <-"Occupied Palestinian Territory" 
ctryall$ctry[ctryall$ctry=="Réunion"] <-"Reunion" 
ctryall$ctry[ctryall$ctry=="Saint-Pierre-et-Miquelon"] <-"Saint Pierre and Miquelon" 
## ctryall$ctry[ctryall$ctry=="Stateless"] <-" " 
## ctryall$ctry[ctryall$ctry=="Tibetan"] <-" " 
ctryall$ctry[ctryall$ctry=="US Virgin Islands"] <-"Virgin Islands, U.S." 
ctryall$ctry[ctryall$ctry=="Wallis and Futuna Islands "] <-"Wallis and Futuna" 

#library(countrycode)
#countrycode_data <- countrycode_data

## http://opengeocode.org/download
## in order to get "latitude" & "longitude" for the centroid of the country
cow <- read.csv("data/cow.txt",skip=28,  encoding="UTF-8", sep=";", comment.char="#")
names(cow)
#str(cow)

# returns string w/o leading whitespace
#trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o trailing whitespace
#trim.trailing <- function (x) sub("\\s+$", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

cow$ISOen_name <- trim(cow$ISOen_name)

#str(ctryall)
ctryall <- merge(x=ctryall , y=cow , by.x="ctry", by.y="ISOen_name" , all.x=TRUE )

## Eliminate record without 
ctryall <-ctryall[!rowSums(is.na(ctryall["latitude"])), ]
ctryall <-ctryall[!rowSums(is.na(ctryall["longitude"])), ]

ctryall <- ctryall[ ,c("ctry2","latitude","longitude")]

#write.csv(ctryall, "data/ctryall.csv")

### Subsetting the dataset
#data <- databackp
databackp <- data

data <-data[!rowSums(is.na(data["Origin"])), ]
data <-data[!rowSums(is.na(data["Asylum"])), ]
data <-data[!rowSums(is.na(data["Year"])), ]
data <-data[!rowSums(is.na(data["Value"])), ]


## Some subsetting on the dataset
#levels(as.factor(data$Population.type))

## only arcs representing more than 5000 indivuals
data <- data[   (data$Value>5000) , ]

## only arcs representing refugees
data <- data[   (data$Population.type=="Refugees (incl. refugee-like situations)") , ]

## taking out locations that do not match countries or territories
data <- data[   !(data$Asylum %in% c("","Various/Unknown","Stateless","Tibetan") ) , ]
data <- data[    !(data$Origin %in% c("","Various/Unknown","Stateless","Tibetan") ) , ]
#str(data)

# add latlons
data.coord <- merge(data, ctryall, by.x , by.x="Asylum", by.y="ctry2" , all.x=TRUE  )
data.coord <- merge(data.coord, ctryall, by.x , by.x="Origin", by.y="ctry2" , all.x=TRUE  )

names(data.coord)

# calculate routes -- Dateline Break FALSE, otherwise we get a bump in the shifted ggplots
rts <- gcIntermediate(data.coord[,c('longitude.x', 'latitude.x')], data.coord[,c('longitude.y', 'latitude.y')], 
                      n=100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)


# http://docs.ggplot2.org/0.9.3.1/fortify.map.html
rts.ff1  <- ldply(rts@lines, fortify)


## Revised function to avoid a bug
source("code/fortify-spatial.R")
rts.ff <- fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot

#### merging back with Value for each arc

data.coord$id <-as.character(c(1:nrow(data.coord))) # that rts.ff$id is a char
gcircles <- merge(rts.ff, data.coord, all.x=T, by="id") # join attributes, we keep them all, just in case



###############################################################
# Plotting the map with Ggplot2

map1 <- ggplot() +
  geom_polygon(aes(long,lat,group=group), size = 0.3, fill="#191919", colour = "grey65", data=worldmap) +
  # set transparency here
  geom_line(aes(long,lat,group=group, color=Value, alpha=Value ), size=0.4, data= gcircles) +        
  # set color gradient here
  scale_colour_gradient(low="#fee8c8", high="#e34a33") +                                                             
  # set theme as organised above
  theme_map() +
# ylim(-60, 90) +
# coord_equal() 
#  coord_cartesian(ylim =c(-45, 70), xlim=c(-165, 165)) +
  coord_equal(ylim =c(-45, 70), xlim=c(-165, 165)) +
  # Adding points asylum - Origin with different color 
  geom_point(aes(longitude.x, latitude.x),data=data.coord, alpha = 0.8, size = 1, colour = "blue") +
  geom_point(aes(longitude.y, latitude.y),data=data.coord, alpha = 0.8, size = 1, colour = "red")

ggsave("out/map1.png", map1, width=10, height=6, units="in", dpi=600)



## playing with Orientation
map2 <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), size = 0.3, fill="#191919", colour = "grey65") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(30, 30, 0))+
  theme_map()+ 
  geom_line(aes(long,lat,group=group, color=Value, alpha=Value ), size=0.4, data= gcircles) +    
  scale_colour_gradient(low="#fee8c8", high="#e34a33") + 
  geom_point(aes(longitude.x, latitude.x),data=data.coord, alpha = 0.8, size = 1, colour = "blue") +
  geom_point(aes(longitude.y, latitude.y),data=data.coord, alpha = 0.8, size = 1, colour = "red")

ggsave("out/map2.png", map2, width=10, height=6,units="in", dpi=600)









