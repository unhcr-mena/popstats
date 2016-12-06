source("code/load-data.R")


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

library(gganimate)  # https://github.com/dgrtwo/gganimate

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
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length   =   unit(0,"cm"),
      panel.grid.major= element_line(colour="black", size=0.3, linetype=1),
     # panel.grid.minor=element_blank(),
     # panel.grid      =   element_blank(),
      panel.background = element_rect(fill = "white", colour=NA),
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
  coord_map("ortho", orientation=c(30, 30, 10))+
  theme_map()
worldmap.map3

#rcols <- terrain.colors(length(unique(s$REGION)))
#s$col <- as.numeric(factor(s$REGION)) par(bg = 'lightblue')
#plot(s, col = rcols[s$col], xlim = c(-180, 180)) 



######################################################################
########## Create curved routes
######################################################################

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
# Plotting maps with Ggplot2
###############################################################
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
map1
ggsave("out/map1.png", map1, width=10, height=6, units="in", dpi=600)



## playing with Orientation
map2 <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), size = 0.3, fill="#191919", colour = "grey65") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(30, 30, 0))+
  theme_map()+ 
  geom_line(aes(long,lat,group=group, color=Value, alpha=0.9 ), size=0.4, data= gcircles[ gcircles$mena.x=="Syria Situation", ]) +    
  scale_colour_gradient(low="#fee8c8", high="#e34a33") + 
  geom_point(aes(longitude.x, latitude.x),data=data.coord[ data.coord$mena.x=="Syria Situation", ], alpha = 0.8, size = 1, colour = "blue") +
  geom_point(aes(longitude.y, latitude.y),data=data.coord[ data.coord$mena.x=="Syria Situation", ], alpha = 0.8, size = 1, colour = "red")

ggsave("out/map2.png", map2, width=10, height=6,units="in", dpi=600)


##################################################################
## Animation of
#names(data.coord)

frame.to.animate <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), size = 0.3, fill="#bbdaa4", colour = "grey65") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(30, 30, 0))+
  theme_map()+ 
  geom_line(aes(x=long,y=lat,group=group,  color="blue", alpha=0.9, frame = Year),size=Value, data= gcircles) +    
  scale_colour_gradient(low="#fee8c8", high="#e34a33") + 
  geom_point(aes(x=longitude.x, y=latitude.x, frame = Year),data=data.coord, alpha = 0.8, size = 1, colour = "blue") +
  geom_point(aes(x=longitude.y, y=latitude.y, frame = Year),data=data.coord, alpha = 0.8, size = 1, colour = "red") +
  geom_text(data = data.coord, aes(x = 28.5, y = 1.5, frame = Year, label = data.coord$Year), color = 'white', size = 12) 

animated.map <- gg_animate(frame.to.animate, interval = 0.005, ani.width = 1200, ani.height = 900, title_frame = FALSE)
animated.map %>% gg_animate_save("out/animatedmap.gif")
