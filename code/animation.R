source("code/load-data.R")


### http://robinlovelace.net/2014/06/22/great-circles-in-rworldmap-ggplot2.html

x <- c("rworldmap", "geosphere", "ggmap") lapply(x, require, character.only = T) 

#install.packages("geosphere")
#install.packages("maps")
#install.packages("rworldmap")
#install.packages("ggmap")

library(maps)
library(geosphere)
library(rworldmap)
library(geosphere)


rotateMap <- function(angle){
  worldmap + 
    #geom_path(data = circles, aes(x = lon, y = lat, group = dist, col = dist), linetype = 2) +
    coord_map("ortho", orientation=c(61, angle, 0)) 
}

library(animation)

saveGIF({
  ani.options(nmax = 360)
  for(i in seq(0,360)){
    print(rotateMap(i))
  }
}, interval = 0.1, outdir="out/", movie.name = "roate.gif")


