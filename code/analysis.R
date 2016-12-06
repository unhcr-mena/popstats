#install.packages("geosphere")
#install.packages("maps")

library(maps)
library(geosphere)


#map("world")



source("code/load-data.R")

#########################################################################3
##### Generate One PNG per year in a loop

# Unique year in that subset -- that's why need to be cast from factor  to character
years <- unique(as.character(data$Year))

# Color
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
colors <- pal(100)
## Bouding box for Middle East
#xlim <- c(7.8, 62.3)
#ylim <- c(12.8, 39.9)

xlim <- c(-180, 180)
ylim <- c(-80, 80)
#map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=1.5, xlim=xlim, ylim=ylim)

for (i in 1:length(years)) {
  png(paste("out/year", years[i], ".png", sep=""), 
      width = 1280, height = 680, units = "px", pointsize = 6,  bg = "transparent", type = "cairo-png")
  map("world", col="#191919", fill=TRUE, bg="#000000", lwd=1.5, xlim=xlim, ylim=ylim)
  fsub <- data[data$Year == years[i],]
  fsub <- fsub[order(fsub$Value),]
  maxcnt <- max(fsub$Value)
  for (j in 1:length(fsub$Year)) {
    Asylum <- ctryall[ctryall$ctry2 == fsub[j,]$Asylum,]
    Origin <- ctryall[ctryall$ctry2 == fsub[j,]$Origin,]
    
    inter <- gcIntermediate(c(Asylum[1,]$longitude, Asylum[1,]$latitude), c(Origin[1,]$longitude, Origin[1,]$latitude), n=100, addStartEnd=TRUE)
    colindex <- round( (fsub[j,]$Value / maxcnt) * length(colors) )
    
    lines(inter, col=colors[colindex], lwd=2.6)
  }
  
  dev.off()
}




