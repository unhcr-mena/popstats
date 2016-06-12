#install.packages("geosphere")
#install.packages("maps")

library(maps)
library(geosphere)


#map("world")



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

#write.csv(ctryall, "data/ctryall.csv")

### Subsetting the dataset
data <- databackp
databackp <- data

data <-data[!rowSums(is.na(data["Origin"])), ]
data <-data[!rowSums(is.na(data["Asylum"])), ]
data <-data[!rowSums(is.na(data["Year"])), ]
data <-data[!rowSums(is.na(data["Value"])), ]


##
#levels(as.factor(data$Population.type))
data <- data[   (data$Value>5000) , ]
data <- data[   (data$Population.type=="Refugees (incl. refugee-like situations)") , ]
data <- data[   !(data$Asylum %in% c("","Various/Unknown","Stateless","Tibetan") ) , ]
data <- data[    !(data$Origin %in% c("","Various/Unknown","Stateless","Tibetan") ) , ]
#str(data)

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




