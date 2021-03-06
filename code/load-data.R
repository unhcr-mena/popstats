
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
str(data$Value)
#data <- data[(data$Value>5000) , ]



## Classify Values
library(classInt)
data$Value.class <- as.factor(findCols(classIntervals(data$Value, n = 6, style = "fixed",
                                                      fixedBreaks = c(0, 500, 5000, 10000, 50000, 10000000))))

summary(data$Value.class)

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
