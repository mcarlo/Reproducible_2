#
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
con <- unz(temp, "repdata-data-StormData.csv") #file we wish to read
data <- data.frame(scan(con),ncol=4,byrow=TRUE)
unlink(temp)

rownames(table(stormData$EVTYPE))



'List of events from NOAA: #http://www.ncdc.noaa.gov/stormevents/ftp.jsp
Astronomical Low Tide  Z 
Avalanche    Z 
Blizzard    Z 
Coastal Flood   Z 
Cold/Wind Chill   Z 
Debris Flow    C 
Dense Fog    Z 
Dense Smoke   Z 
Drought    Z 
Dust Devil    C 
Dust Storm    Z 
Excessive Heat   Z 
Extreme Cold/Wind Chill   Z 
Flash Flood    C   
Flood     C 
Frost/Freeze    Z 
Funnel Cloud   C 
Freezing Fog   Z 
Hail     C 
Heat      Z 
Heavy Rain    C 
Heavy Snow    Z 
High Surf    Z 
High Wind    Z 
Hurricane (Typhoon)   Z 
Ice Storm    Z 
Lake-Effect Snow   Z 
Lakeshore Flood   Z 
Lightning    C 
Marine Hail    M 
Marine High Wind   M 
Marine Strong Wind   M 
Marine Thunderstorm Wind   M  
Rip Current    Z 
Seiche     Z 
Sleet      Z 
Storm Surge/Tide   Z 
Strong Wind    Z 
Thunderstorm Wind   C 
Tornado    C 
Tropical Depression   Z 
Tropical Storm   Z 
Tsunami    Z 
Volcanic Ash   Z 
Waterspout    M 
Wildfire    Z 
Winter Storm   Z 
Winter Weather   Z '


#setwd("C:/Users/Tom_Anichini/Documents/GitHub/Reproducible_2")
setwd("~/GitHub/Reproducible_2")
stormData <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors = FALSE)
dim(stormData)
# Let us consider "harmful to population health" to include FATALITIES and 
# INJURIES

damages <- stormData[stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]


# Isolate cases with fatalities and injuries
casualties <- stormData[stormData$FATALITIES > 1 | stormData$INJURIES > 1,  ]
write.csv(casualties, "cas.csv")

casualties$CAT <- casualties$EVTYPE
casualties$CAT[grepl("TORNAD",casualties$CAT, ignore.case = T) ] <- "TORNADO"
casualties$CAT[grepl("TSTM",casualties$CAT, ignore.case = T)  ] <- "THUNDER"
casualties$CAT[grepl("THUNDER",casualties$CAT, ignore.case = T)  ] <- "THUNDER"
casualties$CAT[grepl("FLOOD",casualties$CAT, ignore.case = T)  ] <- "FLOOD"
casualties$CAT[grepl("HEAT",casualties$CAT, ignore.case = T)  ] <- "HEAT"
casualties$CAT[grepl("WIND",casualties$CAT, ignore.case = T)  ] <- "WIND"
casualties$CAT[grepl("WINTER",casualties$CAT, ignore.case = T)  ] <- "SNOW"
casualties$CAT[grepl("BLIZZARD",casualties$CAT, ignore.case = T)  ] <- "SNOW"
casualties$CAT[grepl("FIRE",casualties$CAT, ignore.case = T)  ] <- "FIRE"
casualties$CAT[grepl("TIDE",casualties$CAT, ignore.case = T)  ] <- "TIDE"
casualties$CAT[grepl("CURRENT",casualties$CAT, ignore.case = T)  ] <- "TIDE"
casualties$CAT[grepl("RAIN",casualties$CAT, ignore.case = T)  ] <- "RAIN"
casualties$CAT[grepl("FOG",casualties$CAT, ignore.case = T)  ] <- "FOG"
casualties$CAT[grepl("HEAT",casualties$CAT, ignore.case = T)  ] <- "HEAT"
casualties$CAT[grepl("COLD",casualties$CAT, ignore.case = T)  ] <- "COLD"
casualties$CAT[grepl("SNOW",casualties$CAT, ignore.case = T)  ] <- "SNOW/ICE"
casualties$CAT[grepl("ICE",casualties$CAT, ignore.case = T)  ] <- "SNOW/ICE"
casualties$CAT[grepl("ICY",casualties$CAT, ignore.case = T)  ] <- "SNOW/ICE"
casualties$CAT[grepl("SURF",casualties$CAT, ignore.case = T)  ] <- "TIDE"
casualties$CAT[grepl("TROP",casualties$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
casualties$CAT[grepl("HURRIC",casualties$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
casualties$CAT[grepl("TYPHOON",casualties$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
casualties$CAT[grepl("TYPHOON",casualties$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
casualties$CAT[grepl("DUST",casualties$CAT, ignore.case = T)  ] <- "DUST"

table(casualties$CAT)[order(-table(casualties$CAT))]



write.csv(damages, "damages.csv")

attach(stormData)
summary(stormData)

stormData[which(INJURIES > quantile(INJURIES,.99999)),]
?quantile
which(table(stormData$X.INJURIES.) == max(table(stormData$X.INJURIES.)))
str(stormData$X.INJURIES.)
intInjuries <- as.integer(stormData$X.INJURIES.)
intInjuries[is.na(intInjuries)] <- 0
hist(intInjuries)
stormData[1767126,]

which(intInjuries == max(intInjuries))
eventOrder <- order(-eventTypes)
eventTypes[eventOrder][1:100]

harmHealth <- stormData[,c(8,23:24)]

fatalMean <- tapply(harmHealth$FATALITIES,harmHealth$EVTYPE,mean)
fatalMedian <-  tapply(harmHealth$FATALITIES,harmHealth$EVTYPE,median)
fatalSum <- tapply(harmHealth$FATALITIES,harmHealth$EVTYPE,sum)
fatalCount <- tapply(1*(harmHealth$FATALITIES>0),harmHealth$EVTYPE,sum)
str(fatalCount[fatalCount > 0])
plot(fatalSum,rownames(fatalSum))
fatalSum
plot(x = fatalMedian[-order(fatalMedian)], y = fatalMean[-order(fatalMedian)], 
     xlab = "Median fatalities", ylab = "Mean fatalities")

summary(fatalMedian[fatalMean > 0 | fatalMedian > 0])
length(fatalSum[fatalSum > 0])

cor(order(fatalMean),order(fatalMedian))

injuryMean <- tapply(stormData$INJURIES,stormData$EVTYPE,mean)
injuryMedian <-  tapply(stormData$INJURIES,stormData$EVTYPE,median)

# Let us consider 
colnames(stormData)

harmEcon <- stormData[,c(8,25:28)]




summary(cbind(stormData$CROPDMGEXP, stormData$CROPDMG))
plot(x=stormData$CROPDMGEXP, y=stormData$CROPDMG)
