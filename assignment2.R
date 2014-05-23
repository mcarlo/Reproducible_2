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

casualties <- read.csv("cas.csv")
# Isolate cases with fatalities and injuries
casualties <- stormData[stormData$FATALITIES > 1 | stormData$INJURIES > 1,  ]
write.csv(casualties, "cas.csv")

# The list of Event Types is all over the place. Let us clean up the grouping 
# with a new variable, CAT, for category

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

unhealthy <- table(casualties$CAT)

fatalities <- tapply(casualties$FATALITIES,casualties$CAT,sum)
injuries <- tapply(casualties$INJURIES,casualties$CAT,sum)

danger <- data.frame(cbind(fatalities, injuries, unhealthy))
colnames(danger) <- c("Fatalities", "Injuries", "Events")
danger$Category <- rownames(danger)

require(ggplot2)
library(scales)

danger2 <- danger[danger$Fatalities >= 10 & danger$Injuries >= 10,]

ggplot(danger2, aes(x = Injuries, y = Fatalities, size = Events, 
                   label = Category), guide = FALSE) + 
         geom_point(colour = "white", fill = "red", shape = 21) + 
         scale_area(range = c(1,25), name = "#Events", labels = comma) + 
         scale_x_continuous(trans = "log2", name = "Total Injuries (log2 scale)", limits = c(2, 2^17), labels = comma) +
         scale_y_continuous(trans = "log2", name = "Total Fatalities (log2 scale)", limits = c(2, 2^15), labels = comma) + 
         geom_text(size = 5) + theme_bw() + 
  ggtitle("Most Dangerous Weather Events")

write.csv(damages, "damages.csv")


# Let us consider 
colnames(stormData)

harmEcon <- stormData[stormData$PROPDMG > 0 | stormData$CROPDMG > 0,c(8,25:28)]
table(harmEcon$PROPDMGEXP)
table(harmEcon$CROPDMGEXP)
tapply(harmEcon$CROPDMG,harmEcon$CROPDMGEXP,median)

tapply(harmEcon$PROPDMG,harmEcon$EVTYPE,sum)[order(-tapply(harmEcon$PROPDMG,harmEcon$EVTYPE,sum))][1:10]

harmEcon$CAT <- harmEcon$EVTYPE
harmEcon$CAT[grepl("TORNAD",harmEcon$CAT, ignore.case = T) ] <- "TORNADO"
harmEcon$CAT[grepl("TSTM",harmEcon$CAT, ignore.case = T)  ] <- "THUNDER"
harmEcon$CAT[grepl("THUNDER",harmEcon$CAT, ignore.case = T)  ] <- "THUNDER"
harmEcon$CAT[grepl("FLOOD",harmEcon$CAT, ignore.case = T)  ] <- "FLOOD"
harmEcon$CAT[grepl("HEAT",harmEcon$CAT, ignore.case = T)  ] <- "HEAT"
harmEcon$CAT[grepl("WIND",harmEcon$CAT, ignore.case = T)  ] <- "WIND"
harmEcon$CAT[grepl("WINTER",harmEcon$CAT, ignore.case = T)  ] <- "SNOW"
harmEcon$CAT[grepl("BLIZZARD",harmEcon$CAT, ignore.case = T)  ] <- "SNOW"
harmEcon$CAT[grepl("FIRE",harmEcon$CAT, ignore.case = T)  ] <- "FIRE"
harmEcon$CAT[grepl("TIDE",harmEcon$CAT, ignore.case = T)  ] <- "TIDE"
harmEcon$CAT[grepl("CURRENT",harmEcon$CAT, ignore.case = T)  ] <- "TIDE"
harmEcon$CAT[grepl("RAIN",harmEcon$CAT, ignore.case = T)  ] <- "RAIN"
harmEcon$CAT[grepl("FOG",harmEcon$CAT, ignore.case = T)  ] <- "FOG"
harmEcon$CAT[grepl("HEAT",harmEcon$CAT, ignore.case = T)  ] <- "HEAT"
harmEcon$CAT[grepl("COLD",harmEcon$CAT, ignore.case = T)  ] <- "COLD"
harmEcon$CAT[grepl("SNOW",harmEcon$CAT, ignore.case = T)  ] <- "SNOW/ICE"
harmEcon$CAT[grepl("ICE",harmEcon$CAT, ignore.case = T)  ] <- "SNOW/ICE"
harmEcon$CAT[grepl("ICY",harmEcon$CAT, ignore.case = T)  ] <- "SNOW/ICE"
harmEcon$CAT[grepl("SURF",harmEcon$CAT, ignore.case = T)  ] <- "TIDE"
harmEcon$CAT[grepl("TROP",harmEcon$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
harmEcon$CAT[grepl("HURRIC",harmEcon$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
harmEcon$CAT[grepl("TYPHOON",harmEcon$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
harmEcon$CAT[grepl("TYPHOON",harmEcon$CAT, ignore.case = T)  ] <- "TROPICAL_STORM"
harmEcon$CAT[grepl("DUST",harmEcon$CAT, ignore.case = T)  ] <- "DUST"

harmEcon$PropertyDamage <- harmEcon$PROPDMG
harmEcon$PropertyDamage[harmEcon$PROPDMG %in% c("b","B")] <- harmEcon$PROPDMG*1e9
harmEcon$PropertyDamage[harmEcon$PROPDMG %in% c("m","M")] <- harmEcon$PROPDMG*1e6
harmEcon$PropertyDamage[harmEcon$PROPDMG %in% c("k","K")] <- harmEcon$PROPDMG*1e3


harmEcon$CropDamage <- harmEcon$CROPDMG
harmEcon$CropDamage[harmEcon$CROPDMG %in% c("b","B")] <- harmEcon$CROPDMG*1e9
harmEcon$CropDamage[harmEcon$CROPDMG %in% c("m","M")] <- harmEcon$CROPDMG*1e6
harmEcon$CropDamage[harmEcon$CROPDMG %in% c("k","K")] <- harmEcon$CROPDMG*1e3

harmful <- table(harmEcon$CAT)

propHarm <- tapply(harmEcon$PropertyDamage,harmEcon$CAT,sum)
cropHarm <- tapply(harmEcon$CropDamage,harmEcon$CAT,sum)

harm <- data.frame(cbind(propHarm, cropHarm, harmful))
colnames(harm) <- c("Property", "Crops", "Events")
harm$Category <- rownames(harmful)
head(harm)

log2(max(harm$Crops))

danger2

harm2 <- harm[harm$Property >10 & harm$Crops > 10,]

ggplot(harm2, aes(x = Property, y = Crops, size = Events, 
                    label = Category), guide = FALSE) + 
  geom_point(colour = "white", fill = "red", shape = 21)+ 
  scale_area(range = c(1,25), name = "#Events", labels = comma) +  
  scale_x_continuous(trans = "log2", name = "Property Damage, $Nominal (log2 scale)", limits = c(2, 2^22), labels = comma) +
  scale_y_continuous(trans = "log2", name = "Total Crop Damage $Nominal (log2 scale)", limits = c(2, 2^20), labels = comma) + 
  geom_text(size = 4) + theme_bw() +
  ggtitle("Most Harmful Weather Events")
