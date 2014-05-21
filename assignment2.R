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


setwd("C:/Users/Tom_Anichini/Documents/GitHub/Reproducible_2")
stormData <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors = FALSE)
dim(stormData)
# Let us consider "harmful to population health" to include FATALITIES and 
# INJURIES

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
