#
setwd("C:/Users/Tom_Anichini/Documents/GitHub/Reproducible_2")
stormData <- read.csv("repdata-data-StormData.csv.bz2")

# Let us consider "harmful to population health" to include FATALITIES and 
# INJURIES

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
