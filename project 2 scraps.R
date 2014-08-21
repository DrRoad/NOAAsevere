setwd("C:/Users/Seth/Documents/Coursera/Reproducible Research/project 2")

# STARTUP TRAIN
stormData <- read.csv("StormData.csv")
stormData$BGN_DATE <- as.character(stormData$BGN_DATE)
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, "%m/%d/%Y")
stormData <- stormData[order(stormData$BGN_DATE),]
sd <- stormData[54960:902297,]
sdp <- sd[, c(2, 8, 25:28)]
sdh <- sd[, c(2, 8, 23, 24)]
sdp$CROPDMGEXP <- toupper(as.character(sdp$CROPDMGEXP))
sdp$PROPDMGEXP <- toupper(as.character(sdp$PROPDMGEXP))
sdp <- sdp[-grep("[^K|M|B]", sdp$CROPDMGEXP),]
sdp <- sdp[-grep("[^K|M|B]", sdp$PROPDMGEXP),]


download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2")
library(R.utils)
bunzip2("StormData.csv.bz2")
stormData <- read.csv("StormData.csv")


stormData$BGN_DATE <- as.character(stormData$BGN_DATE)
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, "%m/%d/%Y")
stormData <- stormData[order(stormData$BGN_DATE),]


hist(stormData$BGN_DATE, breaks = 52, freq= TRUE, main = "Severe Weather Events Reported Per Year", xlab = "Year", ylab="Number of Events")

hist(stormData$BGN_DATE, breaks = 72, freq= TRUE, main = "Severe Weather Events Reported Per Year", xlab = "Year", ylab="Number of Events", ylim=c(0,3000))

hist(stormData$BGN_DATE[1:100000], breaks = 40, freq= TRUE, main = "Severe Weather Events Reported Per Year", xlab = "Year", ylab="Number of Events Reported", ylim=c(0,6000))

sdp <- sdp[order(sdp$BGN_DATE),]
sep <- grep("1968", sdp$BGN_DATE)

se <- grep("1968", stormData$BGN_DATE)
se[1]

sd <- stormData[1622:10484,]

sdp <- sd[, c(2, 8, 25:28)]
sdh <- sd[, c(2, 8, 23, 24)]

str(sdp)
!is.na(sdp$CROPDMGEXP)

length(grep("1968", stormData$BGN_DATE))

##novelty
sd[sdh$FATALITIES==max(sdh$FATALITIES),]
sd[sdh$INJURIES==max(sdh$INJURIES),]

sdp$CROPDMGEXP <- toupper(as.character(sdp$CROPDMGEXP))
sdp$PROPDMGEXP <- toupper(as.character(sdp$PROPDMGEXP))
OR
toupper(levels(sdp$CROPDMGEXP))
toupper(levels(sdp$PROPDMGEXP))

## did the as.character, tried this, didn't work
sdpp <- sdp[grep(" |K|M|B", sdp$PROPDMGEXP),]

sdpp <- sdp[!grep("-|", sdp$PROPDMGEXP),]

##WINNER WINNER (after the as.character)
sdpp <- sdp[-grep("[^K|M|B]", sdp$PROPDMGEXP),]

##CHICKEN DINNER
sdp <- sdp[-grep("[^K|M|B]", sdp$CROPDMGEXP),]
sdp <- sdp[-grep("[^K|M|B]", sdp$PROPDMGEXP),]

## if needed (don't think so)
sdp$CROPDMGEXP <- as.factor(sdp$CROPDMGEXP)
sdp$PROPDMGEXP <- as.factor(sdp$PROPDMGEXP)

### multiplication time
sdp25 <- sdp[20005:20030,]

for(i in 1:length(sdp25$PROPDMGEXP)) {
    if(sdp25$PROPDMGEXP[i]=="K"){
        sdp25$PROPDMG[i]*1000
    } else if(sdp25$PROPDMGEXP[i]=="M"){
        sdp25$PROPDMG[i]*1000000
    } else if(sdp25$PROPDMGEXP[i]=="B"){
        sdp25$PROPDMG[i]*1000000000
    }    
}

###the key
for(i in 1:length(sdp25$PROPDMGEXP)) {
    sdp25$PROPDMG[i] <- sdp25$PROPDMG[i]*2   
}

###Winner!!! but for sdp25
for(i in 1:length(sdp25$PROPDMGEXP)) {
    if(sdp25$PROPDMGEXP[i]=="K"){
        sdp25$PROPDMG[i] <- sdp25$PROPDMG[i]*1000
    } else if(sdp25$PROPDMGEXP[i]=="M"){
        sdp25$PROPDMG[i] <- sdp25$PROPDMG[i]*1000000
    } else if(sdp25$PROPDMGEXP[i]=="B"){
        sdp25$PROPDMG[i] <- sdp25$PROPDMG[i]*1000000000
    }    
}

### real deal
for(i in 1:length(sdp$PROPDMGEXP)) {
    if(sdp$PROPDMGEXP[i]=="K"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000
    } else if(sdp$PROPDMGEXP[i]=="M"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000000
    } else if(sdp$PROPDMGEXP[i]=="B"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000000000
    }    
}

for(i in 1:length(sdp$CROPDMGEXP)) {
    if(sdp$CROPDMGEXP[i]=="K"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000
    } else if(sdp$CROPDMGEXP[i]=="M"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000
    } else if(sdp$CROPDMGEXP[i]=="B"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000000
    }    
}
