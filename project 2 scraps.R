setwd("C:/Users/Seth/Documents/Coursera/Reproducible Research/project 2")

# STARTUP TRAIN
stormData <- read.csv("StormData.csv")
stormData$BGN_DATE <- as.character(stormData$BGN_DATE)
stormData$BGN_DATE <- as.Date(stormData$BGN_DATE, "%m/%d/%Y")
stormData <- stormData[order(stormData$BGN_DATE),]
sd <- stormData[54960:902297,]
#EVTYPE
levels(sd$EVTYPE) <- tolower(levels(sd$EVTYPE))
sd$event <- "other"
sd$event[grep("lightning", sd$EVTYPE)] <- "lightning"
sd$event[grep("thunder|rain", sd$EVTYPE)] <- "heavy rain"
sd$event[grep("snow|blizzard|chill|wint|cold|sleet|ice|freez", sd$EVTYPE)] <- "winter storm"
sd$event[grep("hail", sd$EVTYPE)] <- "hail"
sd$event[grep("seas|surf|tide|swell", sd$EVTYPE)] <- "coastal event"
sd$event[grep("tornado|gustnado|funnel|spout", sd$EVTYPE)] <- "tornado"
sd$event[grep("fire|smoke", sd$EVTYPE)] <- "fire"
sd$event[grep("wind", sd$EVTYPE)] <- "heavy wind"
sd$event[grep("volcan", sd$EVTYPE)] <- "volcano"
sd$event[grep("flood|stream", sd$EVTYPE)] <- "flood"
sd$event[grep("heat|temp|hot", sd$EVTYPE)] <- "heat wave"
sd$event[grep("dry|drought", sd$EVTYPE)] <- "drought"
sd$event[grep("microburst", sd$EVTYPE)] <- "microburst"
sd$event[grep("hurricane|cyclone|surge", sd$EVTYPE)] <- "hurricane"
sd$event <- as.factor(sd$event)
#split
sdp <- sd[, c(2, 25:28, 38)]
sdh <- sd[, c(2, 23, 24, 38)]
#EXP
sdp$CROPDMGEXP <- toupper(as.character(sdp$CROPDMGEXP))
sdp$PROPDMGEXP <- toupper(as.character(sdp$PROPDMGEXP))
sdp <- sdp[-grep("[^K|M|B]", sdp$CROPDMGEXP),]
sdp <- sdp[-grep("[^K|M|B]", sdp$PROPDMGEXP),]
sdp$propmult <- 1
sdp$propmult[sdp$PROPDMGEXP == "K"] <- 1000
sdp$propmult[sdp$PROPDMGEXP == "M"] <- 1000000
sdp$propmult[sdp$PROPDMGEXP == "B"] <- 1000000000
sdp$PROPDMG <- sdp$PROPDMG*sdp$propmult
sdp$cropmult <- 1
sdp$cropmult[sdp$CROPDMGEXP == "K"] <- 1000
sdp$cropmult[sdp$CROPDMGEXP == "M"] <- 1000000
sdp$cropmult[sdp$CROPDMGEXP == "B"] <- 1000000000
sdp$CROPDMG <- sdp$CROPDMG*sdp$cropmult
#totaldmg
sdp$totaldmg <- sdp$CROPDMG+sdp$PROPDMG
#napafix
sdp$totaldmg[sdp$totaldmg==115032500000] <- 115032500
#aggregate
sdpa <- sdp[,c("event", "totaldmg")]
sdpa <- aggregate(sdpa$totaldmg, list(sdpa$event), sum)
colnames(sdpa) <- c("event", "totaldmg")
sdpa <- sdpa[order(sdpa$totaldmg, decreasing=TRUE),]



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

## trying to split it up
for(i in 1:10000) {
    if(sdp$CROPDMGEXP[i]=="K"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000
    } else if(sdp$CROPDMGEXP[i]=="M"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000
    } else if(sdp$CROPDMGEXP[i]=="B"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000000
    }    
}

for(i in 130000:150000) {
    if(sdp$CROPDMGEXP[i]=="K"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000
    } else if(sdp$CROPDMGEXP[i]=="M"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000
    } else if(sdp$CROPDMGEXP[i]=="B"){
        sdp$CROPDMG[i] <- sdp$CROPDMG[i]*1000000000
    }    
}

### 25,000 TAKES ABOUT 20 SECONDS
### THEN 50,000 ONLY TOOK 20 SECONDS
### but then 100,000 took 5 minutes or so (but I was doing other stuff)

## Hood fix (OH FUCK YEA) sort of makes -grep unneccessary, but then you still don't know what the DMG is for those, so it's probably better to purge them anyway.
sdp$propmult <- 1
sdp$propmult[sdp$PROPDMGEXP == "K"] <- 1000
sdp$propmult[sdp$PROPDMGEXP == "M"] <- 1000000
sdp$propmult[sdp$PROPDMGEXP == "B"] <- 1000000000
sdp$PROPDMG <- sdp$PROPDMG*sdp$propmult

## need to run on real thing
sdp25 <- sdp[20005:20030,]
sdp25$PROPDMG <- sdp25$PROPDMG*sdp25$propmult


## EVTYPE fun
for(i in 1:length(sdp$EVTYPE)) {
    if(sdp$PROPDMGEXP[i]=="K"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000
    } else if(sdp$PROPDMGEXP[i]=="M"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000000
    } else if(sdp$PROPDMGEXP[i]=="B"){
        sdp$PROPDMG[i] <- sdp$PROPDMG[i]*1000000000
    }    
}

levels(sdh$EVTYPE) <- tolower(levels(sdh$EVTYPE))
levels(sdp$EVTYPE) <- tolower(levels(sdp$EVTYPE))


grepl("lightning", sdp$EVTYPE)
grepl("thunder|rain", sdp$EVYTPE)
grepl("snow|blizzard|chill|wint|cold|sleet|ice|freez", sdp$EVTYPE)
grepl("hail", sdp$EVTYPE)
grepl("seas|surf|tide|swell", sdp$EVTYPE)
grepl("hurricane", sdp$EVTYPE)
grepl("tornado|gustnado", sdp$EVTYPE)
grepl("fire|smoke", sdp$EVTYPE)
grepl("wind", sdp$EVTYPE)
grepl("volcan", sdp$EVTYPE)
grepl("flood|stream", sdp$EVTYPE)
grepl("heat|temp|hot", sdp$EVTYPE)
grepl("dry|drought", sdp$EVTYPE)

##didn't work
sdp25[grep("snow|blizzard|chill|wint|cold|sleet|ice|freez", sdp25$EVTYPE),]

## YES!!!
sdp25 <- sdp[20005:20030,]
sdp25$event <- "other"
sdp25$event[grep("wind", sdp25$EVTYPE)] <- "wind"
sdp25$event[grep("hail", sdp25$EVTYPE)] <- "hail"

## now testing
sd100$event <- "other"
sd100$event[grep("snow|blizzard|chill|wint|cold|sleet|ice|freez", sd100$EVTYPE)] <- "winter storm"
sd100$event[grep("wind", sd100$EVTYPE)] <- "wind"
sd100$event[grep("hail", sd100$EVTYPE)] <- "hail"
[,c(3,8,38)]

## now for real
sd$event <- "other"
sd$event[grep("lightning", sd$EVTYPE)] <- "lightning"
sd$event[grep("thunder|rain", sd$EVTYPE)] <- "heavy rain"
sd$event[grep("snow|blizzard|chill|wint|cold|sleet|ice|freez", sd$EVTYPE)] <- "winter storm"
sd$event[grep("hail", sd$EVTYPE)] <- "hail"
sd$event[grep("seas|surf|tide|swell", sd$EVTYPE)] <- "rough seas"
sd$event[grep("tornado|gustnado|funnel|spout", sd$EVTYPE)] <- "tornado"
sd$event[grep("fire|smoke", sd$EVTYPE)] <- "fire"
sd$event[grep("wind", sd$EVTYPE)] <- "heavy wind"
sd$event[grep("hurricane", sd$EVTYPE)] <- "hurricane"
sd$event[grep("volcan", sd$EVTYPE)] <- "volcano"
sd$event[grep("flood|stream", sd$EVTYPE)] <- "flood"
sd$event[grep("heat|temp|hot", sd$EVTYPE)] <- "heat wave"
sd$event[grep("dry|drought", sd$EVTYPE)] <- "drought"
sd$event[grep("microburst", sd$EVTYPE)] <- "microburst"
sd$event[grep("tropical", sd$EVTYPE)] <- "tropical storm"
sd$event[grep("hurricane", sd$EVTYPE)] <- "hurricane"

###total damage
sdp$totaldmg <- sdp$CROPDMG+sdp$PROPDMG
sdpa <- sdp[,c("event", "totaldmg")]
sdpa <- aggregate(sdpa$totaldmg, list(sdpa$event), sum)

###the crazy outlier
View(sdp[sdp$totaldmg==max(sdp$totaldmg),])

sdpo <- sdp
sdpo <- sdpo[order(sdpo$totaldmg, decreasing=TRUE),]
top10 <- sdpo[1:10,]
top10
plot(top10$BGN_DATE, top10$totaldmg)


Napa <- sd[sd$BGN_DATE == "2006-01-01",]
Napa <- Napa[23,]
Napa[,c(25:28, 36)]

##napafix
sdp$totaldmg[sdp$totaldmg==115032500000] <- 115032500

Katrina <- rbind(sd[sd$BGN_DATE == "2005-08-29",],sd[sd$BGN_DATE == "2005-08-28",])
Katrina <- Katrina[grep("hurricane|surge", Katrina$EVTYPE),]
Katrina[,c(2,7,8,25,26)]

Katrina <- rbind(sd[sd$BGN_DATE == "2005-08-29",],sd[sd$BGN_DATE == "2005-08-28",])
Katrina <- Katrina[grep("hurricane|surge", Katrina$EVTYPE),]
Katrina$propmult <- 1
Katrina$propmult[Katrina$PROPDMGEXP == "K"] <- 1000
Katrina$propmult[Katrina$PROPDMGEXP == "M"] <- 1000000
Katrina$propmult[Katrina$PROPDMGEXP == "B"] <- 1000000000
Katrina$PROPDMG <- Katrina$PROPDMG*Katrina$propmult
Katrina$cropmult <- 1
Katrina$cropmult[Katrina$CROPDMGEXP == "K"] <- 1000
Katrina$cropmult[Katrina$CROPDMGEXP == "M"] <- 1000000
Katrina$cropmult[Katrina$CROPDMGEXP == "B"] <- 1000000000
Katrina$CROPDMG <- Katrina$CROPDMG*Katrina$cropmult
#totaldmg
Katrina$totaldmg <- Katrina$CROPDMG+Katrina$PROPDMG
Katrina[,c(2,7,8,41)]

Ten <- sd[sd$BGN_DATE == "2001-06-05",]
Ten <- Ten[Ten$PROPDMGEXP == "B",]
Ten[,c(25:28, 36)]

surge <- sd[grep("surge", sd$EVTYPE),]

###works but needs some tweaking
plot(sdpa$event, sdpa$totaldmg/1000000000, las=3)

par(mar=c(7,3,3,1))
plot(sdpa$event, sdpa$totaldmg/1000000000, las=3)

## pretty good, need to add subtitle "By Type of Event, 1975-2011"
par(mar=c(7,4,3,4))
plot(sdpa$event, sdpa$totaldmg/1000000000, las=3, main="Total Damage from Severe Weather Events", ylab="Damage (in Billions of $)")

### sdh

sdh$totaldmg <- sdh$INJURIES+sdh$FATALITIES*3

sdho <- sdh
sdho <- sdho[order(sdho$totaldmg, decreasing=TRUE),]
top10h <- sdho[1:10,]
top10h

hurhealth <- sdh[grep("hurricane", sdh$event),]
hurhealth <- hurhealth[order(sdh$totaldmg),]
hurhealth <- hurhealth[order(hurhealth$totaldmg, decreasing=TRUE),]
hurhealth[1:10,]

par(mar=c(7,3,3,2))
plot(sdha$event, sdha$totaldmg, las=3, main="Population Health Damage \nfrom Severe Weather", ylab="Fatalities and Injuries", yaxt='n')

