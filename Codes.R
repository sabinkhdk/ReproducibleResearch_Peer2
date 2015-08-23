library(ggplot2)
library(dplyr)
library(reshape2)
# Read files
data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"), header=T, sep=",")
# columns of interest
col_interest <- which(colnames(data) %in% c("BGN_DATE", "STATE", "EVTYPE", "INJURIES", "FATALITIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP") )
# subset
data <- data[,col_interest]
# Get year data were collected
data$year = as.numeric(format(as.Date(data$BGN_DATE, format = "%m/%d/%Y"), "%Y"))
# See data distribution by year
nyear <- data$year
n_obs.year <- as.data.frame(table(nyear))
# Looking at table choose dataset with year > 1990
data1 <- subset(data, data$year > 1988)
eventsTYPE <- unique(data$EVTYPE)
evTABLE <- matrix(unlist(eventsTYPE), ncol = 5, byrow = TRUE)
totatEventType <- length(eventsTYPE)
# Check pattern and recategorize event types to few common ones
ptrn1 <- grepl("burst|depression|drizzle|fog|precipitation|percip|rain|wet|wall cloud", 
               data1$EVTYPE, ignore.case = TRUE)
ptrn2 <- grepl("hail", data1$EVTYPE, ignore.case = TRUE)
ptrn3 <- grepl("hurricane|typhoon|storm|wind|wnd", data1$EVTYPE, ignore.case = TRUE)
ptrn4 <- grepl("erosion|slide|slump", data1$EVTYPE, ignore.case = TRUE)
ptrn5 <- grepl("dry|drought|heat|hot|record temperature|record high|thermia|temperature record|warmth|warm", 
               data1$EVTYPE, ignore.case = TRUE)
ptrn6 <- grepl("avalanche|blizzard|chill|cold|cool|ice|frost|freeze|freezing|glaze|icy|snow|sleet|winter|wintry|wintery", 
               data1$EVTYPE, ignore.case = TRUE)
ptrn7 <- grepl("dam break|flood|fld|surf|swells", data1$EVTYPE, ignore.case = TRUE)
ptrn8 <- grepl("current|drowning|high water|marine|seas|tide|tsunami|wave", data1$EVTYPE, ignore.case = TRUE)
ptrn9 <- grepl("dust|saharan", data1$EVTYPE, ignore.case = TRUE)
ptrn10 <- grepl("lightning|tstm|thunderstorm", data1$EVTYPE, ignore.case = TRUE)
ptrn11 <- grepl("funnel|spout|tornado|whirlwind", data1$EVTYPE, ignore.case = TRUE)
ptrn12 <- grepl("fire|smoke", data1$EVTYPE, ignore.case = TRUE)
ptrn13 <- grepl("volcanic", data1$EVTYPE, ignore.case = TRUE)
# Assign new event code
data1$NewEVCODE <- NA       
data1$NewEVCODE[ptrn1] <- "Rain-related"
data1$NewEVCODE[ptrn2] <- "Hail"
data1$NewEVCODE[ptrn3] <- "Wind-related"
data1$NewEVCODE[ptrn4] <- "Soil-related"
data1$NewEVCODE[ptrn5] <- "Heat-related"
data1$NewEVCODE[ptrn6] <- "Snow/Ice"
data1$NewEVCODE[ptrn7] <- "Flood/Water-related"
data1$NewEVCODE[ptrn8] <- "Sea-related"
data1$NewEVCODE[ptrn9] <- "Dust/Saharan winds"  
data1$NewEVCODE[ptrn10] <- "Thunderstorm/Lightning"
data1$NewEVCODE[ptrn11] <- "Tornado"
data1$NewEVCODE[ptrn12] <- "Fire-related"
data1$NewEVCODE[ptrn13] <- "Volcano"
data1$NewEVCODE <- as.factor(data1$NewEVCODE)

convertStr2power <- function(x) {
    if (is.numeric(x)) {
        x <- x
    }
    else if (grepl("h|H", x, ignore.case=T)) {
        x <- 2
    }
    else if (grepl("k|K", x, ignore.case=T)) {
        x <- 3
    }
    else if (grepl("m|M", x, ignore.case=T)) {
        x <- 6
    }
    else if (grepl("b|B", x, ignore.case=T)) {
        x <- 9
    }
    else {
        x <- 0
    }
}

# Convert function
Convert_amt <- function(amt, power){
    power1 <- convertStr2power(power)
    if(is.numeric(amt)){
        tot_amt <- amt * 10^power1
    }
    else if(!is.numeric(amt)){
        tot_amt <- 0
    }
    tot_amt
}

addTWO <- function(x,y){
    tot <- x+y
    tot
}
##
data1$PROP_DMG_amt <- 0
data1$CROP_DMG_amt <- 0
data1$Total_DMG_amt <- 0

##
data1$PROP_DMG_amt <- mapply(Convert_amt, data1$PROPDMG, data1$PROPDMGEXP)
data1$CROP_DMG_amt <- mapply(Convert_amt, data1$CROPDMG, data1$CROPDMGEXP)
data1$Total_DMG_amt <- mapply(addTWO, data1$PROP_DMG_amt, data1$CROP_DMG_amt)

##
Fatal_summary <- aggregate(data1$FATALITIES, by=list(data1$NewEVCODE), FUN=sum)
Injury_summary <- aggregate(data1$INJURIES, by=list(data1$NewEVCODE), FUN=sum)
Amount_summary <- aggregate(data1$Total_DMG_amt, by=list(data1$NewEVCODE), FUN=sum)
# Merge both
merge_result <- Fatal_summary
colnames(merge_result) <- c("Event_Types","Total_Fatalities")
merge_result[, "Total_Injury"] <- Injury_summary$x
merge_result[, "Total_Damage_amt"] <- Amount_summary$x
# qplot(x=Event_Types, y=Total_Damage_amt, data=merge_result, geom="bar", stat="identity", fill="#DD8888") + guides(fill=FALSE)
# For health related questions
data2 <- merge_result[,1:3]
mdata2 <- melt(data2)
png("./figure1.png")
p1 <- ggplot(data=mdata2, aes(x=Event_Types, y=value, fill=variable)) + geom_bar(stat="identity") +
    theme(axis.text.x = element_text(size=10, face = "bold",angle = 45, hjust =1))
p1 <- p1 +ggtitle("Total Health related ") + xlab("Event Types") + ylab("Health related Injuries/Fatalities")
print(p1)
plot(p1)
dev.off()
png("./figure2.png")
p2 <- ggplot(data=merge_result, aes(x=Event_Types, y=Total_Damage_amt)) + geom_bar(fill="#DD8888",color="red",stat="identity") +
    theme(axis.text.x = element_text(size=10, face = "bold",angle = 45, hjust =1))
p2 <- p2 +ggtitle("Total loss in $") + xlab("Event Types") + ylab("Loss in $")
plot(p2)
print(p2)
dev.off()
# library(gridExtra)
# grid.arrange(p1,p2, ncol=2)
