library(dplyr)
storm_data_file <- read.csv("repdata_data_StormData.csv")
head(storm_data_file)
dim(storm_data_file)
library(dplyr)
storm_data <- storm_data_file[ , c(8, 23:28)]
rm(storm_data_file)
head(storm_data)
summary(storm_data$FATALITIES)
summary(storm_data$INJURIES)
total_injuries <- aggregate(INJURIES~EVTYPE, storm_data, sum)
total_injuries <- arrange(total_injuries, desc(INJURIES))
total_injuries <- total_injuries[1:20, ]
total_injuries
total_fatalities <- aggregate(FATALITIES~EVTYPE, storm_data, sum)
total_fatalities <- arrange(total_fatalities, desc(FATALITIES))
total_fatalities <- total_fatalities[1:20, ]
total_fatalities
totals<- merge(total_fatalities, total_injuries, by.x = "EVTYPE", by.y = "EVTYPE")
totals<-arrange(totals,desc(FATALITIES+INJURIES))
names_events <- totals$EVTYPE
barplot(t(totals[,-1]), names.arg = names_events, ylim = c(0,95000), beside = T, cex.names = 0.8, las=2, col = c("light blue", "pink"), main="Top Disaster Casualties")
legend("topright",c("Fatalities","Injuries"),fill=c("light blue","pink"),bty = "n")
storm_data$PROPDAMAGE = 0
storm_data[storm_data$PROPDMGEXP == "H", ]$PROPDAMAGE = storm_data[storm_data$PROPDMGEXP == "H", ]$PROPDMG * 10^2
storm_data[storm_data$PROPDMGEXP == "K", ]$PROPDAMAGE = storm_data[storm_data$PROPDMGEXP == "K", ]$PROPDMG * 10^3
storm_data[storm_data$PROPDMGEXP == "M", ]$PROPDAMAGE = storm_data[storm_data$PROPDMGEXP == "M", ]$PROPDMG * 10^6
storm_data[storm_data$PROPDMGEXP == "B", ]$PROPDAMAGE = storm_data[storm_data$PROPDMGEXP == "B", ]$PROPDMG * 10^9

storm_data$CROPDAMAGE = 0
storm_data[storm_data$CROPDMGEXP == "H", ]$CROPDAMAGE = storm_data[storm_data$CROPDMGEXP == "H", ]$CROPDMG * 10^2
storm_data[storm_data$CROPDMGEXP == "K", ]$CROPDAMAGE = storm_data[storm_data$CROPDMGEXP == "K", ]$CROPDMG * 10^3
storm_data[storm_data$CROPDMGEXP == "M", ]$CROPDAMAGE = storm_data[storm_data$CROPDMGEXP == "M", ]$CROPDMG * 10^6
storm_data[storm_data$CROPDMGEXP == "B", ]$CROPDAMAGE = storm_data[storm_data$CROPDMGEXP == "B", ]$CROPDMG * 10^9
economic_damage <- aggregate(PROPDAMAGE + CROPDAMAGE ~ EVTYPE, storm_data, sum)
names(economic_damage) = c("EVENT_TYPE", "TOTAL_DAMAGE")
economic_damage <- arrange(economic_damage, desc(TOTAL_DAMAGE))
economic_damage <- economic_damage[1:20, ]
economic_damage$TOTAL_DAMAGE <- economic_damage$TOTAL_DAMAGE/10^9
economic_damage$EVENT_TYPE <- factor(economic_damage$EVENT_TYPE, levels = economic_damage$EVENT_TYPE)
head(economic_damage)
with(economic_damage, barplot(TOTAL_DAMAGE, names.arg = EVENT_TYPE, beside = T, cex.names = 0.8, las=2, col = "light green", main = "Total Property and Crop Damage by Top 20 Event Types", ylab = "Total Damage in USD (10^9)"))
