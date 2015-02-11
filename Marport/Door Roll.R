# Roll Investigations

ids = c('NED2014101.5', 'NED2014101.6', 'NED2014101.7', 'NED2014101.8','NED2014101.9', 'NED2014101.10', 'NED2014101.11', 'NED2014101.12', 
        'NED2014101.13','NED2014101.14', 'NED2014101.15', 'NED2014101.16', 'NED2014101.17', 'NED2014101.18', 'NED2014101.19', 'NED2014101.25', 
        'NED2014101.26', 'NED2014101.27', 'NED2014101.28', 'NED2014101.29', 'NED2014101.30', 'NED2014101.31', 'NED2014101.32', 'NED2014101.33', 
        'NED2014101.34', 'NED2014101.35', 'NED2014101.36','NED2014101.38', 'NED2014101.39', 'NED2014101.40','NED2014101.41', 
        'NED2014101.42', 'NED2014101.43', 'NED2014101.45','NED2014101.46', 'NED2014101.47', 'NED2014101.48', 'NED2014101.49', 'NED2014101.50',
        'NED2014101.51', 'NED2014101.52', 'NED2014101.53','NED2014101.54')

r = data.frame( cbind( id=ids, set = NA,  n = NA, no.roll.star = NA, no.roll.port = NA, mean.roll.star = NA, mean.roll.port = NA, min.roll.star = NA,
                       min.roll.port = NA, max.roll.star = NA, max.roll.port = NA, sd.star = NA, sd.port = NA), stringsAsFactors = FALSE )  

for (i in 1:nrow(r)){
  test = which(marport$mission==r[i,"id"])
  marport1 = marport[test, ]
  
  # Number of measurements
  r$n[i]=length(test)
  
  r$set[i] = substring(marport1$mission,12,14) 
  
  # Calculations with roll star
  r$no.roll.star[i] = length(which( is.finite(marport1$RollDoorStar)))
  r$mean.roll.star[i] = mean(marport1$RollDoorStar, na.rm=TRUE)
  r$min.roll.star[i] = min(marport1$RollDoorStar, na.rm=TRUE) 
  r$max.roll.star[i] = max(marport1$RollDoorStar, na.rm=TRUE)
  r$sd.star[i] = sd(marport1$RollDoorStar, na.rm=TRUE)
  
  # Calculations with roll port
  r$no.roll.port[i] = length(which( is.finite(marport1$RollDoorPort)))
  r$mean.roll.port[i] = mean(marport1$RollDoorPort, na.rm=TRUE)
  r$min.roll.port[i] = min(marport1$RollDoorPort, na.rm=TRUE) 
  r$max.roll.port[i] = max(marport1$RollDoorPort, na.rm=TRUE)
  r$sd.port[i] = sd(marport1$RollDoorPort, na.rm=TRUE)  
}


# save r dataframe
file="roll.summary.RData"
save(r, file="roll.summary.RData", compress=T)
load("roll.summary.RData")


# Plots
roll = data.frame(cbind(mission = marport$id, RollDoorPort = marport$RollDoorPort, RollDoorStar = marport$RollDoorStar, 
                        timestamp = marport$timestamp, set = marport$set, wingspread = marport$wingspread, 
                        doorspread =  marport$doorspread, opening.scanmar = marport$opening.scanmar), stringsAsFactors = FALSE )  


colnames(roll) = c("mission", "proll", "sroll", "timestamp", "set", "wingspread", "doorspread", "opening")
roll$proll = as.numeric(roll$proll)
roll$sroll = as.numeric(roll$sroll)
roll$set = as.numeric(roll$set)
roll$doorspread = as.numeric(roll$doorspread)
roll$wingspread = as.numeric(roll$wingspread)
roll$opening = as.numeric(roll$opening)


rollid=roll[which(roll$mission == "NED2014018.225") , ]
str(rollid)
plot(doorspread~timestamp,rollid)
str(rollid)
unique(rollid$mission)




require(lattice)

# Plot histograms for every mission
histogram( ~roll$proll | roll$mission )
histogram( ~roll$sroll | roll$mission )

#Port
histogram( ~ proll | mission, data=roll[which(roll$mission %in% c("NED2014101.27", "NED2014101.28") ), ],
           main = "Port Door Roll Angle", xlab="Angle")
#Starboard
histogram( ~ sroll | mission, data=roll[which(roll$mission %in% c("NED2014101.27", "NED2014101.28") ), ],
           main = "Starboard Door Roll Angle", xlab="Angle")

histogram( ~ proll | marport.mission, data=roll[which(roll$marport.mission %in% c("NED2014018.225", "NED2014018.141") ), ] , 
           layout = c(1,2)) 
histogram( ~ marport.RollDoorPort | marport.mission, data=roll[which(roll$marport.mission %in% c("NED2014101.5", "NED2014101.6", "NED2014101.7", 
          "NED2014101.8","NED2014101.9", "NED2014101.10", "NED2014101.11", "NED2014101.12", 
        "NED2014101.13","NED2014101.14", "NED2014101.15", "NED2014101.16", "NED2014101.17", "NED2014101.18", "NED2014101.19", "NED2014101.25", 
        "NED2014101.26", "NED2014101.27", "NED2014101.28", "NED2014101.29", "NED2014101.30", "NED2014101.31", "NED2014101.32", "NED2014101.33", 
        "NED2014101.34", "NED2014101.35", "NED2014101.36","NED2014101.38", "NED2014101.39", "NED2014101.40","NED2014101.41", 
        "NED2014101.42", "NED2014101.43", "NED2014101.45","NED2014101.46", "NED2014101.47", "NED2014101.48", "NED2014101.49", "NED2014101.50",
        "NED2014101.51", "NED2014101.52", "NED2014101.53","NED2014101.54") ), ] )
bwplot( ~ marport.RollDoorPort | marport.mission, data=roll[which(roll$marport.mission %in% c("NED2014101.5", "NED2014101.6", 
          "NED2014101.7", "NED2014101.8","NED2014101.9", "NED2014101.10", "NED2014101.11", "NED2014101.12", 
         "NED2014101.13","NED2014101.14", "NED2014101.15", "NED2014101.16", "NED2014101.17", "NED2014101.18", "NED2014101.19", "NED2014101.25", 
          "NED2014101.26", "NED2014101.27", "NED2014101.28", "NED2014101.29", "NED2014101.30", "NED2014101.31", "NED2014101.32", "NED2014101.33", 
           "NED2014101.34", "NED2014101.35", "NED2014101.36","NED2014101.38", "NED2014101.39", "NED2014101.40","NED2014101.41", 
         "NED2014101.42", "NED2014101.43", "NED2014101.45","NED2014101.46", "NED2014101.47", "NED2014101.48", "NED2014101.49", "NED2014101.50",
          "NED2014101.51", "NED2014101.52", "NED2014101.53","NED2014101.54") ), ] )
bwplot( ~ marport.RollDoorPort | marport.mission, data=roll[which(roll$marport.mission %in% c("NED2014101.5", "NED2014101.6") ), ],
        main = "Door Port Roll Angle", xlab="Angle")




# relationships between roll and net mensuration
# Port
plot(doorspread~RollDoorPort, marport, col="red", pch=19, main="Doorspread and wingspread with Port door tilt angle", ylab= "spread")
points(wingspread~RollDoorPort, marport, col="blue", pch=19)
legend(70, 60, c("doorspread", "wingspread"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red", "blue"))
# Star
plot(doorspread~RollDoorStar, marport, col="red", pch=19, main="Doorspread and wingspread with Starboard door tilt angle", ylab= "spread")
points(wingspread~RollDoorStar, marport, col="blue", pch=19)
legend(70, 60, c("doorspread", "wingspread"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red", "blue"))

# Door and Wing relationship in Marport

load("duck.RData")
rsamp=duck[sample(1:nrow(duck),10000, replace=FALSE), ]
tyrs=rsamp[which(rsamp$year %in% 2013:2014), ]
unique(tyrs$year)

plot(wingspread~doorspread, marport, pch=19, cex=0.9, main= "Doorspread vs Wingspread")
points(wingspread~doorspread, tyrs, col="red", pch=23, cex=0.7)
legend(75, 6, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red", "black"))


unique(marport$trip)
str(marport)
