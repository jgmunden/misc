# marport plotting
marport = net_mensuration.db( DS="marport.gated",  net.root.dir=net.root.dir )
setwd("C:/cygwin64/home/mundenj/work")

file="marport.edit.RData"
save(marport, file="marport.edit.RData", compress=T)
load("marport.edit.RData")

# Data from Scanmar that we need, use unfiltered data because the Marport data is not filtered 

post =master[which(master$year %in% 2013:2014) , ]

# Creating the simplied verison of marport that now can be easily loaded

summary(marport)
marport$depth1 = marport$DepthScanmar
marport$depth2 = marport$DepthDoorPort
marport$depth3 = marport$DepthDoorStar
marport$depth4 = marport$DepthWingPort
marport$depth5 = marport$DepthWingStar

head(marport)

marport$DepthScanmar = NULL
marport$DepthDoorPort = NULL
marport$PitchDoorPort = NULL
marport$RollDoorPort = NULL
marport$PitchDoorStar = NULL
marport$RollDoorStar = NULL
marport$TempDoorStar= NULL
marport$DepthWingPort = NULL
marport$PitchWingPort = NULL
marport$RollWingPort = NULL
marport$TempWingPort = NULL
marport$ DepthWingStar = NULL
marport$ rootname = NULL
marport$ mission = NULL
marport$ Vessel = NULL
marport$ Cruise = NULL
marport$ TempWingStar = NULL
marport$ opening.scanmar = NULL
marport$ opening = NULL
marport$ DepthDoorStar = NULL

# filter depth
  x = marport$depth1
  i = which( (x > 750) | (x < 0) ) 
  marport = marport [-i, ]  
  summary(marport$depth1)

  x = marport$depth2
  i = which( (x > 750) | (x < 0) ) 
  marport = marport [-i, ]  
  summary(marport$depth2)
  
  x = marport$depth3
  i = which( (x > 750) | (x < 0) ) 
  marport = marport [-i, ]  
  summary(marport$depth3)

  x = marport$depth4
  i = which( (x > 750) | (x < 0) ) 
  marport = marport [-i, ]  
  summary(marport$depth4)

  x = marport$depth5
  i = which( (x > 750) | (x < 0) ) 
  marport = marport [-i, ]  
  summary(marport$depth5)
========================================================================
  
    # filter by year
    com14=master[which(master$year == 2014) , ]
    com13=master[which(master$year == 2013) , ]
    r.samp.com14=com14[sample(1:nrow(com14), 10000, replace=FALSE),]
    r.samp.com13=com13[sample(1:nrow(com13), 10000, replace=FALSE),]
    
    year14=marport[which(marport$year == 2014), ]
    year13=marport[which(marport$year == 2013), ]
=========================================================================
# Plotting  
  
# wingspread plots
points(wingspread~depth, r.samp.com13, pch=19, cex=0.4, col="green")
points(wingspread~depth2, year13, pch=19, cex=0.4, col="orange")
legend("bottomright", c("Marport", "Scanmar"), pch=19, cex=1.3, pt.cex=1, col=c("red", "blue"))

plot(wingspread~depth, r.samp.com14, pch=19, cex=0.4, col="blue")
points(wingspread~depth1, year14, pch=19, cex=0.4, col="red")

# doorspread plot
points(doorspread~depth, r.samp.com13, pch=19, cex=0.4, col="orange", main="2013")
points(doorspread~depth2, year13, pch=19, cex=0.4, col="green")
legend("bottomright", c("Marport", "Scanmar"), pch=19, cex=1.3, pt.cex=1, col=c("red", "blue"))

plot(doorspread~depth, r.samp.com14, pch=19, cex=0.4, col="blue")
points(doorspread~depth1, year14, pch=19, cex=0.4, col="red")
legend("bottomright", c("Marport", "Scanmar"), pch=19, cex=1.3, pt.cex=1, col=c("red", "blue"))


plot(wingspread~depth, com14, pch=19, cex=0.4, col="blue")

# Looking into the 5 depth measure we have
unique(com14$trip)
# for scanmar in 2014 we have trips 2 and 101
t2 =com14[which(com14$trip == 2) , ]
t101 = com14[which(com14$trip == 101) , ]
unique(t101$set)

unique(com13$trip)
# for scanmar in 2013 we have trips 28 and 22
t22 =   com13[which(com13$trip == 22) , ]
t28 = com13[which(com13$trip == 28) , ]

hist(marport$depth1, breaks=40)
hist(marport$depth2)
hist(marport$depth3)
hist(marport$depth4)
hist(marport$depth5)

# generate data sets corresponding to one trawl set and check that there is data for both Scanmar and Marport
test=marport[which(marport$id == "NED2014101.4"), ]
head(test)
test.sc=master[which(master$id == "NED2014101.4"), ]
head(test.sc)

# NED2014018.208, depth 1, 2, 3
# Only data for marport
# General depth plots
summary(test)

d=range(test$depth1, na.rm=TRUE)
plot(depth1~timestamp, test, type = "p", pch=19, xlim=ymd_hms(c("2014-07-29 16:11:46","2014-07-29 17:11:46")),
      col="orange", lwd = 1, main =  "Marport depth measurements", sub=id, ylim=c(d[2], d[1]))
d=range(test$depth2, na.rm=TRUE)
points(depth2~timestamp, test, type = "p", pch=19, col="green", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
d=range(test$depth3, na.rm=TRUE)
points(depth3~timestamp, test, type = "p", pch=19, col="purple", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
legend("topleft",250, c("Scanmar depth", "Marport port door", "Marport star door"), lty=c(1,1), lwd=c(2.5,2.5), col=c("orange","green", "purple"))

# NED2014101.4, depth 2
summary(test)

# filter depth
x = test$depth2
i = which( (x > 150) | (x < 0) ) 
test = test [-i, ]  

d=range(test$depth2, na.rm=TRUE)
plot(depth2~timestamp, test, type = "p", pch=19, xlim=ymd_hms(c("2014-03-09 22:50:13","2014-03-09 23:47:47")),
     col="green", lwd = 1, main =  "Marport depth measurements", sub=id, ylim=c(d[2], d[1]))

d=range(test.sc$depth, na.rm=TRUE)
points(depth~timestamp, test.sc, type = "p", pch=19, col="blue", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
head(test.sc)

legend("topleft",250, c("Scanmar depth", "Marport port door", "Marport star door"), lty=c(1,1), lwd=c(2.5,2.5), col=c("orange","green", "purple"))


