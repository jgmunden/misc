


str(marport)
# Take a random sample
r.samp=marport[sample(1:nrow(marport), 10, replace=FALSE),]
r.samp$id

# PLOTS
  
  id = "NED2014101.7"
  scanmar1 = master[ which(master$id==id),]
  marport1 = marport [ which(marport$id==id),]
  
  
  # Depth
  # Filtering
  a = which( (marport1$DepthDoorPort > 750) | (marport1$DepthDoorPort < 0) ) 
  if (length(a) > 0 ) {
    marport1$DepthDoorPort[a]=NA
  }
   
  # determining x-axis
  min(scanmar1$timestamp)
  max(scanmar1$timestamp)
  min(marport1$timestamp)
  max(marport1$timestamp)
  
  plot(DepthDoorPort~timestamp, marport1, pch=19, col="red", ylim=c(0,100), 
       xlim=ymd_hms(c("2014-03-09 07:00:30","2014-03-09 08:30:31")),
       ylab= "depth", xlab= "timestamp", main = "NED2014101.7")
  
  points(depth~timestamp, scanmar1, pch=22, col="blue")
  # Legend
  legend(ymd_hms("2014-03-19 11:10:25"),250, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))



  
  # Wingspread
  plot(wingspread~timestamp, marport1, pch=19, col="red", ylim=c(0,30), xlim=ymd_hms(c("2014-03-19 11:00:25","2014-03-19 12:00:14")),
       ylab= "wingspread", xlab= "second", main = "NED2014101.38")
  points(wingspread~timestamp, scanmar1, pch=22, col="blue")
  # Legend
  legend(ymd_hms("2014-03-19 11:10:25"),30, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))


  
  
  # doorspread
  # filtering
  h = which( (marport1$doorspread > 90) | (marport1$doorspread < 0) ) 
  if (length(h) > 0 ) {
    marport1$doorspread[h]=NA
  } 
  plot(doorspread~timestamp, marport1, ylim=c(0,100), pch=19, col="red", xlim=ymd_hms(c("2014-03-19 11:00:25","2014-03-19 12:00:14")),
       ylab= "doorspread", xlab= "second", main = "NED2014101.38")
  points(doorspread~timestamp, scanmar1, pch=22, col="blue")
  # Legend
  legend(ymd_hms("2014-03-19 11:10:25"),90, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))


  
  
  # lat/lon
  plot(latitude~longitude, marport1, pch=19, col="red", ylab= "latitude", xlab= "longitude", main = "NED2014101.10" )
  points(latitude~longitude, scanmar1, pch="*", col="blue")
  legend(-64.30, 42.90, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))



# EXPORT TABLES
write.table(scanmar1, file= "scanmar1NED2014101.28.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
write.table(marport1, file= "marport1NED2014101.28.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

  ## Investigations with Pitch and Roll
  str(marport1)
  summary(marport1$PitchDoorPort)
  summary(marport1$PitchDoorStar)
  summary(marport1$RollDoorPort)
  summary(marport1$RollDoorStar)
  marport1$ts = as.numeric(marport1$timestamp)
  
  plot(PitchDoorPort~ts, marport1, col="orange", pch=19, ylim=c(-20, 100), xlim=c(387525.2, 387525.9))
  plot(PitchDoorPort~ts, marport1, col="orange", pch=19, ylim=c(-20, 100), xlim=c(387525.2, 387525.9))
  points(doorspread~ts, marport1, col="green", pch=19)
  
  id="NED2014101.15"
  plot(RollDoorPort~timestamp, marport1, col="orange", pch=19
       , ylab="degree", xlab="min")
  points(PitchDoorStar~ts, marport1, col="purple", pch=19)
  legend(387525.2, 10,  c("Star door", "Port door"), lty=c(1,1), lwd=c(2.5,2.5), col=c("purple","orange"))
  points(doorspread~ts, marport1, col="green", pch=19)
  

# Comparing all sets in a trip
# Form data frames with just the trip that we have Scanmar and Marport data (NED2014101)
head(marport)
unique(marport$Cruise)
marport.1=marport[which(marport$Cruise == 2014101) , ]
head(marport.1)
marport.1$d.source = M

head(master)
modern.2014=modern.data[which(modern.data$year == 2014) , ]
head(modern.2014)
modern.101=modern.2014[which(modern.2014$trip == 101) , ]
head(modern.101)
modern.101$d.source = S

m.u = unique(marport.1$id)
nrow(m.u)
s.u = unique(modern.101$id)
nrow(s.u)
