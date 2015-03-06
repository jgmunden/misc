require (maptools)
mmm
t2$X=t2$longitude
t2$Y=t2$latitude
t2$EID=as.numeric(paste(1:nrow(t2),  sep=""))
#t2$year=years(t2$board_date)

require(PBSmapping)

x=t2
x=as.EventData(x, projection= "LL")


# Create maps with pbsMapping
#--------------------------------------------

source("C:/Scripts/snow.crab.mapping.functions.r")



makemap(x, area="west", title="Tow Locations", addlabels=F)

addPoints(data=s1, col="black", bg="green", pch=21, cex=0.8)  #chart showing all stations
addPoints(data=s2, col="black", bg="red", pch=21, cex=0.8)  #chart showing all stations
addPoints(data=s3, col="black", bg="yellow", pch=21, cex=0.8)  #chart showing all stations

