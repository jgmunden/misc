
# Determing if the marport data has been recording good depth and lat/lon data

test1=marport[which(marport$mission == "NED2013002.45") , ]
# March 11 "NED2013002.53"--> looks good
# March 10

# Depth
no.depth1 = length(which( is.finite(test1$DepthDoorPort)))
no.depth2 = length(which( is.finite(test1$DepthDoorStar)))
no.depth3 = length(which( is.finite(test1$DepthWingPort)))
no.depth4 = length(which( is.finite(test1$DepthWingStar)))
no.depth5 = length(which(is.finite(test1$DepthScanmar)))
no.depth = sum(no.depth1, no.depth2, no.depth3, no.depth4, no.depth5)
# Removing frequencies
i = which( (test1$DepthDoorPort > 750) | (test1$DepthDoorPort < 0) ) 
if (length(i) > 0 ) {
  test1$DepthDoorPort[i]=NA
}
i=which(!is.finite(test1$DepthDoorPort))
test1 = test1[ -i, ]
mean.depth = mean(test1$DepthDoorPort)

# time
min.time=min(test1$timestamp)
max.time=max(test1$timestamp)

# spatial data
min.lat = min(test1$latitude) 
max.lat = max(test1$latitude)
mean.lat = mean(test1$latitude)
min.lon = min(test1$longitude) 
max.lon = max(test1$longitude)
mean.lon = mean(test1$longitude)
