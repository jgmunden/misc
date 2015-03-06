# Estimation of: mean tow length, tow width, tow height, surface area and volume
str(filtered)
tail(filtered)
# Only have sets until from 2004:2009...
unique(filtered$year)
r.samp=gs[sample(1:nrow(gs), 1, replace=FALSE),]
unique(r.samp$id)
require(aspace)

-----------------------------------------------------------------------------------
# Test 1
test=gs[which(gs$id == "NED2013002.89"), ]
summary(test)

# Calculate distance
length(unique(test$longitude))
length(unique(test$latitude))
lat1 = test$lat
lon1 = test$lon
lat2 = test$lat.end
lon2 = test$lon.end

=RadiusEarth*ACOS(COS(RADIANS(90-Lat1)) *COS(RADIANS(90-Lat2)) 
                  +SIN(RADIANS(90-Lat1)) *SIN(RADIANS(90-Lat2)) *COS(RADIANS(Long1-Long2)))
# How to calculate distance when you have a min and max position?
R = 6371
d = R(acos(cos(as_radians(90-lat1))*cos(as_radians(90-lat2)) + sin(as_radians(90-lat1)) 
           * sin(as_radians(90-lat2)) * cos(as_radians(lon1-lon2))))   


# Net mensuration plot
plot(doorspread~timestamp, test, col="green", ylim=c(0, 90))
points(wingspread~timestamp, test, col="red")
points(opening~timestamp, test, col="blue")

# Means
mean.door = mean(test$doorspread, na.rm=TRUE)
mean.wing = mean(test$wingspread, na.rm=TRUE)
mean.head = mean(test$opening, na.rm=TRUE)

# depth
min.depth = min(test$depth)
max.depth = max(test$depth)
plot(depth~timestamp, test, col="purple")
# Why is the tail still included? I thought this was filtered data? min = 4.13 m
# Data needs to be filtered to fishing time..
 
# Time
min.t = min(test$timestamp)
max.t = max(test$timestamp)
diff = max.t - min.t

# Volume
volume = test$mean.wing * unique(test$distance) * test$mean.head
--------------------------------------------------------------------
# Find those sets which have a bottom duration, but NA for SD
bd = which(is.finite(gs$bottom_duration))
bdur = gs[bd, ]
summary(bdur)

summary(gs)


