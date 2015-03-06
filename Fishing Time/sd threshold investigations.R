# Where did all the sets go?

length(unique(master$id))
length(unique(filtered$id))
length(unique(filteredlm$id))

str(gs)
# add year
gs$year = substring(as.character(gs$id), 4,7)
# subset by years
gs=gs[which(gs$year %in% 2004:2014), ]
gs

hist(gs$bc0.sd, breaks=60, xlim=c(0,40))
hist(gs$bc1.sd, breaks=60, xlim=c(0,40))


# need to figure out how to separate gs into those sets that pass the threshold and those sets that do not
gs.pass = gs[which(gs$bc0.sd <= 30 & gs$bc1.sd <= 30), ]
length(unique(gs.pass$id))
length(unique(gs$id))
unique(gs$settype)




summary(gs.pass)
983/2153*100
# Only 45% of the data passes our threshold


# Plot with land
loadfunctions("polygons")
require(PBSmapping)
land = importPolys(polygon.ecomod("worldLR.ll"))

plotMap(land, xlim=c(-71, -56), ylim=c(39, 48),density=0)
points(lat~lon, gs, pch=19, cex=0.4, col="blue")
points(lat~lon, gs.pass, pch=19, cex=0.4, col="red")

legend("bottomright", c("Pass","Fail"), pch=19, cex=0.5, col=c("red", "blue"))
