# Plotting fishing time using gs
gs = net_mensuration.db( DS="bottom.contact",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
load("filteredlm.RData")

str(gs)
gs$bottom_duration = gs$bottom_duration/60
summary(gs$bottom_duration)
max_num = max(gs$bottom_duration)
hist(gs$bottom_duration, main = "Fishing Time Distribution", xlab="Fishing Time (min)", ylab="Frequency", 
     col="light blue", border="blue3")
mean(gs$bottom_duration, na.rm=TRUE)
max(gs$bottom_duration, na.rm=TRUE)
min(gs$bottom_duration, na.rm=TRUE)

text(35, 600, "Mean = 29.22 min")
text(17, 200, "Min = 15.03 min")
text(43, 100, "Max = 44.45 min")
       
# Wingspread
hist(filteredlm$wingspread, main = "Wingspread Frequencies", xlab="Wingspread (m)", ylab="Frequency", 
     col="light blue", border="navy")
mean(filteredlm$wingspread, na.rm=TRUE)
text(16.5, 350000, "Mean = 14.75 m")
abline(v=12.5, col= "red", lwd=c(2.5,2.5))
legend(18.5, 280000, c("standard = 12.5m"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("red"))

# Doorspread
hist(filteredlm$doorspread, main = "Doorspread Frequencies", xlab="Doorspread (m)", ylab="Frequency", 
     col="light green", border="green3")
mean(filteredlm$doorspread, na.rm=TRUE)
text(65, 250000, "Mean = 55.81 m")

# headline
hist(filteredlm$opening, main = "Headline Height Frequencies", xlab="Headline Height (m)", ylab="Frequency", 
     col="orange", border="orange3")
mean(filteredlm$opening, na.rm=TRUE)
text(4.3, 590000, "Mean = 3.28 m")

# depth
hist(filteredlm$depth, main = "Depth Frequencies", xlab="Depth (m)", ylab="Frequency", 
     col="grey", border="grey3", breaks="fd")
mean(filteredlm$depth, na.rm=TRUE)
text(170, 800000, "Mean = 126.92 m")


# relationship between fishing time and depth
plot(bottom_duration~bottom_depth, gs, xlim=c(0, 600), xlab="Bottom depth (m)", ylab="Fishing time (min)", pch=19, cex=0.5)
# Trying to sovle the fishing time bi-modal mystery
# all years, depths..not sure why?
# Perhaps it could be because we haven't applied the std. filter 
# Caused by poor bottom estimates?
points(bottom_duration~bottom_depth, gs.fail, pch=19, cex=0.5, col= "red")
i = which(gs$bc0.sd > 30)
p = which(gs$bc1.sd > 30)
gs.fail = gs[-i, -p] 
# Nope!
legend(500, 40, c("Accepted", "Rejected"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("black", "red"))


# latitude and longitude with fishing time
loadfunctions("polygons")
require(PBSmapping)
land = importPolys(polygon.ecomod("worldLR.ll"))

plotMap(land, xlim=c(-75, -55), ylim=c(35, 50),density=0)
points(latitude~longitude, filteredlm, pch=".", col="grey", xlim=c(-75, -55), ylim=c(35, 50))

# put bottom_duration in mins
gs$bottom_duration = gs$bottom_duration/60
# establish range of fishing time (< 20 mins)
i = gs[which((gs$bottom_duration < 20)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="red", pch=19, cex=0.5)
legend(x="bottomleft", c("Sets with fishing time < 20 min"),
       col=c("red"), pch =(19))

summary(gs$bottom_duration)
summary(gs)

# latitude and longitude 
loadfunctions("polygons")
require(PBSmapping)
land = importPolys(polygon.ecomod("worldLR.ll"))

plotMap(land, xlim=c(-75, -55), ylim=c(35, 50),density=0)
points(latitude~longitude, filteredlm, pch=".", col="grey", xlim=c(-75, -55), ylim=c(35, 50))

# put bottom_duration in mins, separate by less than 20 min and more
gs$bottom_duration = gs$bottom_duration/60
i = gs[which((gs$bottom_duration < 20)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="blue", pch=19, cex=0.4)


# separate 

i = gs[which((gs$bottom_depth < 600)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="red", pch=19, cex=0.4)

i = gs[which((gs$bottom_depth < 400)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="green", pch=19, cex=0.4)

i = gs[which((gs$bottom_depth < 200)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="yellow", pch=19, cex=0.4)

i = gs[which((gs$bottom_depth < 100)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="violet", pch=19, cex=0.4)

i = gs[which((gs$bottom_depth < 50)), ]
list = i$id
flm = filteredlm[which((filteredlm$id %in% list)), ]
length(unique(flm$id))
points(latitude~longitude, flm, col="blue", pch=19, cex=0.4)


legend(x="bottomleft", c("< 600", "< 400", "< 200", "< 100", "< 50"), col=c("red", "green", "yellow", "violet", "blue"), pch =19)










#bathmetry
i200 = read.table( polygon.ecomod("d200.ll"), as.is=TRUE, colClasses="numeric", skip=1 )
names(i200) = c("PID", "SID", "POS", "X", "Y")
i200 = i200 [ which( is.finite( rowSums( i200))), ]
ii = as.PolySet( i200)
addPolys(ii, border="khaki", projection=1)


for ( i in unique( ii$SID )) {
  j = which( ii$SID==i)
  if (length( j) > 1) {
    jj = ii[j,] 
    addPolys(jj, border="khaki")
  }
}




borders= read.csv(file=file.path(wd,"Management_Areas","Fisheries","areaborders.csv"), head=T, sep=",")
b=borders[which(borders$area==area),]


# read in shapefiles
#--------------------------------------
land= importShapefile(file.path(wd,"Basemaps","Terrestrial","landmass_region"))
coast=importShapefile(file.path(wd,"Basemaps","Marine","Coastline","coastline_polyline"))

#Overlay land and coastline such that any bad data (on land) is hidden
addPolys(land, col="khaki", border="khaki")
addLines(coast, col="black")
abline(h=b$slat, lwd=3)
abline(h=b$nlat, lwd=3)
abline(v=-b$wlon, lwd=3)
abline(v=-b$elon, lwd=3)

