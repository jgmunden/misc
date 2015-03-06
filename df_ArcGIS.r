# Create data, export as a text of csv file taht can be oped in ArcGIS

# Create a map with the lat and longs of sets from one trip last year
h.data=master[which(master$year %in% 1990:1992) , ]
summary(h.data$longitude)
m.data=master[which(master$year %in% 2004:2014) , ]
summary(m.data$longitude)
long.p = m.data[ which(m.data$longitude =  -4.12),]
summary(long.p$longitude)
unique(long.p$id)

T2007745.64 = master[ which(master$id == "TEL2007745.64"),]
unique(T2007745.64$longitude)

long.p$longitude*-1


# load groundfish inf table, make a new df called m.data that has only modern data
gsinf = groundfish.db( DS="gsinf" )
gsinf$year=substring(gsinf$sdate,1,4)
gsinf$trip=as.numeric(substring(gsinf$id,8,10))
gsinf$set=as.numeric(substring(gsinf$id,12,14))
head(gsinf)
m.data=gsinf[which(gsinf$year %in% 2004:2014) , ]
head(m.data)
unique(m.data$year)
summary(m.data$trip)
write.table(m.data, file= "modern.data.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

m.2014.101=m.2014[which(m.2014$trip == 101) , ]
unique(m.2014.101$trip)
write.table(m.data, file= "m.data.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
