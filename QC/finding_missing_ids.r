

# find the netmensuration filenames with missing ids
head(master)
i = which(is.na(master$id))
missing.id = unique(master$netmensurationfilename[i])
missing.id = data.frame(id = master$netmensurationfilename)
head(missing.id)
missing.id = which[missing.id$id ==! "Oracle instance of Perley DBs")]

# load in the GSINF table
gsinf = groundfish.db( DS="gsinf" )
gsinfvars=c("id", "sdate", "time", "dist", "settype")

# create a date variable in GSINF and MASTER
gsinf$date=substring(gsinf$sdate,0,10)
master$date=substring(master$timestamp,0,10)

# Create year, trip and set variable within GSINF
gsinf$year=as.numeric(substring(gsinf$id,4,7))
gsinf$trip=as.numeric(substring(gsinf$id,8,10))
gsinf$set=as.numeric(substring(gsinf$id,12,14))
head(gsinf)
gsinf.m=gsinf[which(gsinf$year %in% 2009:2014) , ]
write.table(gsinf.m, file= "gsinf.m.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)




# explore the 2009 data in GSINF and MASTER
data.2009 = gsinf[ which(gsinf$year == 2009),]
data.2009m = master[ which(master$year == 2009),]
unique(data.2009$trip)
unique(data.2009m$trip)
# three trips, 841, 27 and 2
data.841= data.2009[ which(data.2009$trip == 841),]
unique(data.841$date)
data.2= data.2009[ which(data.2009$trip == 2),]
unique(data.2$date)
data.27 = data.2009[ which(data.2009$trip == 27),]
data.27m = data.2009m[ which(data.2009m$trip == 27),]
length(unique(data.27$date))
length(unique(data.27m$date))
unique(data.27$date)
data.07.21 = data.27[ which(data.27$date == "2009-07-21"),]
data.07.21


# explore the 2010 data in GSINF and MASTER
data.2010 = gsinf[ which(gsinf$year == 2010),]
data.2010m = master[ which(master$year == 2010),]
unique(data.2010$trip)
unique(data.2010m$trip)
# three trips, 1, 27 and 2
data.27 = data.2010[ which(data.2010$trip == 27),]
data.27m = data.2010m[ which(data.2010m$trip == 27),]
length(unique(data.27$date))
length(unique(data.27m$date))
unique.data = unique(data.27$date)
unique.data.m = unique(data.27m$date)
unique.data[!unique.data %in% unique.data.m]
unique.data.m[!unique.data.m %in% unique.data]
data.07.11 = data.27m[ which(data.27m$date == "2010-07-11"),]
summary(data.07.11)

data.1 = data.2010[ which(data.2010$trip == 001),]
data.1m = data.2010m[ which(data.2010m$trip == 001),]
length(unique(data.1$set))
length(unique(data.1m$set))
unique(data.1$set)
unique(data.1m$set)
r = data.1[ which(data.1$set == 69),]
r

data.2 = data.2010[ which(data.2010$trip == 2),]
data.2m = data.2010m[ which(data.2010m$trip == 2),]
unique(data.2$trip)
unique(data.2m$trip)
unique(data.1$)

data.841= data.2009[ which(data.2009$trip == 841),]
unique(data.841$date)
data.2= data.2009[ which(data.2009$trip == 2),]
unique(data.2$date)

