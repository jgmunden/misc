
# project that assimilates net mensuration data 
# this file is the main calling program 

# define a few file locations
# file path should be altered once I get my own log on
netswd = file.path("C:", "Users", "MundenJ", "Desktop", "Scanmar")
nmfunctionsdirectory = file.path(netswd, "net_mensuration")
netmensuration.files = list.files( nmfunctionsdirectory, full.names=TRUE )
for (fn in netmensuration.files) source(fn)


# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

library(lubridate)


# Line 22-31 does not need to be ran until we merge more data into the data frame (nm)


# this gets the oracle data and saves to local system
net_mensuration.db( DS="all.historical.data.redo", netswd=netswd ) 
master = net_mensuration.db( DS="all.historical.data", netswd=netswd )

# gating type filtering
master$doorspread = filter.nets("doorspread.range", master$doorspread)
master$wingspread = filter.nets("wingspread.range", master$wingspread)
master$clearance = filter.nets("clearance.range", master$clearance)
master$opening = filter.nets("opening.range", master$opening)
master$depth = filter.nets("depth.range", master$depth)


# if I want to look at only data from the historic period (1990-1992)
master$year=as.numeric(substring(master$id,4,7))
historic.data.1990=master[which(master$year == '1990'),]
historic.data.1991=master[which(master$year == '1991'),]
historic.data.1992=master[which(master$year == '1992'),]
historic.data=rbind(historic.data.1990, historic.data.1991, historic.data.1992)
summary(historic.data)

..


# load gsinf
gsinf = groundfish.db( DS="gsinf" )
gsinfvars=c("id", "sdate", "time", "dist", "settype")
# create year in gsinf
gsinf$year=as.numeric(substring(gsinf$id,4,7))

# load the saved file
nm = net_mensuration.db( DS="nm.timestamp", netswd=netswd ) 

nm$year=as.numeric(substring(nm$id,4,7))

# filtered data
net_mensuration.db( DS="range.checks.redo", netswd=netswd ) 
nm = net_mensuration.db( DS="range.checks", netswd=netswd ) 



# Data filtering for analyses, fixing ranges"gating", better way to do this

nm = net_mensuration.db( DS="nm.timestamp", netswd=netswd ) 

# subsampling
# nm=nm[sample (100000), ] (remove comment if random sample is required)




# Create new data.frame where 1990-1992 = historic.data, 
master$year=as.numeric(substring(master$id,4,7))
historic.data.1990=master[which(master$year == '1990'),]
historic.data.1991=master[which(master$year == '1991'),]
historic.data.1992=master[which(master$year == '1992'),]
historic.data=rbind(historic.data.1990, historic.data.1991, historic.data.1992)
summary(historic.data)

# test that a data.frame only contains one year of data 
# unique(data.frame$variable)

# Create new data.frame where 2004:2009 & 2012:2014 = modern.data

modern.data.2004=nm[which(nm$year == '2004'),]
modern.data.2005=nm[which(nm$year == '2005'),]
modern.data.2006=nm[which(nm$year == '2006'),]
modern.data.2007=nm[which(nm$year == '2007'),]
modern.data.2008=nm[which(nm$year == '2008'),]
modern.data.2009=nm[which(nm$year == '2009'),]

modern.data=rbind(modern.data.2004, modern.data.2005, modern.data.2006, modern.data.2007, modern.data.2008, modern.data.2009, modern.data.2012, modern.data.2013, modern.data.2014)


