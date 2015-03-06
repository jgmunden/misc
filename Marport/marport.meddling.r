
# load gsinf
gsinf = groundfish.db( DS="gsinf" )
gsinfvars=c("id", "sdate", "time", "dist", "settype")


# Make a year column, as well as trip_set column
gsinf$year=as.numeric(substring(gsinf$id,4,7))
unique(gsinf$year)

# Create a data frame including data from such one year
gsinf.marport1=gsinf[which(gsinf$year == '2013'),]
gsinf.marport2=gsinf[which(gsinf$year == '2014'),]
gsinf.marport=rbind(gsinf.marport1, gsinf.marport1)
unique(gsinf.marport$id)







