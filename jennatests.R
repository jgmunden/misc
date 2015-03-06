p = list()

p$libs = RLibrary("INLA", "numDeriv", "lubridate", "PBSmapping"  )

# this relies upon the gsinf table which is accessible from the groundfish functions
p$inits = loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

# define location of local data files 
p$scanmar.dir = file.path( project.directory("groundfish"), "data", "nets", "Scanmar" ) 
p$marport.dir = file.path( project.directory("groundfish"), "data", "nets", "Marport" ) 

p$current.year = 2014
p$netmensuration.years = c(1990:1992, 2004:p$current.year)  ## 2009 is the first year with set logs from scanmar available .. if more are found, alter this date
# p$netmensuration.years = p$current.year  ## for incremental/annual update

setwd( "C:\\cygwin64\\home\\mundenj\\work" )

# load master
marport = marport.db( DS="marport.gated",  p=p )
master  = scanmar.db( DS="sanity.checks", p=p )# load all scanmar data for development ...
master = master[ which( master$net=="standard" & !is.na( master$id)) , ] 

# Load originial gsinf
gsinf = groundfish.db( DS="gsinf" )

# Local data frames in C:\\cygwin64\\home\\mundenj\\work
# master of all years, only valid mission and no us trawls incld
load("master.updated.RData")
# updated historical data
load("h.data.RData")
# updated modern data
load("m.data.RData")

# filitered to bottom contact time
filtered = scanmar.db( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar

# fishing time total included
gs = scanmar.db( DS="bottom.contact", p=p)  # bring in estimates of bottom contact times from scanmar
# Easier to look at bottom duration when its in mins
gs$ftmins = (gs$bottom_duration)/60

# Load marport/basedata
marport = marport.db( DS="marport",  p=p )
marport = marport.db( DS="marport.gated",  p=p )
--------------------------------------------------------------------------------------------------------------------------  
# form data sets by year
com14=filteredlm[which(filteredlm$year == 2014) , ]
com13=filteredlm[which(filteredlm$year == 2013) , ]
com12=filteredlm[which(filteredlm$year == 2012) , ] 
com11=filteredlm[which(filteredlm$year == 2011) , ]
com10=filteredlm[which(filteredlm$year == 2010) , ]
com09=filteredlm[which(filteredlm$year == 2009) , ]
com08=filteredlm[which(filteredlm$year == 2008) , ]
com07=filteredlm[which(filteredlm$year == 2007) , ]
com06=filteredlm[which(filteredlm$year == 2006) , ]
com05=filteredlm[which(filteredlm$year == 2005) , ]
com04=filteredlm[which(filteredlm$year == 2004) , ]
com90=filteredlm[which(filteredlm$year == 1990) , ]
com91=filteredlm[which(filteredlm$year == 1991) , ]

# Saving local copies of historical and modern data
historical.data=master[which(master$year %in% 1990:1992) , ]
file="h.data.updated.RData"
save(historical.data, file="h.data.updated.RData", compress=T)
modern.data=master[which(master$year %in% 2004:2014) , ]
file="m.data.updated.RData"
save(modern.data, file="m.data.updated.RData", compress=T)
  
# To write a csv with all rows that have NA for mission
i = which(is.na(master$id))
t = unique( master$netmensurationfilename[i])
p = data.frame(id = t)
write.table(t, file= "missing_id.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)

# Create a for loop to visualize trawl geometry changes within a set (doorspread, wingspread and headline height)
allids=unique(master$id)
i=sample(1:length(allids),15)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,] 
  if(any(is.finite(i$doorspread))){
    plot(doorspread~timestamp,i, main =  "Trawl Geometry", sub=id, ylim=c(0,90), pch=19, cex=0.5 )
    points(wingspread~timestamp, i, col="blue", pch=19, cex=0.5)
    points(opening~timestamp, i, col="red", pch=19, cex=0.5)
    legend("topright", c("Doorspread", "Wingspread", "Headline height"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("black", "red", "blue"))
    
  }
}

# Look at the number of unique values within a set
id = "NED2005001.1" 
mm = modern.data[ which(modern.data$id==id) , ]
length(unique(mm$doorspread))
length(unique(mm$wingspread))  
length(unique(mm$opening))
length(unique(mm$clearance))  
length(unique(mm$latitude))
length(unique(mm$longitude)) 
 
# Generate random samples
r.samp=filteredlm[sample(1:nrow(filteredlm), 10, replace=FALSE),]
r.samp$id

# Determing theh number of unique sets within a year
test=master[which(master$year == 2013) , ]  
length(unique(test$id))  
# 122 sets in 2014
# 248 sets in 2013
# 370 setse in total

# Adding a variable by subseting an existing variable
master$date=substring(master$timestamp,0,10)  

# General depth plots
i = master[which(master$id == "NED2014002.38"), ]
d=range(i$depth, na.rm=TRUE)
plot(depth~timestamp,i, type = "p", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))

--------------------------------------------------------
# March 2, 2015 update and investigations into data.frame
# load updated scanmar data, 1723 sets included
nm = scanmar.db( DS="scanmar.filtered",  p=p )  
length(unique(nm$id))
# get rows
nms = which(is.finite(nm$time.difference))
# create df
nmsc = nm[nms, ]
summary(nmsc)
length(unique(nmsc$id))
# cannot be right, only 9 sets..??

# load data before sd filtering
gs = scanmar.db( DS="bottom.contact",  p=p )
summary(gs)
gs$year=substring(as.character(gs$timestamp), 1,4)
# create dataframe with only modern data
gs.mod = gs[which((gs$year %in% 2004:2014)), ]
unique(gs.mod$year)
summary(gs.mod)
gs.mod$bottom_duration = gs.mod$bottom_duration/60 
hist(gs.mod$bottom_duration)
hist(gs.mod$bc0.sd, breaks="fd", xlim=c(0,100))
hist(gs.mod$bc1.sd, breaks="fd", xlim=c(0,100))

# Determine the number of sets that pass the sd threshold
x = gs.mod$bc0.sd
y = gs.mod$bc1.sd
i = which( (x > 30) | (y > 30) ) 
gs.mod.pass = gs.mod [-i, ]  

hist(gs.mod.pass$bottom_duration, "fd")
hist(gs.mod.pass$bc1.sd, breaks="fd", xlim=c(0,100), col="red")
hist(gs.mod.pass$bc0.sd, breaks="fd", xlim=c(0,100), col="blue")
plot(bottom_depth~bottom_duration, gs.mod.pass, ylim=c(0,500))
str(gs)
length(unique(gs.mod.pass$id))
# 3825
length(unique(gs.mod$id))
# 4884
# Only 1059 sets should be removed after the SD filtering
length(unique(nm$id))
# The filtering step actually removed 3320 sets, why? nm needs to be re-run

summary(gs.mod.pass)
hist(gs.mod.pass$bottom_duration, breaks="fd", xlim=c(15,45), col="blue")
# Need to re-run the filtering step as it is biased by using gsinf time
