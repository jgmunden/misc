
# project that assimilates net mensuration data 
# this file is the main calling program 

# define a few file locations
# file path should be altered once I get my own log on
netswd = file.path("C:", "Users", "ChoiJ", "Desktop")
nmfunctionsdirectory = file.path(netswd, "Jenna Doc", "R", "net_mensuration")
netmensuration.files = list.files( nmfunctionsdirectory, full.names=TRUE )
for (fn in netmensuration.files) source(fn)


# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 

library(lubridate)


# Line 22-31 does not need to be ran until we merge more data into the data frame (nm)


# this gets the oracle data and saves to local system
net_mensuration.db( DS="perley.database.datadump", netswd=netswd ) 
# merge in the two perley databases
net_mensuration.db( DS="perley.database.merge", netswd=netswd ) 
# load it
nm = net_mensuration.db( DS="perley.database", netswd=netswd ) 
# here we will add the more modern data series and merge with perley
...
# merge groundfish  timestamps and ensure that net mensuration timestamps are correct
net_mensuration.db( DS="nm.timestamp.redo", nm=nm, netswd=netswd ) 


# load the saved file
nm = net_mensuration.db( DS="nm.timestamp", netswd=netswd ) 

# filtered data
net_mensuration.db( DS="range.checks.redo", netswd=netswd ) 
nm = net_mensuration.db( DS="range.checks", netswd=netswd ) 





------ testing 
  
  
nm = net_mensuration.db( DS="nm.timestamp", netswd=netswd ) 
# subsampling
nm=nm[sample (50000), ]



# Defining the ranges for each variable that needs filtering

# doorspread sanity check
i = which( (nm$doorspread > 90) | (nm$doorspread < 0) ) 
if (length(i) > 0 ) {
  nm$doorspread[i]=NA
}

# wingspread sanity check
i = which( (nm$wingspread > 25) | (nm$wingspread < 0) ) 
if (length(i) > 0 ) {
  nm$wingspread[i]=NA
}

# opening sanity check
i = which( (nm$opening > 10) | (nm$opening < 0) ) 
if (length(i) > 0 ) {
  nm$opening[i]=NA
}

# clearance sanity check
i = which( (nm$clearance > 5) | (nm$opening < 0) ) 
if (length(i) > 0 ) {
  nm$clearance[i]=NA
}

# depth sanity check
i = which( (nm$depth > 750) | (nm$depth < 0) ) 
if (length(i) > 0 ) {
  nm$depth[i]=NA
}

# To determine the percent of plots that remain after filtering
# This is A.T method, need to modify the variable names to fit our data
Rows$pdoor <- (Rows$doortrue/Rows$length) * 100
Rows$pwing <- (Rows$wingtrue/Rows$length) * 100
Rows$popen <- (Rows$opentrue/Rows$length) * 100


# Relational plots

# global wingspread vs doorspread plot 
plot(doorspread~wingspread,nm, pch=20)

# global opening vs wingspread plot
plot(wingspread~opening,nm, pch= 20)

# global clearance vs opening
plot(clearance~opening,nm, pch= 21)

# global depth vs doorspread 
plot(doorspread~depth,nm, pch= 21)
plot(doorspread~depth,nm, xlim=c(0, 750), ylim=c(0,90), pch= 21)




# Modeling

# wingspread, doorspread
o=lm(doorspread~wingspread, nm)
summary (o)
require(MASS)
r= rlm(doorspread~wingspread, nm)
summary (r)

cor(nm$wingspread, nm$doorspread, na.rm = TRUE, method = c(pearson))

# depth, doorspread
t=lm(doorspread~depth, nm)
summary (t)
require(MASS)
z= rlm(doorspread~depth, nm)
summary (r)

# loooking at the data within 2 S.E of the mean
pred=data.frame(wingspread=0:22)
out=predict(o, newdata=pred, se.fit=T)
out = as.data.frame(out)
out2=predict(r, newdata=pred, se.fit=T)
out2= as.data.frame(out2)
out
out2
out$ub=out$fit+2*out$se.fit
out$lb=out$fit-2*out$se.fit
out

# Summary statistics
summary (nm$doorspread)
summary(nm$wingspread)
summary(nm$clearance)
summary(nm$opening)
summary(nm$depth)
