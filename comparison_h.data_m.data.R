# Exploring the question: Is there a difference in net performance between historic and modern data?
# Make sure master is loaded in

#Create the data.frames

# Historic period (1990-1992)
master$year=as.numeric(substring(master$id,4,7))
historic.data.1990=master[which(master$year == '1990'),]
historic.data.1991=master[which(master$year == '1991'),]
historic.data.1992=master[which(master$year == '1992'),]
historic.data=rbind(historic.data.1990, historic.data.1991, historic.data.1992)

summary(historic.data)

# Finding the number of data points recorded for each net mensuration variable
l = length(historic.data$id)
win = length(which(is.finite(historic.data$wingspread)))
dor = length(which(is.finite(historic.data$doorspread)))
cle = length(which(is.finite(historic.data$clearance)))
ope = length(which(is.finite(historic.data$opening)))

# Determing the percent time when data points are recorded for a selected parameter
wh = (win/l*100)
dh = (dor/l*100)
ch = (cle/l*100)
oh = (ope/l*100)


# Modern period (2004-2014)
modern.data.2004=master[which(master$year == '2004'),]
modern.data.2005=master[which(master$year == '2005'),]
modern.data.2006=master[which(master$year == '2006'),]
modern.data.2007=master[which(master$year == '2007'),]
modern.data.2008=master[which(master$year == '2008'),]
modern.data.2009=master[which(master$year == '2009'),]
modern.data.2010=master[which(master$year == '2010'),]
modern.data.2011=master[which(master$year == '2011'),]
modern.data.2012=master[which(master$year == '2012'),]
modern.data.2013=master[which(master$year == '2013'),]
modern.data.2014=master[which(master$year == '2014'),]
modern.data=rbind(modern.data.2004, modern.data.2005, modern.data.2006, modern.data.2007, modern.data.2008, modern.data.2009, modern.data.2010, modern.data.2011, modern.data.2012, modern.data.2013, modern.data.2014)     

summary(modern.data)

# Finding the number of data points recorded for each net mensuration variable
l = length(modern.data$id)
win = length(which(is.finite(modern.data$wingspread)))
dor = length(which(is.finite(modern.data$doorspread)))
cle = length(which(is.finite(modern.data$clearance)))
ope = length(which(is.finite(modern.data$opening)))
dep = length(which(is.finite(modern.data$depth)))
lts = length(which(is.finite(modern.data$ltspeed)))
cts = length(which(is.finite(modern.data$ctspeed)))
lat = length(which(is.finite(modern.data$latitude)))
lon = length(which(is.finite(modern.data$longitude)))
gyr = length(which(is.finite(modern.data$gyro)))


# Determing the percent time when data points are recorded for a selected parameter
wm = (win/l*100)
dm = (dor/l*100)
cm = (cle/l*100)
om = (ope/l*100)
lts/l*100
cts/l*100
lat/l*100
lon/l*100
gyr/l*100


# the percent change between time frames
(wm-wh)/wh*100
(dm-dh)/dh*100
(cm-ch)/ch*100
(om-oh)/oh*100

# determining average number of measurements per tow









# creating a data.frame with the percent of data obtained/parameter 
# historic data
percent.data.historic <-c(44.59, 30.09, 39.98, 67.41)
as.numeric <- percent.data.historic
str(percent.data.historic)
headers <-c("wingspread", "doorspread", "clearance", "opening")
h <- cbind(percent.data.historic, headers)
h

# modern data
percent.data.modern <-c(, , , )
as.numeric <- percent.data.modern
str(percent.data.modern)
headers <-c("wingspread", "doorspread", "clearance", "opening")
m <- cbind(percent.data.modern, headers)
m











# Attempting to slice the data frame into sets which can be plotted and enable the comparing of means

# to get the number of unique sets in the historic data
historic.data.sets=unique(historic.data$id)
# the resulting  matrix
historic.data.sets

# take a random sample of 10 individual sets
historic.data.sets=historic.data.sets[sample (10)]

# Subsetting to compare data within a tow
sub <- subset(historic.data, id == "NED1992173.25") 
# plots within a tow
plot(doorspread~logtime, sub)
plot(wingspread~logtime, sub)
plot(clearance~logtime, sub)
plot(opening~logtime, sub)












