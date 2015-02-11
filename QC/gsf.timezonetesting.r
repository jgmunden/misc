# Make a year column, as well as trip_set column
gsinf$year=as.numeric(substring(gsinf$id,4,7))
unique(gsinf$year)
unique(nm$year)
gsinf$trip_set=as.numeric(substring(gsinf$id,8,14))
str(gsinf)

# Create a data frame including data from such one year
gsinf.marport1=gsinf[which(gsinf$year == '2013'),]
gsinf.marport2=gsinf[which(gsinf$year == '2014'),]
gsinf.marport=rbind(gsinf.marport1, gsinf.marport1)
unique(gsinf.marport$id)


gsinf.2009=gsinf[which(gsinf$trip_set == '2009'),]
plot(gsinf.2009$time)

# Choose the same set for both modern.data.2009 and gsinf.2009
gsinf.2009_=gsinf.2009[which(gsinf.2009$id == 'NED2009027.186'),]
modern.data.2009_=modern.data.2009[which(modern.data.2009$id == 'NED2009027.186'),]

range(modern.data.2009_$logtime)
range(modern.data.2009_$timestamp)

# Generic code to look up any id and compare times between gsinf and nm
gsinfq=gsinf[which(gsinf$id == 'NED2009027.186'),]
nmq=nm[which(nm$id == 'NED2009027.186'),]
# Conclusion: the same time zones were used as start time was within 3 mins, end time 13 mins

gsinfq=gsinf[which(gsinf$id == 'HAM1991231.1'),]
nmq=nm[which(nm$id == 'HAM1991231.1'),]
range(nmq$logtime)
# Conclusion: the same time zones were used as start time was within 3 mins, no end time for gsinf

gsinfq=gsinf[which(gsinf$id == 'ATC1970175.22'),]
nmq=nm[which(nm$id == 'ATC1970175.22'),]
range(nmq$logtime)
# Conclusion: nm did not have a corresponding id

gsinfq=gsinf[which(gsinf$id == 'ATC1970175.2'),]
nmq=nm[which(nm$id == 'ATC1970175.2'),]
range(nmq$logtime)
# Conclusion: nm did not have a corresponding id

gsinfq=gsinf[which(gsinf$id == 'NED2006001.73'),]
nmq=nm[which(nm$id == 'NED2006001.73'),]
range(nmq$logtime)
# Conclusion: start time within 4 mins, end time within 2 mins

gsinfq=gsinf[which(gsinf$id == 'TEM2008830.56'),]
nmq=nm[which(nm$id == 'TEM2008830.56'),]
range(nmq$logtime)
# Conclusion: start within 34 mins, end within 26 mins

gsinfq=gsinf[which(gsinf$id == 'TEL2007745.46'),]
nmq=nm[which(nm$id == 'TEL2007745.46'),]
range(nmq$logtime)
# Conclusion: start within 35 mins, end within 26 mins

gsinfq=gsinf[which(gsinf$sdate == '2009-07-02 20:15:14'),]
nmq=nm[which(nm$timestamp == '2009-07-02 20:15:14'),]
range(nmq$logtime)
# Conclusion: start within 35 mins, end within 26 mins

gsinf.id=gsinf$id[sample (30)] 
gsinf.id

nm.id=nm$id[sample (30)]
nm.id

unique(nm$id)which(nm$year == 'ATC1970175.22'),]

pp$id2=as.character(substring(pp$id,4,7))






gf.new.1=gf[which(gf$year == '2012'),] (#340 sets)
gf.new.2=gf[which(gf$year == '2013'),] (#537 sets)
gf.new.3=gf[which(gf$year == '2014'),] (no data)
gf.new.4=gf[which(gf$year == '2009'),] (# 341 sets) 
 
gf.new=rbind(gf.new.1, gf.new.2, gf.new.3, gf.new.4)
 
gf.new.1$id
gf.new.2$id
gf.new.3$id
gf.new.4$id

meta.2009$sdate

range(modern.data.2009$timestamp)

