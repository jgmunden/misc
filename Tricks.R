# Tricks of the Trade, useful R code I tend to forget!

# How to subset from a data frame using a smaller list
mastersub = master[which((master$id %in% ids)), ]

# Subsettings a data frame with a certain value (i.e. year = 2013)
test=master[which(master$year == 2013), ]

# How to take a random sample
r.samp=com1[sample(1:nrow(com1), 10000, replace=FALSE),]

# Legends for graphs
legend(x,y, c("Marport", "Scanmar"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("red", "blue"))
legend(ymd_hms("2014-03-19 11:10:25"),250, c("Scanmar", "Marport"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue","red"))


# Export into csv file
write.table(r, file= "scanmarhh.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# create a new data.frame with raw data, performing an operation for each set using a for loop
ids = c('NED2014101.5', 'NED2014101.6')
r = data.frame( cbind( id=ids, n = NA, d.perc = NA, ), stringsAsFactors = FALSE )  

for (i in 1:nrow(r)){
  test = which(master$id==r[i,"id"])
  scanmar1 = master[test, ]
  
  r$n[i]=length(test)
  r$no.door[i] = length(which( is.finite(scanmar1$doorspread)))
  
# save r dataframe
file="roll.summary.RData"
save(r, file="roll.summary.RData", compress=T)
load("roll.summary.RData")

# Name the variables in a data.frame
colnames(roll) = c("mission", "proll", "sroll", "timestamp", "set", "wingspread", "doorspread", "opening")

# Create a histogram
require(lattice)
histogram( ~ marport.RollDoorPort | marport.mission, data=roll[which(roll$marport.mission %in% 
                      c("NED2014101.27", "NED2014101.28") ), ], main = "Port Door Roll Angle", xlab="Angle")

# Create a box and whiskers
require(lattice)
bwplot( ~ marport.RollDoorPort | marport.mission, data=roll[which(roll$marport.mission %in% 
              c("NED2014101.5", "NED2014101.6", ) ), ] )

# Plot an abline
# vertical
abline (v=5, col=mcol, lty="dotted")
# horizontal
abline (h=5, col=mcol, lty="dotted")

# Plot points
points( depth~ts, x, pch=20, col="blue", cex=0.2)

# How to subset a variable
master$date = substring(as.character(master$timestamp), 1,10)

# Creating a linear model
lm.w100 = lm(doorspread~depth, data=d0)
summary(lm.w100)

# filter
x = marport$depth1
i = which( (x > 750) | (x < 0) ) 
marport = marport [-i, ]  
 
# Plotting a for loop
allids=unique(modern.data$id)
i=sample(1:length(allids),10)
allids=allids[i]

for (id in allids){
  
  test=which(modern.data$id==id)
  i=modern.data[test,]
  d=range(i$depth, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(depth~timestamp,i, type = "p", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
  }
}

or

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

#setwd
setwd("C:/cygwin64/home/mundenj/work")

# Subsetting a variabel when it is NA
ed = which(!is.finite(gs$edate))
ied = gs[ed,]
head(ied)

#Summaries
xtabs(~year,filtered)

# Select for those sets < 23 mins
i = which(gs$bottom_duration < 23*60)
gs[i,]
gs[i,"id"]
