  # Load modern data
  load("m.data.RData")

  # data frame with NA removed from doorspread and wingspread
  masterdw = master[complete.cases(master[,4:5]),]
  
  # Remove zeros froms from d.perc and w.perc
  i = which(masterdw$doorspread == 0)
  b = which(masterdw$wingspread == 0)
  summary(masterdw$doorspread [-i])
  summary(masterdw$wingspread[-b])
  masterdw = masterdw[-i,]
  masterdw = masterdw[-b,]
  summary(masterdw)
  boxplot(masterdw$wingspread)
  
  file="masterdw.RData"
  save(masterdw, file="masterdw.RData", compress=T)
  load("masterdw.RData")
  
  ## Plot the relationship
  plot(wingspread~doorspread, masterdw)
  rrows = masterdw[sample(nrow(masterdw), 9000), ]
  plot(wingspread~doorspread, rrows)
  
  ids = unique(masterdw$id)
  mid = data.frame(cbind(id = ids, avdoorspread = as.numeric(NA), avwingspread = as.numeric(NA), avheadline = as.numeric(NA) ))
  str(mid)
  
  for (i in 1:nrow(mid)){
    test = which(master$id==mid[i,"id"])
    mid1 = master[test, ]
  
    mid$avdoorspread[i] = mean(master$doorspread)
    mid$avwingspread[i] = mean(master$wingspread)
    mid$avheadline[i] = mean(master$clearance)
  }

  # form data sets by year
  com =masterdw[which(masterdw$year == 2014) , ]
  com0=masterdw[which(masterdw$year == 2013) , ]
  com1=masterdw[which(masterdw$year == 2012) , ]
  com2=masterdw[which(masterdw$year == 2011) , ]
  com3=masterdw[which(masterdw$year == 2010) , ]
  com4=masterdw[which(masterdw$year == 2009) , ]
  com5=masterdw[which(masterdw$year == 2008) , ]
  com6=masterdw[which(masterdw$year == 2007) , ]
  com7=masterdw[which(masterdw$year == 2006) , ]
  com8=masterdw[which(masterdw$year == 2005) , ]
  com9=masterdw[which(masterdw$year == 2004) , ]
  
  plot(wingspread~doorspread, com, pch=".")

points(wingspread~doorspread, com, col="green", pch=20)
points(wingspread~doorspread, com0, col="blue", pch=20)
points(wingspread~doorspread, com1, col="purple")
points(wingspread~doorspread, com2, col="pink", pch=20)
points(wingspread~doorspread, com3, col="orange", pch=20)
points(wingspread~doorspread, com4, col="yellow", pch=20)
points(wingspread~doorspread, com5, col="grey", pch=20)
points(wingspread~doorspread, com6, col="red", pch=20)
points(wingspread~doorspread, com7, col="brown", pch=20)
points(wingspread~doorspread, com8, col="lavender", pch=20)
points(wingspread~doorspread, com9, col="forest green", pch=20)

# determine the trips included in 2013 and 2014
unique(com$trip)
unique(com0$trip)


# Legend
# for all years
legend(80,15.5, c("2014", "2013","2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("blue","green", "purple", "pink", "orange", "yellow", "grey", "red", "brown", "lavender", "forest green"))
# for 2014/2013
legend(80,15.5, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("blue","green", "purple", "pink"))

# trip
trip28 =com[which(com$trip == 28) , ]
trip22 =com[which(com$trip == 22) , ]

# 2010 trips (com3)
unique(com3$trip)

# 27 and 2
trip27= com3[which(com3$trip == 27) , ]
trip02= com3[which(com3$trip == 2) , ]
points(wingspread~doorspread, trip27, col="red", pch=20)
points(wingspread~doorspread, trip02, col="forest green", pch=20)
legend(68,7, c("NED2010027", "NED2010002"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("green","red"))
check11=master[which(master$year == 2011) , ]
unique(check11$trip)
check2=check11[which(check11$trip == 2) , ]
summary(check2)
write.table(check27, file= "Trip 2010027.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 
histogram( ~ doorspread | trip, data=check2[which(check2$trip == ("2") ), ],
           main = "Wingspread during the trip NED2011002", xlab="Wingspread", ylab="Doorspread")

# There are no wingspread measurements for trip NED2010002

plot(wingspread~doorspread, data=tdw, main= "Doorspread vs Wingspread", pch=19)
points(wingspread~doorspread, trip28, col="green", pch=20)
points(wingspread~doorspread, trip22, col="red", pch=20)
legend(68,7, c("NED2013028 (comparative)", "NED2013022"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("green","red"))



summary(trip22$timestamp)
# Still looks like the US trawl is included
# Comparative trips  NED2013028, NED2013002, NED2014102

com=master[which(tdw$year == 2013) , ]
com0=com[which(com$trip == 028) , ]
com1=master[which(master$year == 2014) , ]

unique(com$trip)
# 22 and 28
unique(com1$trip)
# 2 and 101


# form sets based on depth
plot(wingspread~doorspread, data=tdw, main= "Doorspread vs Wingspread", pch=19)
d1 = tdw[which(tdw$depth >= 75) , ]
summary(d1$depth)
d2 = tdw[which(tdw$depth < 75) , ]

# depth plots
plot(wingspread~doorspread, d1, col="purple", pch=20, main = "Doorspread vs Wingspread")
points(wingspread~doorspread, d2, col="pink", pch=20)
legend(80,15.5, c(">= 75 m", "< 75 m"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("purple","pink"))


  
# data frame with NA removed from doorspread, wingspread AND headline height
masterdwh = masterdw[complete.cases(masterdw[,7]),]
file="masterdwh.RData"
save(masterdwh, file="masterdwh.RData", compress=T)
load("masterdwh.RData")
  
  
# wingspread and headline height
plot(opening~wingspread, tdw, main= "Wingspread vs Headline Height", pch=19)
points(opening~wingspread, com, col="red", pch=20, cex=0.75)
points(opening~wingspread, com0, col="green", pch=19, cex=0.8)
points(opening~wingspread, com1, col="blue", pch=25)
points(opening~wingspread, com2, col="purple", pch=23, cex=1)
legend(21,8, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("green","red", "blue", "purple"))

d1 = tdw[which(tdw$depth >= 200) , ]
d2 = tdw[which(tdw$depth < 200) , ]
legend(21,8, c(">= 200 m", "< 200 m"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("red","green"))
# year
points(opening~wingspread, com, col="green", pch=20)
points(opening~wingspread, com0, col="blue", pch=20)
points(opening~wingspread, com1, col="purple", pch=20)
points(opening~wingspread, com2, col="pink", pch=20)
points(opening~wingspread, com3, col="orange", pch=20)
points(opening~wingspread, com4, col="yellow", pch=20)
points(opening~wingspread, com5, col="grey", pch=20)
points(opening~wingspread, com6, col="red", pch=20)
points(opening~wingspread, com7, col="brown", pch=20)
points(opening~wingspread, com8, col="lavender", pch=20)
points(opening~wingspread, com9, col="forest green", pch=20)
# legend 2013-2014
legend(21,8, c("2014", "2013"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("blue","green"))
# legend all years
legend(23.5,9.9, c("2014", "2013","2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("blue","green", "purple", "pink", "orange", "yellow", "grey", "red", "brown", "lavender", "forest green"))



# headline height and clearance
# use modern.data instead od tdw beacsue wingspread or doorspread is not used
plot(clearance~opening, tdw, main= "Clearance vs Headline Height", pch=19, ylim=c(1,5))
# woah too much data
plot(clearance~opening, modern.data, main= "Clearance vs Headline Height", pch=19)



# Now that we have the linear model, we can use it to solve for doorspread when 
  # we have sufficient wingspread data but not doorspread

# Create a priority where doorspread is used if there is enough data to calculate the average
  ## and if not wingspread is used to calculate doorspread through the relationship already established

# if neither variable has sufficient data, then the average trawlpath width cannot be calculated, comment is printed
  ## "not enough data for trawl spread calculation"


# trawl variables with depth
plot(doorspread~depth, tdw, pch=19, main = "Depth, Doorspread")
points(doorspread~depth, com, col="green", pch=19, cex=0.8)
points(doorspread~depth, com1, col="orange", pch=21, cex=1.2)
points(doorspread~depth, com2, col="red", pch=22, cex=0.85)
points(doorspread~depth, com3, col="blue", pch=25, cex=0.9)
points(doorspread~depth, com4, col="yellow", pch=20)
points(doorspread~depth, com5, col="grey", pch=20)
points(doorspread~depth, com6, col="light blue", pch=20)
points(doorspread~depth, com7, col="brown", pch=20)
points(doorspread~depth, com8, col="lavender", pch=20)
points(doorspread~depth, com9, col="forest green", pch=20)
legend(600,40, c("2014", "2013", "2011"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("orange","green", "blue"))

plot(wingspread~depth, tdw, pch=19, main = "Depth, Wingspread")
points(wingspread~depth, com, col="green", pch=19, cex=0.8)
points(wingspread~depth, com1, col="orange", pch=21, cex=1.2)
points(wingspread~depth, com2, col="red", pch=22, cex=0.85)
points(wingspread~depth, com3, col="blue", pch=25, cex=0.9)
points(wingspread~depth, com4, col="yellow", pch=20)
points(wingspread~depth, com5, col="grey", pch=20)
points(wingspread~depth, com6, col="light blue", pch=20)
points(wingspread~depth, com7, col="brown", pch=20)
points(wingspread~depth, com8, col="lavender", pch=20)
points(wingspread~depth, com9, col="forest green", pch=20)
legend(600,10, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("orange","green", "red", "blue"))

plot(opening~depth, tdw, pch=19, main = "Depth, Opening")
points(opening~depth, com, col="green", pch=19)
points(opening~depth, com1, col="orange", pch=19)
legend(600,8, c("2014", "2013"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("orange","green"))

plot(clearance~depth, tdw, pch=19, main = "Depth, Clearance")
points(clearance~depth, com, col="green", pch=19)
points(clearance~depth, com1, col="orange", pch=19)
legend(600,3, c("2014", "2013"), lty=c(1,1), 
       lwd=c(2.5,2.5), col=c("orange","green"))

