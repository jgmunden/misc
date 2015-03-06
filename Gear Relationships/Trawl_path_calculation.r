trawl.width.model = function (master) {

## Generating a data frame (r) with the percent of doorspread and wingspread values per set (id) 
  allids=unique(master$id)
  
  allids=unique(modern.data$id)
  allids=unique(historical.data$id)
  length(allids)
  
  # create a null df which the for loop below will populate
  r = data.frame(id = allids, d.perc = NA, w.perc = NA, stringsAsFactors = FALSE)

  # To calculate the percent data generated for wingspread and doorspread per set and insert them into a df (r)
  for (i in 1:nrow(r)){
    test = which(master$id==r[i,"id"])
    mm = master[test, ]
    n=length(test)
    dsval = length(which( is.finite(mm$doorspread)))
    r$d.perc[i] = (dsval/n)*100
    wsval = length(which( is.finite(mm$wingspread)))
    r$w.perc[i] = (wsval/n)*100
  }
  
  # save r dataframe
  file="r.perc.RData"
  save(r, file="r.perc.RData", compress=T)
  load("r.perc.RData")

  # Remove zeros froms from d.perc and w.perc
  i = which(r$d.perc == 0)
  b = which(r$w.perc == 0)
  # Determine the corresponding ID's to i
  f = subset(which[master$id == i])

  # Summaries and boxplots with the zeros removed
  summary(r$d.perc[-i])
  summary(r$w.perc[-b])
  boxplot(r$d.perc[-i], main = "percent observations for doorspread")
  boxplot(r$w.perc[-b], main = "percent observations for wingspread")

## create df (r.d.filter) which contains set(id) and d.perc when d.perc does not equal zero
  r.d.filter = data.frame(id = allids[-i], d.perc = r$d.perc[-i])
  # create df (r.w.filter) which contains set(id) and w.perc when w.perc does not equal zero
  r.w.filter = data.frame(id = allids[-b], w.perc = r$w.perc[-b])
  # create df (r.dw.filter) which contains set(id), d.perc and w.perc, with no zero's incld
  r.dw.filter = merge(r.d.filter, r.w.filter, by = "id") 
  
  # select those sets with >= 50 d.perc and w.perc
  r.dw.filter.50 = NULL
  r.dw.filter.50 = r.dw.filter[which(r.dw.filter$d.perc >= 50 & r.dw.filter$w.perc >= 50),]
  summary(r.dw.filter.50)
  
  # Select the sets listed in df(r.dw.filter.50) and make a new df which these sets are subsetted from master (tdw)
  allids2=r.dw.filter.50$id
  tdw <- master[master$id == allids2, ]

## Calculate the linear model
  dw.lm = lm(wingspread~doorspread, data=tdw)
  summary(dw.lm)

# save linear model
file="wing.door.lm.RData"
save(dw.lm, file="wing.door.lm.RData", compress=T)
load("wing.door.lm.RData")

# save tdw
file="tdw.RData"
save(tdw, file="tdw.RData", compress=T)
load("tdw.RData")


## Plot the relationship
  plot(wingspread~doorspread, data=tdw, main= "Doorspread vs Wingspread", pch=19)

str(tdw)
# form data sets by year
com =tdw[which(tdw$year == 2013) , ]
com0=tdw[which(tdw$year == 2014) , ]
com1=tdw[which(tdw$year == 2012) , ]
com2=tdw[which(tdw$year == 2011) , ]
com3=tdw[which(tdw$year == 2010) , ]
com4=tdw[which(tdw$year == 2009) , ]
com5=tdw[which(tdw$year == 2008) , ]
com6=tdw[which(tdw$year == 2007) , ]
com7=tdw[which(tdw$year == 2006) , ]
com8=tdw[which(tdw$year == 2005) , ]
com9=tdw[which(tdw$year == 2004) , ]

points(wingspread~doorspread, com, col="green", pch=20)
points(wingspread~doorspread, com0, col="blue", pch=20)
points(wingspread~doorspread, com1, col="purple", pch=20)
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



# wingspread and headline height
plot(opening~wingspread, tdw, main= "Wingspread vs Headline Height", pch=19)
points(opening~wingspread, d1, col="red", pch=20)
points(opening~wingspread, d2, col="green", pch=20)
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