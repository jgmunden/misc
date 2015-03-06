# Need to determine the normal variability of wingpsread and doorspread within a tow so that when we conduct the swept area estimates
## we know what is most likely real data and what is most likely noise.

load("filteredlm.RData")
load( "C:/cygwin64/home/mundenj/work/filteredlm.Rdata")
setwd("C:/cygwin64/home/mundenj/work")


# test small sampple of ids to see if the code works
r.samp=filteredlm[sample(1:nrow(filteredlm), 10, replace=FALSE),]
ids = r.samp$id


# create a new d.f which has mission and the mean and standard deviation of wingspread, doorspread, depth and headline height
ids = unique(filteredlm$id)
spread.v = data.frame( cbind( id=ids, 
                       n = NA, mean.depth = NA, depth.sd = NA, mean.wingspread = NA, wing.sd = NA, 
                       mean.doorspread=NA, door.sd = NA, mean.hheight=NA, hh.sd = NA ), stringsAsFactors = FALSE)  

                
                for (i in 1:nrow(spread.v)){
                  test = which(filteredlm$id==spread.v[i,"id"])
                  test1 = filteredlm[test, ]
                  
                    spread.v$n[i]=length(test)
                    spread.v$mean.depth[i] = mean(test1$depth, na.rm = TRUE)
                    spread.v$depth.sd[i]   = sd(test1$depth, na.rm=TRUE)
                    spread.v$mean.wingspread[i] = mean(test1$wingspread, na.rm = TRUE)
                    spread.v$wing.sd[i] = sd(test1$wingspread, na.rm=TRUE)
                    spread.v$mean.doorspread[i] = mean(test1$doorspread, na.rm = TRUE)
                    spread.v$door.sd[i] = sd(test1$doorspread, na.rm=TRUE)
                    spread.v$mean.hheight[i] = mean(test1$opening, na.rm = TRUE)
                    spread.v$hh.sd[i] = sd(test1$opening, na.rm=TRUE)
                }

                  # Change format to numeric
                  spread.v$mean.depth=as.numeric(spread.v$mean.depth)
                  spread.v$depth.sd=as.numeric(spread.v$depth.sd)
                  spread.v$mean.wingspread=as.numeric(spread.v$mean.wingspread)
                  spread.v$wing.sd=as.numeric(spread.v$wing.sd)
                  spread.v$mean.doorspread=as.numeric(spread.v$mean.doorspread)
                  spread.v$door.sd=as.numeric(spread.v$door.sd)
                  spread.v$mean.hheight=as.numeric(spread.v$mean.hheight)
                  spread.v$hh.sd=as.numeric(spread.v$hh.sd)
                  
                  # add year
                  spread.v$year = substring(as.character(spread.v$id), 4,7)
                 
                  file="spread.v.RData"
                  save(spread.v, file="spread.v.RData", compress=T)
                  load("spread.v.RData")



# Subset by year
year14=spread.v[which(spread.v$year == 2014) , ]
year13=spread.v[which(spread.v$year == 2013) , ]
year12=spread.v[which(spread.v$year == 2012) , ] 
year11=spread.v[which(spread.v$year == 2011) , ]
year10=spread.v[which(spread.v$year == 2010) , ]
year09=spread.v[which(spread.v$year == 2009) , ]
year08=spread.v[which(spread.v$year == 2008) , ]
year07=spread.v[which(spread.v$year == 2007) , ]
year06=spread.v[which(spread.v$year == 2006) , ]
year05=spread.v[which(spread.v$year == 2005) , ]
year04=spread.v[which(spread.v$year == 2004) , ]
year90=spread.v[which(spread.v$year == 1990) , ]
year91=spread.v[which(spread.v$year == 1991) , ]

# subset by groups of years
post = spread.v[which(spread.v$year %in% 2013:2014) , ]
pre = spread.v[which(spread.v$year %in% 2004:2012) , ]

# Plotting 
  # wingspread and depth
  plot(mean.wingspread~mean.depth, spread.v, pch=19, cex=0.5)
  # two separate curves
  points(mean.wingspread~mean.depth, post, pch=19, cex=0.5, col="blue")
  points(mean.wingspread~mean.depth, pre, pch=19, cex=0.5, col="orange")
  
  # more recent curve (post)
  plot(mean.wingspread~mean.depth, post, pch=19, cex=0.5)  
  # Modelling values with Loess
  mean.depth = seq(0, 600, by=10)
  y.loess <- loess(mean.wingspread~mean.depth, span=0.25, data=post)
  y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  lines(y.predict~depth, col="blue")
  # plot the curve
  plot(y.predict~depth, col="blue", type='l', lwd=2.5)

  # secondary curve (pre)
  y.loess <- loess(mean.wingspread~mean.depth, span=0.25, data=pre)
  y1.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  lines(y1.predict~depth, col="red", lwd=2.5)
  text(50, 18.5, "Wingspread")
  legend(500,18, c("2013 & 2014", "2004-2012"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("blue", "red"))




  # doorspread and depth
  y.loess <- loess(mean.doorspread~mean.depth, span=0.25, data=post)
  y2.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  # plot the curve
  plot(y2.predict~depth, col="blue", type='l', lwd=2.5)

  # secondary curve (pre)
  y.loess <- loess(mean.doorspread~mean.depth, span=0.25, data=pre)
  y3.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  lines(y3.predict~depth, col="red", lwd=2.5)

  legend(500,60, c("2013 & 2014", "2004-2012"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("blue", "red"))
  text(20, 70, "Doorspread")



  # headline and depth
  y.loess <- loess(mean.hheight~mean.depth, span=0.25, data=post)
  y4.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  # plot the curve
  lines(y4.predict~depth, col="blue", lwd=2.5)
  
  # secondary curve (pre)
  y.loess <- loess(mean.hheight~mean.depth, span=0.25, data=pre)
  y5.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  plot(y5.predict~depth, col="red", lwd=2.5, type='l', ylim=c(2.8, 4.0))
  text(130, 3.8, "Headline height")  
  legend(500,3.4, c("2013 & 2014", "2004-2012"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("blue", "red"))



# Looking at standard deviation

hist(spread.v$door.sd, breaks=45, col="light blue", border= "dark blue")
hist(spread.v$wing.sd, breaks=45, col="light blue", border= "dark blue")
hist(spread.v$hh.sd, breaks=45, col="light blue", border= "dark blue")

require(lattice)

# wingspread
bwplot(spread.v$wing.sd)
quantile(spread.v$wing.sd, na.rm=TRUE)
# 75% of the data with a s.d of 1.46

# doorspread
bwplot(spread.v$door.sd)
quantile(spread.v$door.sd, na.rm=TRUE)
# 75% of the data with a s.d of 9.46

# headline height
bwplot(spread.v$hh.sd)
quantile(spread.v$hh.sd, na.rm=TRUE)
# 75% of the data with a s.d of 0.81

