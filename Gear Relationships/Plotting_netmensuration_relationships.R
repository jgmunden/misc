
  # Data frames which include ALL scanmar data, not filtered for bottom contac
  load("goose.RData") # those where there are 50% or more number of data measurements for headline and wingspread 
  load("duck.RData") # those where there are 50% or more number of data measurements for doorspread and wingspread
  
  # Data that has been filtered to include only measurements considered part of fishing time
  filtered = net_mensuration.db( DS="scanmar.filtered",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
  # filtered data put through the linear model
  load("filteredlm.RData")

  # Born out of gsinf, includes the start and end times indicated by winch action, and our estimate
  gs = net_mensuration.db( DS="bottom.contact",  net.root.dir=net.root.dir )  # bring in estimates of bottom contact times from scanmar
  
  require(lattice)
  
  
  # coarse gating of the filtered data to uliminate measurements that seems unlrealistic for when a trawl is fishing on bottom
  um = which( filtered$wingspread < 7) 
  filtered$wingspread[ um] =NA
  
  un = which( filtered$doorspread < 20) 
  filtered$doorspread[ un] =NA
  
    require(lubridate)
    require(mgcv)
    filtered$year = year( filtered$timestamp )
    filtered$good = TRUE
------------------------------------------------------------------------------------------------  
  # Filtering of the doorspread and wingspread data using linear models
    yrs = sort(unique(filtered$year))  
    for (y in yrs){  
      yi =which(filtered$year==y)  
      # first pass -- a linear model to quickly determine large residuals
      good = which (! is.na( filtered$doorspread[yi] + filtered$wingspread[yi] ))
      print(paste(length(good), y))    
      if (length(good) < 30 ) next()
      dw = lm( wingspread ~ doorspread, data=filtered[yi, ], na.action="na.exclude" )
      # hist(dw$residuals, "fd")
      dwr = residuals( dw )
      q.resids = quantile( dwr, probs=c(0.025, 0.975), na.rm=TRUE )
      good = which( dwr < q.resids[1] | dwr > q.resids[2] )
      dwg = rep(TRUE, length(dwr))
      dwg[good] = FALSE
      filtered$good[yi] = dwg 
    }


          # Plot the whole data.frame filtered after the linear model has furthered limited the data
          plot(wingspread~doorspread,data=filtered, type = "p", col = "light grey", pch=".")
          cols = c("green", "blue", "purple", "pink", "orange", "yellow", "light blue", "red", "brown", "lavender", "forest green", "blue") 
          yrs = c(1990:1992, 2004:2014) 
          for (i in 1:length(yrs)) {
            yr = yrs[i]
            col=cols[i]
            points(wingspread~doorspread, filtered[filtered$year==yr & filtered$good,], pch=".", col = col)
          }
  
    # Data.frame that includes only the data that passed the linear model  
     filteredlm =filtered[filtered$good,]
      head(filteredlm)
      unique(filteredlm$year)
    --------------------------------------------------------------------------------------------------------------------------------------  
          # two time frames, post 2013, and pre 2013
          post =filtered[which(filtered$year %in% 2013:2014) , ]
          pre=filtered[which(filtered$year %in% 2004:2011) , ]
          plot(wingspread~doorspread,data=post, type = "p", col = "red", pch="19", cex=0.5)
          points(wingspread~doorspread,data=pre, type = "p", col = "grey", pch="19", cex=0.5)
            legend(80,15, c("2004-2012", "2013 & 2014"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("grey","red"))
        
          # two time frames, 2013 & 2014
          f13 =filtered[which(filtered$year == 2013) , ]
          f14=filtered[which(filtered$year == 2014) , ]
          plot(wingspread~doorspread,data=f13, type = "p", col = "purple", pch=".")
          points(wingspread~doorspread,data=f14, type = "p", col = "black", pch=".")
            legend(80,15, c("2013", "2014"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("purple","black"))
          
          # 2012
          f12 =filteredlm[which(filteredlm$year == 2012) , ]
          points(wingspread~doorspread,data=f12, type = "p", col = "purple", pch=".")
          plot(wingspread~doorspread,data=pre, type = "p", col = "grey", pch=".")
          points(wingspread~doorspread,data=post, type = "p", col = "light green", pch=".")
            legend(80,15, c("2012", "2004-2011", "2013-2014"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("purple","grey", "green"))
  
  
                    # Calculations for powerpoint presentation
                    mean(f12$depth, na.rm=TRUE)
                    mean(pre$depth, na.rm=TRUE)
                    mean(f12$wingspread, na.rm=TRUE)
                    mean(pre$wingspread, na.rm=TRUE) 
                    mean(f12$doorspread, na.rm=TRUE)
                    mean(pre$doorspread, na.rm=TRUE)
 ---------------------------------------------------------------------------------------------------------- 
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
  
  
  # Random sample of filtered
  r.samp=filtered[sample(1:nrow(filtered), 250000, replace=FALSE),]
  yr14=r.samp[which(r.samp$year == 2014) , ]
  yr13=r.samp[which(r.samp$year == 2013) , ]
  yr12=r.samp[which(r.samp$year == 2012) , ] 
  yr11=r.samp[which(r.samp$year == 2011) , ]
  yr10=r.samp[which(r.samp$year == 2010) , ]
  yr09=r.samp[which(r.samp$year == 2009) , ]
  yr08=r.samp[which(r.samp$year == 2008) , ]
  yr07=r.samp[which(r.samp$year == 2007) , ]
  yr06=r.samp[which(r.samp$year == 2006) , ]
  yr05=r.samp[which(r.samp$year == 2005) , ]
  yr04=r.samp[which(r.samp$year == 2004) , ]
  yr90=r.samp[which(r.samp$year == 1990) , ]
  yr91=r.samp[which(r.samp$year == 1991) , ]
  
  # Filtered data frame that passed through the linear model
  r.samp=filteredlm[sample(1:nrow(filteredlm), 100000, replace=FALSE),]
  flm14=r.samp[which(r.samp$year == 2014) , ]
  flm13=r.samp[which(r.samp$year == 2013) , ]
  flm12=r.samp[which(r.samp$year == 2012) , ] 
  flm11=r.samp[which(r.samp$year == 2011) , ]
  flm10=r.samp[which(r.samp$year == 2010) , ]
  flm09=r.samp[which(r.samp$year == 2009) , ]
  flm08=r.samp[which(r.samp$year == 2008) , ]
  flm07=r.samp[which(r.samp$year == 2007) , ]
  flm06=r.samp[which(r.samp$year == 2006) , ]
  flm05=r.samp[which(r.samp$year == 2005) , ]
  flm04=r.samp[which(r.samp$year == 2004) , ]
  flm90=r.samp[which(r.samp$year == 1990) , ]
  flm91=r.samp[which(r.samp$year == 1991) , ]
----------------------------------------------------------------------------------------------------------  

          # Look at the doorspread/wingspread curve for each year to see if trend remains consistent 
          unique(com14$trip) # 2014
            t2 =com14[which(com14$trip == 2) , ]
            plot(wingspread~doorspread, t2, col="blue", cex=0.5)
            t101 = com14[which(com14$trip == 101) , ]
            points(wingspread~doorspread, t101, col="green", cex=0.5)
            legend(80,15, c("2", "101"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("blue","green"))
            
          unique(com13$trip) # 2013
            t22 =   com13[which(com13$trip == 22) , ]
            plot(wingspread~doorspread, t22, col="blue", cex=0.5)
            t28 = com13[which(com13$trip == 28) , ]
            points(wingspread~doorspread, t28, col="purple", cex=0.5)
            legend(80,15, c("22", "28"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("blue","purple"))
            
          unique(com12$trip) # 2012
            t2 =  com12[which(com12$trip == 2) , ]
            plot(wingspread~doorspread, t2, pch=".")
          
          unique(com11$trip) # 2011 (no good data)
            t2 =com11[which(com11$trip == 2) , ]
            points(wingspread~doorspread, t2, col="red", cex=0.5)
            t28 = com11[which(com11$trip == 28) , ]
            points(wingspread~doorspread, t28, col="green", cex=0.5)
            legend(80,15, c("22", "28"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("blue","green"))
            
          unique(com10$trip) # 2010 (no good data in 2)
            t27 = com10[which(com10$trip == 27) , ]
            plot(wingspread~doorspread, t27, col="green", cex=0.2)
            legend(80,15, c("22", "28"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("red","green"))
            
          unique(com09$trip) # 2009 (no good data in 2)
            t27 = com09[which(com09$trip == 27) , ]
            plot(wingspread~doorspread, t27, col="green", cex=0.5)
            
          unique(com08$trip) # 2008
            t775 = com08[which(com08$trip == 775) , ]
            points(wingspread~doorspread, t775, col="green", cex=0.5)
            t830 = com08[which(com08$trip == 830) , ]
            plot(wingspread~doorspread, t830, col="orange", cex=0.5)
            legend(80,15, c("775", "830"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("green","orange"))
            
          unique(com07$trip) # 2007
            t745 = com07[which(com07$trip == 745) , ]
            plot(wingspread~doorspread, t745, col="green", cex=0.5)
            t685 = com07[which(com07$trip == 685) , ]
            points(wingspread~doorspread, t685, col="orange", cex=0.5)
            legend(80,15, c("745", "685"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("green","orange")) 
            
          unique(com06$trip) # 2006
            t30 = com06[which(com06$trip == 30) , ]
            plot(wingspread~doorspread, t30, col="green", cex=0.5)
            t614 = com06[which(com06$trip == 614) , ]
            points(wingspread~doorspread, t614, col="orange", cex=0.5)
            legend(80,15, c("30", "614"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("green","orange")) 
            
          unique(com05$trip) # 2005
            t1 = com05[which(com05$trip == 1) , ]
            points(wingspread~doorspread, t1, col="green", cex=0.5)
            t545 = com05[which(com05$trip == 545) , ]
            points(wingspread~doorspread, t545, col="orange", cex=0.5)
            t605 = com05[which(com05$trip == 605) , ]
            points(wingspread~doorspread, t605, col="yellow", pch=19, cex=0.5)
            t633 = com05[which(com05$trip == 633) , ]
            plot(wingspread~doorspread, t633, col="purple", cex=0.7, main = "2005")
            legend(80,15, c("605", "633"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("yellow", "purple"))    
            
          unique(com04$trip) # 2004
            t529 = com04[which(com04$trip == 529) , ]
            plot(wingspread~doorspread, t529, col="green", cex=0.5, main = 2004)
            t530 = com04[which(com04$trip == 530) , ]
            points(wingspread~doorspread, t530, col="purple", cex=0.5)
            legend(80,10, c("529", "530"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("green", "purple"))
            
          unique(com91$trip) # 1991
            t133 = com91[which(com91$trip == 133) , ]
            plot(wingspread~doorspread, t133, col="green", cex=0.5, main = "historical data")
            
          unique(com90$trip) # 1990
            t231 = com90[which(com90$trip == 231) , ]
            points(wingspread~doorspread, t231, col="purple", cex=0.5, main = "historical data")
  
---------------------------------------------------------------------------------------------------------------------  
  
  
        # Exploring the different curves for the year 2012, trip 002
        t2 =   com12[which(com12$trip == 2) , ]
        s1 =   t2[which(t2$set %in% 1:85) , ]
        s2 =   t2[which(t2$set %in% 90:100) , ]
        s3 =   t2[which(t2$set %in% 101:142) , ]
      
  
        s889 = t2[which(t2$set %in% 88:89) , ]
        s867 = t2[which(t2$set %in% 86:87) , ]        
          
            # Doorspread vs Wingspread
            plot(wingspread~doorspread, t2, pch=".", col="grey")
            
            points(wingspread~doorspread, s1, col="blue", pch=".")
            points(wingspread~doorspread, s2, col="green", pch=".")
            points(wingspread~doorspread, s3, col="red", pch=".")
  
            points(wingspread~doorspread, s889, col="blue", pch=".")
            points(wingspread~doorspread, s867, col="green", pch=".")
            
            # Latitude vs Longitude
      
            legend(70,10, c("sets 1-85","sets 91-100","sets 101-141"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("blue","green", "red"))
      
            # Plot with land
            loadfunctions("polygons")
            require(PBSmapping)
            land = importPolys(polygon.ecomod("worldLR.ll"))
            
            plotMap(land, xlim=c(-70, -60), ylim=c(39, 47),density=0)
            points(latitude~longitude, t2, pch=19, cex=0.4, col="grey")
            points(latitude~longitude, s1, col="blue", pch=22, cex=0.4)
            points(latitude~longitude, s867, col="green", pch=22, cex=0.4)
            points(latitude~longitude, s889, col="blue", pch=22, cex=0.4)
            points(latitude~longitude, s2, col="green", pch=22, cex=0.4)
            points(latitude~longitude, s3, col="red", pch=22, cex=0.4)
            title = "2012 Set Locations"
  
  legend("bottomright", c("sets 2-85, 88 & 89","sets 91-100, 86 & 87","sets 101-141"), pch=22, col=c("blue","green", "red"))
  
      
----------------------------------------------------------------------------------------------------------------------
# Depth investigations
    r.samp=filteredlm[sample(1:nrow(filteredlm), 50000, replace=FALSE),]
    summary(r.samp)  
  
  # Investigate whether depth is the discriminating factor: create a data frame with x= depth, y= w/d (spread)
  # Not a pretty graph, hard to make any sense of it but I think we can take away that depth is not the discriminating factor  
  com14$spread = (com14$wingspread/com14$doorspread)
  com14$depth.f = (com14$depth/100)
      plot(spread~depth, com14, pch=19, cex=0.5, ylim=c(0,1))
        points(spread~depth, s3, col="blue", pch=22, cex=0.5)
        points(spread~depth, s57, col="green", pch=22, cex=0.5)
        points(spread~depth, s103, col="purple", pch=22, cex=0.5)
        points(spread~depth, s95, col="yellow", pch=22, cex=0.5)
        points(spread~depth, s141, col="pink", pch=22, cex=0.5)
        points(spread~depth, s68, col="forest green", pch=22, cex=0.5)
        points(spread~depth, s17, col="grey", pch=22, cex=0.5)
        points(spread~depth, s87, col="orange", pch=22, cex=0.5)
          legend(400,0.6, c("3", "57","103", "95", "141", "68", "17", "87"), lty=c(1,1), 
               lwd=c(2.5,2.5), col=c("blue","green", "purple", "yellow", "pink", "forest green", "grey", "orange"))

  # Doorspread and depth
    plot(doorspread~depth, r.samp, pch=19, cex=0.3, main = "Depth, Doorspread", xlim=c(0,600))
      points(doorspread~depth, yr14, col="green", pch=19, cex=0.8)
      points(doorspread~depth, yr13, col="orange", pch=21, cex=1.2)
      points(doorspread~depth, yr12, col="red", pch=22, cex=0.85)
      points(doorspread~depth, yr11, col="blue", pch=25, cex=0.9)
      points(doorspread~depth, yr10, col="yellow", pch=20)
      points(doorspread~depth, yr09, col="grey", pch=20)
      points(doorspread~depth, yr08, col="light blue", pch=20)
      points(doorspread~depth, yr07, col="brown", pch=20)
      points(doorspread~depth, yr06, col="lavender", pch=20)
      points(doorspread~depth, yr05, col="forest green", pch=20)
      points(doorspread~depth, yr04, col="forest green", pch=20)
        legend(600,40, c("2014", "2013", "2011"), lty=c(1,1), 
             lwd=c(2.5,2.5), col=c("orange","green", "blue"))
  
  
  # Modelling doorspread predicted values with Loess
  plot(doorspread~depth, r.samp, pch=19, cex=0.3, main = "Depth, Doorspread", xlim=c(0,600), col="grey")
  depth = seq(0, 600, by=10)
  y.loess <- loess(doorspread ~ depth, span=0.25, data=r.samp)
  y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  lines(y.predict~depth, col="blue")
  legend(500,40, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c( "blue"))
  
  # Wingspread and depth
    plot(wingspread~depth, r.samp, pch=19, cex=0.3, xlim=c(0,400))
      points(wingspread~depth, yr14,pch=19, cex=0.5)
      points(wingspread~depth, yr13,pch=19, cex=0.5)
      points(wingspread~depth, yr12, pch=19, cex=0.5)
      points(wingspread~depth, yr11, col="yellow", pch=20)
      points(wingspread~depth, yr10,pch=19, cex=0.5)
      points(wingspread~depth, yr09, pch=19, cex=0.5)
      points(wingspread~depth, yr08, pch=19, cex=0.5)
      points(wingspread~depth, yr07, pch=19, cex=0.5)
      points(wingspread~depth, yr06, pch=19, cex=0.5)
      points(wingspread~depth, yr05, pch=19, cex=0.5)
      points(wingspread~depth, yr04, pch=19, cex=0.5)

        # Plot the standard
        # Exclude yr 6 and yr 3 for presentation
        abline(h=12.5, col = "red", lty="dotted")
        legend(500,40, c("standard = 12.5m", "predicted values"), lty=c(1,1), 
               lwd=c(2.5,2.5), col=c("red", "blue"))
  
      # Modelling wingspread predicted values wiht Loess
      plot(wingspread~depth, r.samp, pch=19, cex=0.3, col="grey", main="Wingspread/Depth", na.action="na.exclude" )
      depth = seq(0, 750, by=10)
      y.loess <- loess(wingspread ~ depth, span=0.25, data=r.samp)
      y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
      lines(y.predict~depth, col="blue")
  
  
        # Modelling predicted values of headline height with Loess
        plot(opening~depth, r.samp, pch=19, cex=0.5, ylim=c(1,10), col="grey",  main="Headline Height/Depth", na.action="na.exclude")
        depth = seq(0, 750, by=10)
        y.loess <- loess(opening ~ depth, span=0.25, data=r.samp)
        y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y.predict~depth, col="blue")
        legend(500,7, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))

        # Modelling predicted values of clearance with Loess
        plot(clearance~depth, r.samp, pch=19, cex=0.5, ylim=c(0,5), col="grey", xlim=c(0,400),  main="Clearance/Depth", na.action="na.exclude")
        depth = seq(0, 750, by=10)
        y.loess <- loess(opening ~ depth, span=0.25, data=r.samp)
        y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y.predict~depth, col="blue")
        legend(500,7, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))
------------------------------------------------------------------------------------------------------------------------
# Calculating mean spreads
    
  # Calculating mean wingspreads per year
  w14= mean(flm14$wingspread, na.rm=TRUE)
  w13 = mean(flm13$wingspread, na.rm=TRUE)
  w12= mean(flm12$wingspread, na.rm=TRUE)
  w11= mean(flm11$wingspread, na.rm=TRUE)
  w10= mean(flm10$wingspread, na.rm=TRUE)
  w09= mean(flm09$wingspread, na.rm=TRUE)
  w08= mean(flm08$wingspread, na.rm=TRUE)
  w07= mean(flm07$wingspread, na.rm=TRUE)
  w06= mean(flm06$wingspread, na.rm=TRUE)
  w05 = mean(flm05$wingspread, na.rm=TRUE)
  w04 = mean(flm04$wingspread, na.rm=TRUE)

  # Calculating mean doorspreads per year
  d14= mean(flm14$doorspread, na.rm=TRUE)
  d13 = mean(flm13$doorspread, na.rm=TRUE)
  d12= mean(flm12$doorspread, na.rm=TRUE)
  d11= mean(flm11$doorspread, na.rm=TRUE)
  d10= mean(flm10$doorspread, na.rm=TRUE)
  d09= mean(flm09$doorspread, na.rm=TRUE)
  d08= mean(flm08$doorspread, na.rm=TRUE)
  d07= mean(flm07$doorspread, na.rm=TRUE)
  d06= mean(flm06$doorspread, na.rm=TRUE)
  d05 = mean(flm05$doorspread, na.rm=TRUE)
  d04 = mean(flm04$doorspread, na.rm=TRUE)
  
  doorspread.means = c(d14, d13, d12, d11, d10, d09, d08, d07, d06, d05, d04)
  average.door = mean(doorspread.means, na.rm=TRUE)
  wingspread.means = c(w14, w13, w12, w11, w10, w09, w08, w07, w06, w05, w04)
  average.wing = mean(wingspread.means, na.rm=TRUE)
  years = c(2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004)
  
  plot(wingspread.means~years, pch=19)
  abline(h=14.35894, col="red", lwd = c(2.5))
  legend(2011,15.5, c("overall mean = 14.36m"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red"))
  title("Mean Annual Wingspreads")  plot(doorspread.means~years, pch=19)
  
  plot(doorspread.means~years, pch=19)
  abline(h=55.24157, col="red", lwd = c(2.5))
  legend(2011,52, c("overall mean = 55.24m"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("red"))
  title("Mean Annual Doorspreads")
  
---------------------------------------------------------------------------------------------  
# Trying to match up times in GS, Scanmar and our bc estimates
    
    
  # Need to make a new d.f. to figure out timestamps
  i=which(!is.finite(gs$bc0.datetime))
  gs = gs[ -i, ]
  r.samp=gs[sample(1:nrow(gs), 10, replace=FALSE),]
  listid = r.samp$id
  
  gs.time = data.frame(r.samp$id, r.samp$sdate, r.samp$edate, r.samp$bc0.datetime, r.samp$ bc1.datetime)
  head(gs.time)
  colnames(gs.time) =  c("id", "sdate", "edate", "bc0", "bc1")
  str(gs.time) 
  write.table(gs.time, file= "gs.time.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
  
  # individual sets from master
  test=master[which(master$id == "TEL2004530.96"), ]
  min(test$timestamp, na.rm = TRUE))
  max(test$timestamp, na.rm = TRUE))

  
  # Plot fishing time with winches vs fishing time with our methods
  # Take random sample of ids
  r.samp=modern.data[sample(1:nrow(modern.data), 10, replace=FALSE),]
  r.samp$id
  
  subgs = gs[which((gs$id == "NED2010002.31")), ]
  submaster = master[which((master$id == "NED2010002.31")), ]
  
  um = which(submaster$depth > 200) 
  submaster$depth[ um] =NA
  
  subfiltered = filtered[which((filtered$id == "NED2013028.51")), ]
  d=range(submaster$depth, na.rm=TRUE)
  plot(depth~timestamp,submaster, type = "p", lwd = 1, main =  "Trawl Profile", sub=id, ylim=c(d[2], d[1]))
  title("Trawl profile")
  points(depth~timestamp, subfiltered, col = "green")
  
  
  # Min and max time recorded by Scanmar
  min(submaster$timestamp)
  max(submaster$timestamp)
  
  # Force GSINF times to match Scanmar
  dated = force_tz(subgs$sdate, tzone = "UTC")
  dated
  abline(v = dated, col = "red")
  stdate = dated + (3*60*60)
  abline(v=stdate, col="red")
  
  datedd = force_tz(subgs$edate, tzone = "UTC")
  datedd
  abline(v = datedd, col = "red")
  endate = datedd + (3*60*60)
  abline(v=endate, col="red")
  
  # Force our calculated bottom times to match Scanmar
  bc = force_tz(subgs$bc0.datetime, tzone = "UTC")
  bc0 = bc + 60*60
  abline(v=bc0, col="blue")
  
  bc = force_tz(subgs$bc1.datetime, tzone = "UTC")
  bc1 = bc + 60*60
  abline(v=bc1, col="blue")
  
    legend(ymd_hms("2010-08-06 12:30:00"),40, c("Fishing time determined by the winch"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red"))
    legend(ymd_hms("2010-08-06 12:40:00"),40, c("Winch", "Algorithm"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red","blue"))
  

  # Comparing bottom time estimates, ours versus GSinf
  gs$gsbt = as.numeric((gs$edate - gs$sdate)/60)
  gs$bottom_duration = gs$bottom_duration/60
  gs$diff = abs(gs$gsbt - gs$bottom_duration)
  
  plot(gs$diff)
  hist(gs$diff)
  
  summary(gs$bottom_duration)
  summary(gs$gsbt)
------------------------------------------------------------------------  
  # Headline height investigations  
    
    r.samp=filteredlm[sample(1:nrow(filteredlm), 10, replace=FALSE),]
    plot(wingspread~opening, r.samp, na.rm=TRUE, pch=19, cex=0.3)
    plot(opening~timestamp, filteredlm)
    plot(opening~depth, r.samp, pch=19, cex=0.3)
    abline(h=4, col="red", lwd=2.5)
    abline(h=2, col="blue", lwd=2.5)
    abline(h=3, col="yellow", lwd=2.5)
    abline(h=5, col="purple", lwd=2.5)
    abline(h=6, col="green", lwd=2.5)
    title = main="Opening & depth profile"
     # depth bias has an effect on headline height as well, with increase depth, you have decreased headline height, which would
        # result in increased escapement of some roundfish (i.e. haddock)

  
  # Headline height and depth over the years
  plot(opening~depth, r.samp, pch=19, cex=0.4, ylim=c(1,10), col="grey")
  points(opening~depth, flm13, col="red", pch=19, cex=0.4)
  points(opening~depth, flm12, col="green", pch=19, cex=0.4)
  points(opening~depth, flm11, col="purple", pch=19, cex=0.4)
  points(opening~depth, flm10, col="pink", pch=19, cex=0.4)
  points(opening~depth, flm09, col="orange", pch=19, cex=0.4)
  points(opening~depth, flm08, col="forest green", pch=19, cex=0.4)
  points(opening~depth, flm07, col="light green", pch=19, cex=0.4)
  points(opening~depth, flm06, col="light blue", pch=19, cex=0.4)
  points(opening~depth, flm05, col="grey", pch=19, cex=0.4)
  points(opening~depth, flm04, col="violet", pch=19, cex=0.4)

  
  str(filteredlm)
  
