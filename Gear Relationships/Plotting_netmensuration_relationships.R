  # Update on data frames, after timestamp issue have been fixed, including only those sets which passed the SD threshold
  gs = scanmar.db( DS="bottom.contact", p=p)  # bring in estimates of bottom contact times from scanmar
  filtered = scanmar.db( DS="scanmar.filtered",  p=p )  # bring in estimates of bottom contact times from scanmar
  load("master.updated.RData")
  

  # Data frames which include ALL scanmar data, not filtered for bottom contac
  load("goose.RData") # those where there are 50% or more number of data measurements for headline and wingspread 
  load("duck.RData") # those where there are 50% or more number of data measurements for doorspread and wingspread
  
  # filtered data put through the linear model
  setwd("C:/cygwin64/home/mundenj/work")
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
          post =filteredlm[which(filteredlm$year %in% 2013:2014) , ]
          pre=filteredlm[which(filteredlm$year %in% 2004:2011) , ]
          plot(wingspread~doorspread,data=post, type = "p", col = "red", pch=".")
          points(wingspread~doorspread,data=pre, type = "p", col = "grey", pch=".")
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
          points(wingspread~doorspread,data=f12, type = "p", col = "blue", pch=".")
          plot(wingspread~doorspread,data=pre, type = "p", col = " darkgrey", pch=".")
          points(wingspread~doorspread,data=post, type = "p", col = "black", pch=".")
            legend(80,15, c("2012", "2004-2011", "2013-2014"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("blue","grey", "black"))
  
  
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
  r.samp=filteredlm[sample(1:nrow(filteredlm), 250000, replace=FALSE),]
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
            plot(wingspread~doorspread, t2, pch=".", col="grey", ylim=c(10,20), xlim=c(25, 80))
            
            points(wingspread~doorspread, s1, col="blue", pch=19, cex=0.1)
            points(wingspread~doorspread, s2, col="green", pch=19, cex=0.1)
            points(wingspread~doorspread, s3, col="red", pch=19, cex=0.1)
  
            points(wingspread~doorspread, s889, col="blue", pch=19, cex=0.1)
            points(wingspread~doorspread, s867, col="green", pch=19, cex=0.1)
            
            # Latitude vs Longitude
      
            legend(70,14, c("sets 1-85, 88-89","sets 91-100, 86-87","sets 101-141"), lty=c(1,1), 
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
    plot(doorspread~depth, r.samp, pch=19, cex=0.3, xlim=c(0,600))
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
  plot(doorspread~depth, r.samp, pch=19, cex=0.3, xlim=c(0,600), col="grey")
  depth = seq(0, 600, by=10)
  y.loess <- loess(doorspread ~ depth, span=0.25, data=r.samp)
  y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
  lines(y.predict~depth, col="blue")
  legend(500,40, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c( "blue"))
  text(30,80, "Doorspread")
  
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
        abline(h=12.5, col = "red", lty=2.5)
        legend(500,40, c("standard = 12.5m", "predicted values"), lty=c(1,1), 
               lwd=c(2.5,2.5), col=c("red", "blue"))
-------------------------------------------------------------------------------------------------------  
      # Modelling wingspread predicted values with Loess
      plot(wingspread~depth, r.samp, pch=19, cex=0.3, col="grey", na.action="na.exclude", xlim=c(0,600))
      depth = seq(0, 750, by=10)
      y.loess <- loess(wingspread ~ depth, span=0.25, data=r.samp)
      y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
      lines(y.predict~depth, col="blue")
      text(30,23, "Wingspread")
      abline(h=12.5, col = "red")
      legend(500,25, c("standard = 12.5m"), lty=c(1,1), 
            lwd=c(2.5,2.5), col=c("red"))
  
  
        # Modelling predicted values of headline height with Loess
        plot(opening~depth, r.samp, pch=19, cex=0.3, ylim=c(1,10), col="grey", xlim=c(0,600), na.action="na.exclude")
        depth = seq(0, 750, by=10)
        y.loess <- loess(opening ~ depth, span=0.25, data=r.samp)
        y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y.predict~depth, col="blue")
        legend(500,7, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))
        text(30,8, "Headline height")

        # Modelling predicted values of clearance with Loess
        plot(clearance~depth, r.samp, pch=19, cex=0.5, ylim=c(0,5), col="grey", xlim=c(0,400),  main="Clearance/Depth", na.action="na.exclude")
        depth = seq(0, 750, by=10)
        y.loess <- loess(opening ~ depth, span=0.25, data=r.samp)
        y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y.predict~depth, col="blue")
        legend(500,7, c("predicted values"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue"))
-----------------------------------------------------------------------------------------------------------------------  
  
        # Plotting depth ranges
        # 0-75
        d0=r.samp[which(r.samp$depth < 75 & r.samp$depth > 30), ]
        plot(doorspread~depth, d0, pch=19, cex=0.4, xlim=c(30, 75), main= "doorspread 0-75m")
        depth = seq(30, 70, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d0)
        y0.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y0.predict~depth, col="blue", lwd=2.5)
        lm.w75 = lm(doorspread~depth, data=d0)
        summary(lm.w75)
        
        # 0-100
        d1=r.samp[which(r.samp$depth < 100), ]
        plot(doorspread~depth, d1, pch=19, cex=0.4, xlim=c(20, 100), main= "doorspread 0-100m")
        depth = seq(0, 110, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d1)
        y.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y.predict~depth, col="blue", lwd=2.5)
        lm.w100 = lm(doorspread~depth, data=d0)
        summary(lm.w100)
  
        # 100-200
        d2=r.samp[which(r.samp$depth > 100 & r.samp$depth < 200), ]
        plot(doorspread~depth, d2, pch=19, cex=0.4, xlim=c(100, 200), main= "doorspread 100-200m")
        depth = seq(100, 200, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d2)
        y1.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y1.predict~depth, col="blue", lwd=2.5)
      
        # 200-300
        d3=r.samp[which(r.samp$depth > 200 & r.samp$depth < 300), ]
        plot(doorspread~depth, d3, pch=19, cex=0.4, xlim=c(200, 300), main= "doorspread 200-300m")
        depth = seq(200, 300, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d3)
        y2.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y2.predict~depth, col="blue", lwd=2.5)
  
        # Only by 100m isn't enough to see trends
        d4=r.samp[which(r.samp$depth < 200 & r.samp$depth >30), ]
        plot(doorspread~depth, d4, pch=19, cex=0.4, xlim=c(20, 200), main= "doorspread < 300m")
        depth = seq(30, 200, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d4)
        y3.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y3.predict~depth, col="blue", lwd=2.5)
        lm.d100 = lm(doorspread~depth, data=d4)
        summary(lm.d100)
        
        # Look at the deepest depths
        d5=filteredlm[which(filteredlm$depth > 300), ]
        plot(doorspread~depth, filteredlm, pch=".", xlim=c(300, 750), main= "doorspread > 300m")
        depth = seq(300, 750, by=10)
        y.loess <- loess(doorspread ~ depth, span=0.25, data=d5)
        y4.predict <- predict(y.loess, newdata=data.frame(depth=depth))
        lines(y4.predict~depth, col="blue", lwd=2.5)
        
        #Linear model
        lm.w300 = lm(doorspread~depth, data=d4)
        summary(lm.w300)     
        plot(lm.w300)

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

  
  hist(filteredlm$opening, col="light blue", border="navy blue", xlab= "headline height", main="Headline height frequency")
---------------------------------------------------------------------  
# Looking at speed
# First filter to remove error values
x = filteredlm$ltspeed
i = which( (x > 10) | (x < 0) ) 
filteredlm = filteredlm [-i, ]  
r.samp=filteredlm[sample(1:nrow(filteredlm), 50000, replace=FALSE),]

# wingspread
plot(wingspread~ltspeed, com12, pch=19, cex=0.2, col="blue", main= "Speed and wingspread 2012")
  ltspeed = seq(0, 3.5, by=0.5)
  y.loess <- loess(wingspread ~ ltspeed, span=0.25, data=com12)
  y6.predict <- predict(y.loess, newdata=data.frame(ltspeed=ltspeed))
  lines(y6.predict~ltspeed, col="red", lwd=2.5)

# doorspread
  plot(doorspread~ltspeed, com12, pch=19, cex=0.2, col="blue", main= "Speed and doorspread 2012")
  ltspeed = seq(0, 3.5, by=0.1)
  y.loess <- loess(doorspread ~ ltspeed, span=0.25, data=com09)
  y7.predict <- predict(y.loess, newdata=data.frame(ltspeed=ltspeed))
  lines(y7.predict~ltspeed, col="red", lwd=2.5)  
  
# headline height
  plot(opening~ltspeed, com10, pch=19, cex=0.4, col="blue", main= "Speed and headline height 2010")
  ltspeed = seq(0, 3.5, by=0.1)
  y.loess <- loess(opening ~ ltspeed, span=0.25, data=com09)
  y8.predict <- predict(y.loess, newdata=data.frame(ltspeed=ltspeed))
  lines(y8.predict~ltspeed, col="red", lwd=2.5)  
  abline(h=4, col="green", lwd=3)  
  
  
  
  
hist(filteredlm$ltspeed, breaks = 40, col="green", main= "Speed frequency", xlab= "Speed (m/s)")
mean(filteredlm$ltspeed, na.rm=TRUE)
abline(v=3.5, col="dark grey", lwd = 2.5)
legend("topleft", c("Standard = 3.5 knots"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("grey"))
# Speed of trawl is far from what they think its at
# when do we have speed data?  
  
  summary(com14) #none
  summary(com13) #none  
  summary(com12) #yes
  summary(com11) #yes
  summary(com10) #yes
  summary(com09) #yes
  summary(com08) #none
  summary(com07) #none
  summary(com06) #none
  summary(com05) #none
  summary(com04) #none
  summary(com90) #none
  summary(com91) #none
  
# Look at speed between years
  # create data frame with only those years that have speed data
  speed=filteredlm[which(filteredlm$year == 2009:2012), ]
  unique(speed$year)
  
  # Generate sample
  allids=unique(speed$id)
  i=sample(1:length(allids),15)
  allids=allids[i]
  
  for (id in allids){
    
    test=which(speed$id==id)
    i=speed[test,] 
    if(any(is.finite(i$doorspread))){
      plot(doorspread~ltspeed,i, main =  "Trawl Geometry and Speed", sub=id, ylim=c(0,90), pch=19, cex=0.5 )
      points(wingspread~ltspeed, i, col="blue", pch=19, cex=0.5)
      points(opening~ltspeed, i, col="red", pch=19, cex=0.5)
      legend("topright", c("Doorspread", "Wingspread", "Headline height"), lty = c(1,1), lwd = c(2.5, 2.5), col=c("black", "red", "blue"))
      
    }
  }
  -----------------------------------------------------------------------------------------------------------------------------------------
  # Doorspread and headline height
    r.samp=filteredlm[sample(1:nrow(filteredlm), 250000, replace=FALSE),]
    plot(opening~doorspread, r.samp, pch=19, col="blue", cex=0.1)
    abline(h=2, col="red", lwd=2.5)
    abline(h=4, col="red", lwd=2.5)
    abline(h=5, col="red", lwd=2.5)
    abline(h=3, col="red", lwd=2.5)
    title("Doorspread and headline height")
