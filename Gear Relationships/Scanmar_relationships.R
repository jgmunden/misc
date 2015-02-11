# Plotting net mensuration relationships using SCANMAR data

  # Load data
    load("goose.RData") # those where there are 50% or more number of data measurements for headline and wingspread 
    load("duck.RData") # those where there are 50% or more number of data measurements for doorspread and wingspread
  
  
  # DOORSPREAD AND WINGSPREAD
  
  # form data sets by year
    x = duck
    com14=x[which(x$year == 2014) , ]
    com13=x[which(x$year == 2013) , ]
    com12=x[which(x$year == 2012) , ] 
    com11=x[which(x$year == 2011) , ]
    com10=x[which(x$year == 2010) , ]
    com09=x[which(x$year == 2009) , ]
    com08=x[which(x$year == 2008) , ]
    com07=x[which(x$year == 2007) , ]
    com06=x[which(x$year == 2006) , ]
    com05=x[which(x$year == 2005) , ]
    com04=x[which(x$year == 2004) , ]
    com90=x[which(x$year == 1990) , ]
    com91=x[which(x$year == 1991) , ]
    
    all.years = c(com14, com13, com12, com11, com10, com09, com08, com07, com06, com05, com04, com90, com91)
  
  # Separate the data by year and plot on the same graph
    points(wingspread~doorspread, com13, col="green", pch=".")
    points(wingspread~doorspread, com12, col="blue", pch=".")
    points(wingspread~doorspread, com11, col="purple", pch=".")
    points(wingspread~doorspread, com10, col="pink", pch=".")
    points(wingspread~doorspread, com09, col="orange", pch=".")
    points(wingspread~doorspread, com08, col="yellow", pch=".")
    points(wingspread~doorspread, com07, col="grey", pch=".")
    points(wingspread~doorspread, com06, col="red", pch=".")
    points(wingspread~doorspread, com05, col="brown", pch=".")
    points(wingspread~doorspread, com04, col="lavender", pch=".")
    points(wingspread~doorspread, com90, col="forest green", pch=".")
    points(wingspread~doorspread, com91, col="blue", pch=19)
    
    # Looking further into 2014
      unique(com14$trip) # Two trips in 2014: 2, 101 
    # Plot only one trip: NED2014002
      t2 =   com14[which(com14$trip == 2) , ]
    # Take a sample to better visualize
      t2 = com14[sample(1:nrow(com14), 10000), ]
    # Plot individual sets in 2014
      s.no = unique(t2$set)
      plot(wingspread~doorspread, t2, pch=19, cex=0.4)
      # Form data frames for randomly selected sets
        s5 =   t2[which(t2$set == 5) , ]
        s18 =   t2[which(t2$set == 18) , ]
        s27 =   t2[which(t2$set == 27) , ]
        s39 =   t2[which(t2$set == 39) , ]
        s41 =   t2[which(t2$set == 41) , ]
        s68 =   t2[which(t2$set == 68) , ]
        s17 =   t2[which(t2$set == 17) , ]
        s73 =   t2[which(t2$set == 73) , ]
      # Plot sets
        points(wingspread~doorspread, s5, col="blue", pch=22, cex=0.3)
        points(wingspread~doorspread, s18, col="green", pch=22, cex=0.3)
        points(wingspread~doorspread, s27, col="purple", pch=22, cex=0.3)
        points(wingspread~doorspread, s39, col="yellow", pch=22, cex=0.3)
        points(wingspread~doorspread, s41, col="pink", pch=22, cex=0.3)
        points(wingspread~doorspread, s68, col="forest green", pch=21, cex=0.3)
        points(wingspread~doorspread, s17, col="grey", pch=22, cex=0.3)
        points(wingspread~doorspread, s73, col="orange", pch=22, cex=0.3)
      
        legend(80,20, c("5", "18","27", "39", "41", "68", "17", "73"), lty=c(1,1), 
          lwd=c(2.5,2.5), col=c("blue","green", "purple", "yellow", "pink", "forest green", "grey", "orange"))
    
    
    
      # Looking further into 2012 as it seems perculiar, Only one trip (NED2012002) but there seems to be three separate relationships
      # Raw data (com12) has 268,615 oberavations
      # take a random sample to visualize better
        r.samp=com12[sample(1:nrow(com1), 10000, replace=FALSE),]
        plot(wingspread~doorspread, r.samp, pch=19, cex=0.5)
    
        # Form data frames for randomly selected sets
          s.no = unique(com12$set)
          s3 =   r.samp[which(r.samp$set == 3) , ]
          s57 =   r.samp[which(r.samp$set == 57) , ]
          s103 =   r.samp[which(r.samp$set == 103) , ]
          s95 =   r.samp[which(r.samp$set == 95) , ]
          s141 =   r.samp[which(r.samp$set == 141) , ]
          s68 =   r.samp[which(r.samp$set == 68) , ]
          s17 =   r.samp[which(r.samp$set == 17) , ]
          s87 =   r.samp[which(r.samp$set == 87) , ]
        # Plot the sets
          points(wingspread~doorspread, s3, col="blue", pch=22, cex=0.5)
          points(wingspread~doorspread, s57, col="green", pch=22, cex=0.5)
          points(wingspread~doorspread, s103, col="purple", pch=22, cex=0.5)
          points(wingspread~doorspread, s95, col="yellow", pch=22, cex=0.5)
          points(wingspread~doorspread, s141, col="pink", pch=22, cex=0.5)
          points(wingspread~doorspread, s68, col="forest green", pch=21, cex=0.5)
          points(wingspread~doorspread, s17, col="grey", pch=22, cex=0.5)
          points(wingspread~doorspread, s87, col="orange", pch=22, cex=0.5)
  
          legend(80,20, c("3", "57","103", "95", "141", "68", "17", "87"), lty=c(1,1), 
            lwd=c(2.5,2.5), col=c("blue","green", "purple", "yellow", "pink", "forest green", "grey", "orange"))
  
    
          # LATITUDE AND LONGITUDE (2012 cont.)
            
            # Randomly selected sets from NED2012002  
              idsll = c("NED2012002.3", "NED2012002.57", "NED2012002.103", "NED2012002.95", "NED2012002.141",
                      "NED2012002.68","NED2012002.17","NED2012002.87")
             # Use the start lat and longs from gsinf, subset gsinf using idsll
              gsll=gsinf[which((gsinf$id %in% idsll)  ), ]
              # Create edits to our new df (gsll), add set and multiply lon.end by -1
              gsll$set = substring(gsll$id, 12,14 )
              gsll$lon.end = (gsll$lon.end* -1) 
              # Plot all sets
              plot(lat~lon, gsll, xlim=c(-63,-69), ylim=c(40, 45))
              points(lat.end~lon.end, gsll)
              # Plot individual sets
                s3 =   gsll[which(gsll$set == 3) , ]
                s57 =   gsll[which(gsll$set == 57) , ]
                s103 =   gsll[which(gsll$set == 103) , ]
                s95 =   gsll[which(gsll$set == 95) , ]
                s141 =   gsll[which(gsll$set == 141) , ]
                s68 =   gsll[which(gsll$set == 68) , ]
                s17 =   gsll[which(gsll$set == 17) , ]
                s87 =   gsll[which(gsll$set == 87) , ]
              
              points(lat~lon, s3, col="blue", pch=19)
              points(lat.end~lon.end, s3, col="blue", pch=19)
            
              points(lat~lon, s57, col="green", pch=19)
              points(lat.end~lon.end, s57, col="green", pch=19)
            
              points(lat~lon, s103, col="purple", pch=19)
              points(lat.end~lon.end, s103, col="purple", pch=19)
            
              points(lat~lon, s95, col="yellow", pch=19)
              points(lat.end~lon.end, s95, col="yellow", pch=19)
            
              points(lat~lon, s141, col="pink", pch=19)
              points(lat.end~lon.end, s141, col="pink", pch=19)
              
              points(lat~lon, s68, col="forest green", pch=19)
              points(lat.end~lon.end, s68, col="forest green", pch=19)
            
              points(lat~lon, s17, col="grey", pch=19)
              points(lat.end~lon.end, s17, col="grey", pch=19)
            
              points(lat~lon, s87, col="orange", pch=19)
              points(lat.end~lon.end, s87, col="orange", pch=19)
              
              legend(-66,45, c("3", "57","103", "95", "141", "68", "17", "87"), lty=c(1,1), 
            
            
            # "SPREAD" AND DEPTH (2012 cont.)           
            
            # Create a df with spread, i.e. (wingspread/doorspread)
              com12$spread = (com12$wingspread/com12$doorspread)
            # Plot df with all sets  
              plot(spread~depth, com12, pch=19, cex=0.2, ylim=c(0,1))
            # Plots df with individual sets
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
            
           # DEPTH AND WINGSPREAD (2012 cont.)
           
           # Make a new df which selects the raw data (before 50% filtering) corresponding to the sets of interest
              mtest=master[which((master$id %in% idsll)  ), ]
              plot(wingspread~depth, mtest)
            # Make a d.f which corresponds to each set
              s3 =   mtest[which(mtest$set == 3) , ] 
              s57 =   mtest[which(mtest$set == 57) , ]
              s103 =   mtest[which(mtest$set == 103) , ]
              s95 =   mtest[which(mtest$set == 95) , ]
              s141 =   mtest[which(mtest$set == 141) , ]
              s68 =   mtest[which(mtest$set == 68) , ]
              s17 =   mtest[which(mtest$set == 17) , ]
              s87 =   mtest[which(mtest$set == 87) , ]
            # Plot each set
           
              points(wingspread~depth, s57, col="green", pch=19)
              points(wingspread~depth, s103, col="purple", pch=19)
              points(wingspread~depth, s95, col="yellow", pch=19)
              points(wingspread~depth, s141, col="pink", pch=19)
              points(wingspread~depth, s68, col="forest green", pch=19)
              points(wingspread~depth, s17, col="grey", pch=19)
              points(wingspread~depth, s87, col="orange", pch=19)
              legend(300,15, c("3", "57","103", "95", "141", "68", "17", "87"), lty=c(1,1), 
                lwd=c(2.5,2.5), col=c("blue","green", "purple", "yellow", "pink", "forest green", "grey", "orange"))
              

  

            # DEPTH
  
            plot(wingspread~doorspread, data=duck, main= "Doorspread vs Wingspread", pch=19)
            d1 = duck[which(duck$depth >= 75) , ]
            summary(d1$depth)
            d2 = duck[which(duck$depth < 75) , ]
            
            # Plotting depths, < 75 and >= 75
            plot(wingspread~doorspread, d1, col="purple", pch=20, main = "Doorspread vs Wingspread")
            points(wingspread~doorspread, d2, col="pink", pch=20)
            legend(80,15.5, c(">= 75 m", "< 75 m"), lty=c(1,1), 
                   lwd=c(2.5,2.5), col=c("purple","pink"))

  

  
  
  
  # WINGSPREAD AND HEADLINE 
  str(goose)
  # goose has 3,537,577 obs.
  r.samp = goose[sample(1:nrow(goose), 50000, replace = FALSE),]
  # form data sets by year
  yr =r.samp[which(r.samp$year == 2014) , ]
  yr0=r.samp[which(r.samp$year == 2013) , ]
  yr1=r.samp[which(r.samp$year == 2012) , ]
  yr2=r.samp[which(r.samp$year == 2011) , ]
  yr3=r.samp[which(r.samp$year == 2010) , ]
  yr4=r.samp[which(r.samp$year == 2009) , ]
  yr5=r.samp[which(r.samp$year == 2008) , ]
  yr6=r.samp[which(r.samp$year == 2007) , ]
  yr7=r.samp[which(r.samp$year == 2006) , ]
  yr8=r.samp[which(r.samp$year == 2005) , ]
  yr9=r.samp[which(r.samp$year == 2004) , ]
  yr10=r.samp[which(r.samp$year == 1990) , ]
  yr11=r.samp[which(r.samp$year == 1991) , ]
  plot(opening~wingspread, r.samp, main= "Wingspread vs Headline Height", pch=19, cex= 0.5)
  points(opening~wingspread, yr, col="red", pch=20, cex=0.75)
  points(opening~wingspread, yr0, col="green", pch=19, cex=0.8)
  points(opening~wingspread, yr1, col="blue", pch=25)
  points(opening~wingspread, yr2, col="purple", pch=23, cex=1)
  
  legend(21,8, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("green","red", "blue", "purple"))
  
  # Depth and Opening
  d1 = r.samp[which(r.samp$depth >= 200) , ]
  d2 = r.samp[which(r.samp$depth < 200) , ]
  plot(opening~wingspread, d1)
  points(opening~wingspread, d2, col= "red", pch=19)
  legend(21,8, c(">= 200 m", "< 200 m"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("red","green"))
  plot(opening~depth, r.samp, pch=19, cex=0.4)
  # year
  points(opening~depth, yr, col="green", pch=20)
  points(opening~depth, yr0, col="blue", pch=22)
  points(opening~depth, yr1, col="purple", pch=23)
  points(opening~depth, yr2, col="pink", pch=20)
  points(opening~depth, yr3, col="orange", pch=20)
  points(opening~depth, yr4, col="yellow", pch=20)
  points(opening~depth, yr5, col="grey", pch=20)
  points(opening~depth, yr6, col="red", pch=20)
  points(opening~depth, yr7, col="brown", pch=20)
  points(opening~depth, yr8, col="lavender", pch=20)
  points(opening~depth, yr9, col="forest green", pch=20)
  # legend 2011:2014
  legend(400,8, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("green","blue", "purple", "pink"))
  
  
  
  # headline height and clearance
  # use modern.data instead od tdw beacsue wingspread or doorspread is not used
  plot(clearance~opening,r.samp, main= "Clearance vs Headline Height", pch=19, ylim=c(1,5))
  plot(clearance~opening, r.samp, main= "opening vs Headline Height", pch=19, cex= 0.2)
  points(clearance~opening, yr, col="red", pch=20, cex=0.75)
  points(clearance~opening, yr0, col="green", pch=19, cex=0.8)
  points(clearance~opening, yr1, col="blue", pch=20, cex=0.75)
  points(clearance~opening, yr2, col="purple", pch=19, cex=1)

  
  
  
  
# DEPTH
  
  
  # trawl variables with depth
  plot(doorspread~depth, r.samp, pch=".", main = "Depth, Doorspread")
  points(doorspread~depth, yr, col="green", pch=".")
  points(doorspread~depth, yr0, col="light salmon", pch=".")
  points(doorspread~depth, yr1, col="orange", pch=".")
  points(doorspread~depth, yr2, col="red", pch=".")
  points(doorspread~depth, yr3, col="blue", pch=".")
  points(doorspread~depth, yr4, col="yellow", pch=".")
  points(doorspread~depth, yr5, col="grey", pch=".")
  points(doorspread~depth, yr6, col="light blue", pch=".")
  points(doorspread~depth, yr7, col="brown", pch=".")
  points(doorspread~depth, yr8, col="lavender", pch=".")
  points(doorspread~depth, yr9, col="forest green", pch=".")
  points(doorspread~depth, yr10, col="forest green", pch=".")
  points(doorspread~depth, yr11, col="steel blue", pch=".")
  legend(600,40, c("2014", "2013", "2011"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("orange","green", "blue"))
  
  plot(wingspread~depth, r.samp, pch=19, main = "Depth, Wingspread")
  points(wingspread~depth, yr, col="green", pch=19, cex=0.8)
  points(wingspread~depth, yr0, col="orange", pch=21, cex=1.2)
  points(wingspread~depth, yr1, col="red", pch=22, cex=0.85)
  points(wingspread~depth, yr2, col="blue", pch=25, cex=0.9)
  points(wingspread~depth, yr3, col="yellow", pch=20)
  points(wingspread~depth, yr4, col="grey", pch=20)
  points(wingspread~depth, yr5, col="light blue", pch=20)
  points(wingspread~depth, yr6, col="brown", pch=20)
  points(wingspread~depth, yr7, col="lavender", pch=20)
  points(wingspread~depth, yr8, col="forest green", pch=20)
  points(wingspread~depth, yr9, col="forest green", pch=".")
  points(wingspread~depth, yr10, col="forest green", pch=".")
  points(wingspread~depth, yr11, col="steel blue", pch=".")
  legend(600,10, c("2014", "2013", "2012", "2011"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("orange","green", "red", "blue"))
  
  plot(opening~depth, r.samp, pch=19, main = "Depth, Opening")
  points(opening~depth, yr, col="green", pch=19)
  points(opening~depth, yr0, col="orange", pch=19)
  legend(600,8, c("2014", "2013"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("orange","green"))
  
  plot(clearance~depth, r.samp, pch=19, main = "Depth, Clearance")
  points(clearance~depth, yr, col="green", pch=19)
  points(clearance~depth, yr0, col="orange", pch=19)
  legend(600,3, c("2014", "2013"), lty=c(1,1), 
         lwd=c(2.5,2.5), col=c("orange","green"))
  
