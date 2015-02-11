
require(numDeriv)
  
library(lubridate)

# this relies upon the gsinf table which is accessible from the groundfish functions
loadfunctions( "groundfish", functionname="load.groundfish.environment.r") 


# define a few file locations
# file path should be altered once I get my own log on
netswd = file.path("C:", "Users", "MundenJ", "Desktop", "Scanmar")
nmfunctionsdirectory = file.path(netswd, "net_mensuration")
netmensuration.files = list.files( nmfunctionsdirectory, full.names=TRUE )
for (fn in netmensuration.files) source(fn)


# to load the current data version
master = net_mensuration.db( DS="post.perley", netswd=netswd )


# Use one set to test upon 'NED2012002.54' and call the data frame df
df=master[which(master$id == "2009-Jul01-234555.SET.LOG"),]
head(df)
# step to view depth as a trawl profile
d=range(df$depth, na.rm=TRUE) 
plot(depth~timestamp,df, type = "p", col = "orange", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
# frequency histogram
hist(df$depth, "fd") 
x = df
n.req = 30



bottom.contact = function(x, n.req=30) {
  
  # n.req = 30 # number of data points required at a minumum to try this
  
  O = data.frame( cbind(z=NA, zsd=NA, t0=NA, t1=NA, dt=NA, ndata=NA, comments=NA ) ) 
 
  problem =F
  if (nrow(x) < n.req) {
    O$comments="Not enough data"
    return(O)
  }
  
  if (length( which(is.finite(x$depth) ) )< n.req) {
    O$comments="Not enough depth data"
    return(O)  # no data 
  }
  
  mediandepth = quantile( x$depth, probs=c(0.5), na.rm=TRUE)
  drange = 35 # range in depths to permit into analysis (relative to median); depths[3] ==median
  gooddata = which( x$depth < (mediandepth + drange)  & x$depth > (mediandepth - drange)  )
  
  x = x[ gooddata,]
  
  h = hist( x$depth, breaks=100)
  i = which(h$counts > 5)
  lbdepth = h$mids[min(i)]
  ubdepth = h$mids[max(i)]
  
  # remove any simple trend in depth data and so operate upon the residuals 
  central.portion = which( x$depth > lbdepth & x$depth < ubdepth  ) # the 25th and 99 quantiles
  i0 = min( central.portion ) 
  i1 = max( central.portion )
  
  
  if (length( gooddata )< n.req) {
    O$comments="Not enough depth data near the median"
    return(O)  # no data 
  }
  
  # in case time is not in sequence
  x = x[order( x$timestamp ) ,]
  x$ts = unclass( x$timestamp ) 
  x$ts = x$ts - min(x$ts, na.rm=TRUE)
  
  x$zresid = NA
  
  zlm = try(lm(depth~ts, data=x[i0:i1, ]))
  if ("try-error" %in% class( zlm)) {
    O$comments="Linear model will not fit ..assuming linear enough"
	x$zresid = x$depth
  } else {
	x$zresid = x$depth - predict( zlm, newdata=x, type="response"  ) 
  }
  
  
  
  #method 1 .. calculus
  # first smooth the residual to obtain a smooth surface to compute curvature
  kn = kernel( "modified.daniell", c(3, 3) )
  kpsmoothed = kernapply( x$zresid, kn )
  fun = approxfun( x$ts, x$zresid )
  
  res0 = grad( fun, x$ts, method="simple" )
  res0[which(!is.finite(res0))] = 0
  res = kernapply( res0, kn )
  nd = length(res) 
  
  k0 = 1
  for( k0 in 1:nd) {
	if (!is.finite( res[ k0 ] )) next()
	if (res[ k0 ] < 0) break()
  }
  
  k1 = nd
  for( k1 in nd:1) {
	if (!is.finite( res[ k1 ] )) next()
	if (res[ k1 ] > 0) break()
  }
  
  fishing1 = k0:k1
  
   
  
  # method 2 .. .compare variance in fishing area and detect when thing deviate too much from this variance
  # get an initial estimate of standard deviation in the area near the mode
  sd.initial = sd(x[i0:i1,"depth"], na.rm=TRUE)
  median.initial = median(x[i0:i1,"depth"], na.rm=TRUE)
  
  # using initial standard deviation estimate and the median, select a 
  # smaller interval where fishing is occuring with greatest probability
  sd.portion = which( x$zresid > -sd.initial & x$zresid < sd.initial  ) 
  f0 = min( sd.portion ) 
  f1 = max( sd.portion )
  
  if ( (f1-f0) < n.req) {
    O$comments="not enough data for estimating fishing method 1"
    return(O)
  } 
   
  target.sd = max( 0.1, sd (x$zresid[f0:f1], na.rm=T ), na.rm=T ) 
    
  # start from centre and move left and contuniue until sd of residuals begins to deviate sustantially
 
  drangeleft = 0:30
  nx2 = trunc((f1+f0)/2) # midpoint
  # from centre to left 
  for ( j0 in nx2:1 ) if ( sd(( x$zresid[ j0+drangeleft]), na.rm=T) > target.sd )  break()
  
  # end time .. begin from centre to right 
  drangeright = -30:0
  for ( j1 in nx2:nrow(x) ) if ( sd(( x$zresid[j1+drangeright]), na.rm=T) > target.sd ) break()
  
  if ( (j1-j0) < n.req) {
    O$comments="not enough data for estimating fishing method 2"
    return(O)
  } 
    
  fishing2 = j0:j1
  
  
  # final updates
  O$n = nrow(x)
  O$n = nrow(x)
  O$t0 = x$chron[1]
  
  O$t1 = x$chron[O$n]
  O$dt = O$t1 - O$t0
  
  #   O$dt = as.character( O$dt )
  #   O$t0 = as.character( O$t0 )
  #   O$t1 = as.character( O$t1 )
  
  O$z = mean( x$depth, na.rm=T )
  O$zsd = sd( x$depth, na.rm=T )
  
  if (exists( "temperature", x )) {
    O$t = mean( x$temperature, na.rm=T )
    O$tsd = sd( x$temperature, na.rm=T )
  }
  
  return( O )
}