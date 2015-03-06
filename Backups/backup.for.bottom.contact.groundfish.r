bottom.contact.groundfish = function(x, n.req=30) {
  
  # load in libraries
  require(numDeriv)
  require(mgcv)
  
  
  
  # n.req = 30 # number of data points required at a minumum to try this
  
  O = data.frame( cbind( fishing.start.index=NA, fishing.end.index=NA, comments=NA ) ) 
  
  
  
  
  
  # Do some rough filtering to remove sets wihout sifficient data
  problem =F
  if (nrow(x) < n.req) {
    O$comments="Not enough data"
    return(O)
  }
  
  if (length( which(is.finite(x$depth) ) )< n.req) {
    O$comments="Not enough depth data"
    return(O)  # no data 
  }
  
  i = which(x$depth < 25)
  if (length(i) > 0) x = x[-i, ]
  
  # range in depths to permit into analysis (relative to median); depths[3] ==median
  mediandepth = quantile( x$depth, probs=c(0.5), na.rm=TRUE)
  drange = 50 
  gooddata = which( x$depth < (mediandepth + drange)  & x$depth > (mediandepth - drange)  )
  
  x = x[ gooddata,]
  
  # breaks defines how many columns you want in your histogram
  h = hist( x$depth, breaks=100, plot=FALSE)
  i = which(h$counts > 10)
  # lb =lower bound, ub = upper bound
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
  
  
  
  
  
  # Calculate gam
  
  zlm = try(gam(depth~s(ts, bs="ts"), data=x[c(i0:i1),], weights=depth^2 ))
  if ("try-error" %in% class( zlm)) {
    O$comments="Linear model will not fit ..assuming linear enough"
    x$zresid = x$depth
  } else {
    u = as.vector( predict( zlm, newdata=x, type="response"  ) )
    x$zresid = x$depth -u
    # points( u~ts, x)
  }
  
  
  
  
  
  # Method 1 .. .compare variance in fishing area and detect when thing deviate too much from this variance
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
  
  nx2 = trunc((f1+f0)/2) # midpoint
  # from centre to left 
  for ( j0 in nx2:1 ) if ( sd(( x$zresid[ (nx2+10):j0]), na.rm=T) > target.sd )  break()
  
  # end time .. begin from centre to right 
  for ( j1 in nx2:nrow(x) ) if ( sd(( x$zresid[ (nx2-10):j1]), na.rm=T) > target.sd ) break()
  
  if ( (j1-j0) < n.req) {
    O$comments="not enough data for estimating fishing method 1"
    return(O)
  } 
  
  O$fishing.start.index2 = x$timestamp[j0]
  O$fishing.end.index2 = x$timestamp[j1]
  
  x = x[j0:j1, ]
  
  
  
  
  
  
  
  
  # Method 2 .. calculus
  # first smooth the residual to obtain a smooth surface to compute curvature
  
  #    recenter the smooths 
  kpsmoothed = kernapply( x$zresid, kernel( "modified.daniell", c(3, 3) ) )
  diff = nrow(x) - length(kpsmoothed)
  delta = trunc(diff/2)
  offset = (1+delta):(nrow(x)-delta)
  
  fun = approxfun( x$ts[offset], kpsmoothed )
  
  #    recenter the smooths 
  res0 = grad( fun, x$ts, method="simple" )
  res0[which(!is.finite(res0))] = 0
  res = kernapply( res0, kernel( "modified.daniell", c(3, 3) ) )
  nd = length(res) 
  
  diff = nrow(x) - nd
  delta = trunc(diff/2)
  offset = (1+delta):(nrow(x)-delta)
  
  
  # plot(x$zresid ~ x$ts)
  # plot(res ~ x$ts[ offset])
  
  
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
  
  O$fishing.start.index = x$timestamp[k0]
  O$fishing.end.index = x$timestamp[k1]
  
  return( O )
}







