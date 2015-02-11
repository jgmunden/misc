bottom.contact.groundfish3 = function(x, n.req=30, sd.fraction=1, drange = 50, densitythreshold = 0.05,
                                     gam.smooth.var="depth", gam.data.var="x", kernel=c(3,3) ) {
  
  # n.req=30;sd.fraction=1;gam.data.var="x";gam.smooth.var="depth";kernel=c(3,3); drange = 50; densitythreshold = 0.05
  
  # load in libraries
  require(numDeriv)
  require(mgcv)
  require(INLA)
  
  O = list(  comments=NA )  # output list
  
  # in case time is not in sequence
  x = x[order( x$timestamp ) ,]
  
  # timestamps have frequencies higher than 1 sec .. duplciates are created and this can pose a problem
  x$ts = as.numeric(x$timestamp)  # in seconds 
  x$ts = x$ts - min(x$ts) 
  x$rowindex = 1:nrow(x)
  
  ## STEP 1 -- simple range limits (gating) of depths to remove real extremes
  
  # eliminate records with NA for depth  
  i = which(!is.finite(x$depth))
  if (length(i) > 0) x$rowindex[i] = NA
  
  # eliminiate shallow records due to operation at sea level
  i = which(x$depth < 25)
  if (length(i) > 0) x$rowindex[i] = NA
  
  # eliminate a range around the median value 
  mediandepth = quantile( x$depth, probs=c(0.5), na.rm=TRUE)
  
  baddata = which( x$depth > (mediandepth + drange) | x$depth < (mediandepth - drange)  )
  if (length(baddata) > 0) x$rowindex[baddata] = NA
  lb = which.min( x$rowindex )
  ub = which.max( x$rowindex )
  x = x[lb:ub,]
  x$depth[ which(!is.finite(x$rowindex))] = NA
  
  O$gated.data = x
  
  
  ## STEP 2 - filter by looking for modal distribution and estimating sd of the modal group 
  # by removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  
  # expectation from uniform random is 1/2 of the threshold for testing that the freqeuncy belongs to the mode
  nbreaks =  trunc( nrow(x) /( densitythreshold/2 * 100) ) 
  h = hist( x$depth[which(is.finite(x$rowindex))], breaks=nbreaks, plot=FALSE)
  i = which(h$density > densitythreshold)
  central.portion = which( x$depth > h$mids[min(i)] & x$depth < h$mids[max(i)]  ) 
  # indices of x where the data distribution suggests we have bottom contact
  i0 = min( central.portion ) 
  i1 = max( central.portion )
  
  #  points(depth~timestamp, x[i0:i1,], col="black", pch=20)
  
  if (length( i0:i1 ) < n.req) {
    O$comments="Not enough depth data near the median"
    return(O)  # no data 
  }
  
  # Do some rough filtering to remove sets wihout sufficient data
  problem =F
  if (nrow(x) < n.req) {
    O$comments="Not enough data"
    return(O)
  }
  
  if (length( which(is.finite(x$depth) ) )< n.req) {
    O$comments="Not enough depth data"
    return(O)  # no data 
  }
  
  ## STEP 3 --<variance> compute SD in the area of interest and compare with a lagged process to 
  ## determine first estimate of fishing range based upon SD in depth data
  ## i0 and i1 are determined from histogram-based mode identification method above
  
  # start from centre and move left and contuniue until sd of residuals begins to deviate sustantially
  target.sd = sd.fraction * max( 0.1, sd (x$depth[i0:i1], na.rm=T ), na.rm=T ) 
  
  # start from centre and move left and contuniue until sd of residuals begins to deviate sustantially
  
  nx2 = trunc((i1+i0)/2) # midpoint
  # from centre to left 
  for ( j0 in nx2:1 ) if ( sd(( x$depth[ (nx2+10):j0]), na.rm=T) > target.sd )  break()
  
  # end time .. begin from centre to right 
  for ( j1 in nx2:nrow(x) ) if ( sd(( x$depth[ (nx2-10):j1]), na.rm=T) > target.sd ) break()
  
  if ( (j1-j0) < n.req) {
    O$comments="not enough data for estimating fishing method 1"
    return(O)
  } 
  
  O$variance.method = c( x$timestamp[j0], x$timestamp[j1] )
  O$variance.method.indices = which( O$gated.data$timestamp >= O$variance.method[1] &  O$gated.data$timestamp <= O$variance.method[2] )
  
  
  if ( (j1-j0) < n.req) {
    O$comments="not enough data for estimating fishing method 1"
    return(O)
  } 
}
  
  
  
  # STEP 4 -- smooth data to remove local trends and compute a detrended residual <gam>
  
  #  .. .compare SD in fishing area and detect when things deviate too much from this SD
  # first, remove local trends in data via a GAM and operated upon residuals
  # and then get an initial estimate of standard deviation in the area near the mode
  
  # Calculate gam to remove linear/ general trends
  
  if (gam.data.var=="x") {
    zlm = try(gam(depth ~ts + s( ts, bs="cr"), data=x, weights=depth^2, na.action = "na.omit"))
  } else {
    zlm = try(gam(depth ~ts + s( ts, bs="cr"), data=x[c(i0:i1),], weights=depth^2, na.action = "na.omit"))
  }
  x$zresid = NA
  
  if ("try-error" %in% class( zlm)) {
    O$comments="Linear model will not fit ..assuming linear enough"
    x$zresid = x$depth
  } else {
    u = as.vector( predict( zlm, newdata=x, type="response"  ) )
    x$zresid = x$depth - u
    # plot( u~ts, x)
  }
  
  # determine approximating function based upon data to facilitate curvature estimation
  if (gam.smooth.var=="depth") {
    smv = x$depth
  } else {
    smv = x$zresid
  }
  # first smooth it to remove discontinuities (2nd smooth kernal)
  kpsmoothed = kernapply( smv, kernel( "modified.daniell", kernel ) )
  diff = nrow(x) - length(kpsmoothed)
  delta = trunc(diff/2)
  offset = (1+delta):(nrow(x)-delta)
  fun = approxfun( x$ts, smv )
  
  # first, recenter the smooths 
  slopes = grad( fun, x$ts, method="simple" )
  slopes[which(!is.finite(slopes))] = 0  
  
  ## use method similar to STEP 2 - but filter by looking for modal distribution and estimating sd of the modal group of slopes
  # removing small frequencies (5 or less) being ignored 
  # breaks defines how many columns you want in your histogram
  h = hist( slopes, breaks=100, plot=FALSE)
  i = which(h$counts > 5)
  central.portion = which( slopes > h$mids[min(i)] & slopes < h$mids[max(i)]  ) 
  # indices of x where the data distribution suggests we have bottom contact
  s0 = min( central.portion ) 
  s1 = max( central.portion )
  
  slopes.smoothed = kernapply( slopes, kernel( "modified.daniell", kernel ) )
  
  nd = length(slopes.smoothed) 
  
  diff = nrow(x) - nd
  delta = trunc(diff/2)
  offset = (1+delta):(nrow(x)-delta)
  
  # plot(x$depth ~ x$ts)
  # plot(slopes.smoothed ~ x$ts[ offset])
  
  
  
  
  ## STEP 5 - using (smoothed) first derivative determine inflection points (crossing of the zero line)
  nd = length(slopes.smoothed) 
  
  k0 = 1
  for( k0 in 1:nd) {
    if (!is.finite( slopes.smoothed[ k0 ] )) next()
    if (slopes.smoothed[ k0 ] < 0) break()
  }
  
  k1 = nd
  for( k1 in nd:1) {
    if (!is.finite( slopes.smoothed[ k1 ] )) next()
    if (slopes.smoothed[ k1 ] > 0) break()
  }
  O$gam.method =  c( x$timestamp[k0], x$timestamp[k1] )
  O$gam.method.indices = which( O$gated.data$timestamp >= O$gam.method[1] &  O$gated.data$timestamp <= O$gam.method[2] ) 
  
  return(O)
  
}

