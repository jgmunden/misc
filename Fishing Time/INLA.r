


if ("inla" %in% method) {
  if ("variance"%in% method) x = O$gated.data[O$variance.method.indices,]
  oo = which( duplicated( x$ts ))
  if (length(oo)>0 ) x$ts[oo] = x$ts[oo] + 0.5
  require(INLA)
  mod = inla( depth ~ f(ts, model="ar1"), data=x )  # essentially a GAM but implemented in INLA with priors  ?? 
  x$zresid = mod$summary.random[[1]][["mean"]]
  # determine approximating function based upon data to facilitate curvature estimation .. no need to smooth as it is already smoothed
  fun = approxfun( x$ts, x$zresid )  
  # first, recenter the smooths 
  slopes = grad( fun, x$ts, method="simple" )
  mod.slope = inla( slopes ~ f(ts, model="ar1"), data=x ) 
  # slopes.smoothed = mod.slope$summary.random[[1]][["mean"]]
  slopes.smoothed = slopes *NA
  i = which(is.finite(slopes.smoothed))
  slopes.smoothed[i] = kernapply( slopes[i], kernel( "modified.daniell", kernel ) )
  
  ## STEP 5 - using (smoothed) first derivative determine inflection points (crossing of the zero line) <inla
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
  O$inla.method =  c( x$timestamp[k0], x$timestamp[k1] )
  O$inla.method.indices = which( O$gated.data$timestamp >= O$inla.method[1] &  O$gated.data$timestamp <= O$inla.method[2] )
  
}

return( O )
}





