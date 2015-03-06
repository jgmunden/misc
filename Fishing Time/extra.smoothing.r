require(mgcv)


# remove data where there is no useful information

u = which( is.finite( master$longitude) & is.finite( master$latitude) )
master = master[u,]
# n = 4211584

u = which( is.finite( master$doorspread) | is.finite( master$wingspread) |is.finite( master$depth)   )
master = master[u,]
# n = 4180297

# pick id (mission.set)
id=unique(master$id)
idx=id[sample(1:length(id),1)]
idx

smi=which(master$id == idx)
sm=master[smi, ]
sm$ts=unclass(sm$timestamp)
head(sm)

gs=gsinf[which(gsinf$id == idx), ]
gs

master$doorspread = filter.nets("doorspread.range", master$doorspread)
master$wingspread = filter.nets("wingspread.range", master$wingspread)
master$clearance = filter.nets("clearance.range", master$clearance)
master$opening = filter.nets("opening.range", master$opening)
master$depth = filter.nets("depth.range", master$depth)



# Linear model

z.linear <- lm(wingspread ~ ts,data = sm)
summary(z.linear)
# to check the fit of the model
# Akaike's An Information Criterion, the smaller the AIC the better the fit
AIC(z.linear)
# r squared value
summary(z.linear)$r.sq
# plot raw
plot(wingspread~ts, data=sm)
# plot model
plot(z.linear)




# GAM

z.gam <- gam(wingspread ~ s(ts, bs = "cr"), data = sm)
summary(z.gam)

# to check the fit of the model
AIC(z.gam)
# the GVC score (Generalized cross validation), better to be smaller
summary(z.gam)$sp.criterion
# r.squared value
summary(z.gam)$r.sq
# plot raw
plot(wingspread~ts, data=sm)
# plot model
plot(z.gam)



# Jae's work looking at GAM's and se.fit
  
var = "wingspread"
var = "depth"

vsm = sm[,var]

z.gam <- gam( vsm ~ s(ts, bs = "cr"), data = sm)
summary(z.gam)

# to check the fit of the model
AIC(z.gam)

# Predict data with model
out = predict( z.gam, newdata=sm, newdata.guaranteed=T, se.fit=TRUE  )


str(sm)
plot( vsm ~ sm$ts, pch=".")

lines(out$fit ~ sm$ts, col="green")

summary(out$fit)
summary(sm$wingspread)

sd.proposal = 5

ub = out$fit +  sd.proposal * out$se.fit 
lb = out$fit - (sd.proposal * out$se.fit )

lines( ub ~ sm$ts, col="orange")
lines( lb ~ sm$ts, col="blue")

good = which( vsm > lb & vsm < ub )
points( vsm[good] ~ sm$ts[good] , pch="o", col="purple")









# Filtering raw data using the range of the modelled data

rangew=range(sm$wingspread.smoothed)
rangew
y = min(sm$wingspread.smoothed)
x = max(sm$wingspread.smoothed)
sm$wingspread[which(sm$wingspread > (x))]=NA
sm$wingspread[which(sm$wingspread < (y))]=NA

summary(sm$wingspread)



# Making sure the resulting data falls within a certain standard error
 
sd <- sd(sm$wingspread, na.rm = TRUE)
sd
mean <- mean(sm$wingspread, na.rm = TRUE)
mean
se <- sd/sqrt(length(sm$wingspread))
se

high.end = mean + 2*sd
high.end
low.end = mean - 2*sd
low.end


pred=data.frame(wingspread=0:22)
out=predict(o, newdata=pred, se.fit=T)
out = as.data.frame(out)
out2=predict(r, newdata=pred, se.fit=T)
out2= as.data.frame(out2)
out
out2
out$ub=out$fit+2*out$se.fit
out$lb=out$fit-2*out$se.fit
out



# ANOVA

# Determine if incorporating non-linear effects improves the model
anova(z.linear, z.gam, test = "Chisq")


# Multiple predictors
z.gam2 <- gam(ts ~ wingspread + doorspread, data = sm)
summary(z.gam2)
AIC(z.gam2)
summary(z.gam2)$sp.criterion
summary(z.gam2)$r.sq
plot(z.gam2)

z.gam3 <- gam(ts ~ s(wingspread) + s(doorspread), data = sm)
summary(z.gam3)
summary(z.gam3)$r.sq
plot(z.gam3)

predict(z.gam3)


plot(wingspread~doorspread, sm)

# subsample the master 
mastersub=master[sample (100000), ]

# Global investigation of doorspread and wingspread
plot(wingspread~doorspread, master, pch=1)


depth.master=master[which(master$id == 'TEM2008830.51'),]
summary(depth.master$depth)
range(depth.master$depth)
boxplot(depth.master$depth)
--------------------------------------------------------------------------


# Jae's snowcrab analyses with GAM

localtime = ( header=header, outvalue="localtime" )

# determine deepest point if possible, using a smoothed depth as variability due to incorrect pings are frequent
deepest.point = NULL
if (nc0 %in% c( 13, 14 ) ) {
  require(mgcv)
  z.gam = try ( gam( depth ~ s(tinc, k=5, bs="ts"), data=scanmar, optimizer=c("outer", "nlm") ), silent=T )
  if ( ! "try-error" %in% class( z.gam )) {
    scanmar$depth.smoothed = predict( z.gam, newdata=scanmar, newdata.guaranteed=T  )
    deepest.point = which.max ( scanmar$depth.smoothed)
  }
}

if ( nc0==12 | length(deepest.point) == 0 ) deepest.point = round( nrow(scanmar) / 2 )

scanmar$lon = - (scanmar$lon.deg + (scanmar$lon.min / 60) )
scanmar$lat =    scanmar$lat.deg + (scanmar$lat.min / 60)
scanmar = scanmar[, c("ndate", "ntime", "lat", "lon", "speed", "primary", "secondary", "doorspread", "depth")]
scanmar$ndate = paste(substring(scanmar$ndate,1,2), substring(scanmar$ndate,3,4), substring(scanmar$ndate,5,6), sep="-")
scanmar$ntime = paste(substring(scanmar$ntime,1,2), substring(scanmar$ntime,3,4), substring(scanmar$ntime,5,6), sep=":")

scanmar$chron = chron( dates.=scanmar$ndate, times.=scanmar$ntime, format=c(dates="y-m-d", times="h:m:s"), out.format=outfmt )

# scanmar data stored in GMT from GPS; the offset varies depending upon season due to daylight savings time (3 or 4 hrs)
# obtain time offset in hours
time.offset = scanmarDate( header=header, outvalue="timeoffset" )   #  rounded to hours of fractional days
scanmar$chron = as.chron( as.numeric(scanmar$chron) + time.offset, out.format = outfmt )

scanmar.timestamp = scanmar$chron[deepest.point]
yr = as.numeric( as.character( years( scanmar.timestamp ) ) )


line.localtime = grep("Local Time:", header, ignore.case=T  )
line.ship = grep("Ship:", header, ignore.case=T  )
line.comments = grep("Comments:", header, ignore.case=T )


trip = gsub( "^.*Trip:", "", header[ line.ship ] )
trip = gsub( "Tow:.*$", "", trip )
trip = gsub( "[[:space:]]", "", trip )

if ( ! grepl( "^S[[:digit:]]{8}$", trip, ignore.case=T ) )  { # not a standard code
  dy = paste( "00", as.character( days(scanmar.timestamp) ), sep="")
  dy = substring( dy, nchar(dy)-1, nchar(dy) )
  mn = paste( "00", as.character( as.numeric(months(scanmar.timestamp))), sep="")
  mn = substring( mn, nchar(mn)-1, nchar(mn) )
  yr = paste( "00", as.character( years(scanmar.timestamp) ), sep="")
  yr = substring( yr, nchar(yr)-3, nchar(yr) )
  trip=paste("S", dy, mn, yr, sep="" )
}

scanmar$scanmar_uid = scanmar_uid 








--------------
  
  logistic equation method

str(sm)
ndata = nrow(sm)
sm$fishing = NA
deepestpoint = which.max( sm$depth )
maxdepth = sm$depth[deepestpoint]

depththreshold= 50 

nonfishing = which( sm$depth < (maxdepth - depththreshold) ) # all rows where it is not fishing for sure

sm$fishing[nonfishing] = 0

fishingthreshold = 15
fishing = which( sm$depth > (maxdepth - fishingthreshold) )
sm$fishing[fishing] = 1
  
plot( sm$depth ~ sm$ts, pch="." )
points(sm$depth[fishing] ~ sm$ts[fishing], pch="o", col="red"  )
points(sm$depth[nonfishing] ~ sm$ts[nonfishing], pch="-", col="green"  )

mod = gam( fishing ~ s(depth)  + s(longitude) + s(latitude) , data=sm, family=binomial() )

predfishing = predict( mod, sm, type="response", se.fit=TRUE)

plot(predfishing$fit ~ sm$ts)

plot(sm$clearance ~ sm$ts)


var = "clearance"
missing = which(!is.finite(sm[,var] ) )
notmissing = which(is.finite(sm[,var] ) )
out = approx( sm[notmissing,"ts"], sm[notmissing,var] , xout=sm[missing,"ts"])
sm[missing,var] = out$y
  