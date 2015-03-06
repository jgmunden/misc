
# If you want to filter before
master$doorspread = filter.nets("doorspread.range", master$doorspread)
master$wingspread = filter.nets("wingspread.range", master$wingspread)
master$clearance = filter.nets("clearance.range", master$clearance)
master$opening = filter.nets("opening.range", master$opening)
master$depth = filter.nets("depth.range", master$depth)

#Depth
allids=unique(modern.data$id)
i=sample(1:length(allids),10)
allids=allids[i]

for (id in allids){
  
  test=which(modern.data$id==id)
  i=modern.data[test,]
  d=range(i$depth, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(depth~timestamp,i, type = "p", lwd = 1, main =  "Depth", sub=id, ylim=c(d[2], d[1]))
  }
}

#wingspread/timestamp
allids=unique(master$id)
i=sample(1:length(allids),50)
allids=allids[i]

for (id in allids){
 
  test=which(master$id==id)
  i=master[test,]
  d=range(i$wingspread, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(wingspread~timestamp,i, type = "p", col = "orange", lwd = 1, main =  "Wingspread", sub=id)
  }
}
  
#doorpsread/timestamp
allids=unique(master$id)
i=sample(1:length(allids),25)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$doorspread, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(doorspread~timestamp,i, type = "p", col = "orange", lwd = 1, main =  "doorspread", sub=id)
  }
}

#opening/timestamp
allids=unique(master$id)
i=sample(1:length(allids),25)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$opening, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(opening~timestamp,i, type = "p", col = "orange", lwd = 1, main =  "opening", sub=id)
  }
}

#clearance/depth/wingspread/opening/doorspread/timestamp
allids=unique(master$id)
i=sample(1:length(allids),50)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$clearance, na.rm=TRUE)
  e=range(i$depth, na.rm=TRUE)
  f=range(i$wingspread, na.rm=TRUE)
  g=range(i$opening, na.rm=TRUE)
  h=range(i$doorspread, na.rm=TRUE)
  if(any(is.finite(d & e & f & g & h))) {
    plot(clearance~timestamp,i, type = "p", col = "orange", lwd = 1, main =  "set_variables", sub=id, ylim=c(0,200))
    lines(depth~timestamp,i, type = "p", col = "blue", lwd = 1, ylim=c(0,200))
    lines(wingspread~timestamp,i, type = "p", col = "purple", lwd = 1, ylim=c(0,200))
    lines(opening~timestamp,i, type = "p", col = "red", lwd = 1, ylim=c(0,200))
    lines(doorspread~timestamp,i, type = "p", col = "green", lwd = 1, ylim=c(0,200))
  }
}

# wingspread/doorspread/timestamp
allids=unique(master$id)
i=sample(1:length(allids),50)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  f=range(i$wingspread, na.rm=TRUE)
  h=range(i$doorspread, na.rm=TRUE)
  if(any(is.finite( f & h))) {
    plot(wingspread~timestamp,i, type = "p", col = "purple", lwd = 1, main =  "set_variables", sub=id, ylim=c(0,90))
    lines(doorspread~timestamp,i, type = "p", col = "green", lwd = 1, ylim=c(0,90))
  }
}


#doorspread/wingspread
allids=unique(master$id)
i=sample(1:length(allids),50)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$doorspread, na.rm=TRUE)
  if(any(is.finite(i$doorspread))) {
  if(any(is.finite(i$wingspread))){}
    plot(doorspread~wingspread,i, type = "p", col = "orange", lwd = 1, main =  "Door/Wing", xlim=c(10, 25), ylim=c(50,200), sub=id
         )
  }
} 

# depth/doorspread
# odd double track, does this signify double bounce of acoustic signal? or perhaps something else?
allids=unique(master$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$doorspread, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(doorspread~depth,i, type = "p", col = "orange", lwd = 1, main =  "Doorspread", sub=id, xlim=c(0, 600), ylim=c(10,100))
  }
}

#wingspread/headline height
#You can definitely tell where the mean is, cluster of points, but relationship
allids=unique(master$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$wingspread, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(opening~wingspread,i, type = "p", col = "orange", lwd = 1, main =  "Wing/Head", sub=id, xlim=c(0, 30), ylim=c(0,15))
  }
}

#Clearance/depth

allids=unique(master$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(master$id==id)
  i=master[test,]
  d=range(i$clearance, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(clearance~depth,i, type = "p", col = "orange", lwd = 1, main =  "depth/clearance", sub=id, xlim=c(0,600), ylim=c(0, 50))
  }
}

# Only for historic data
allids=unique(historic.data$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(historic.data$id==id)
  i=historic.data[test,]
  d=range(i$clearance, na.rm=TRUE)
  e=range(i$opening, na.rm=TRUE)
  if(any(is.finite(d & e))) {
    plot(clearance~timestamp,i, type = "p", col = "orange", lwd = 1, main =  "clearance", sub=id, ylim=c(0,10))
    lines(opening~timestamp,i, type = "p", col = "blue", lwd = 1, ylim=c(0,10))
  }
}

i=which(is.finite(master$depth))
ms=master[i,]
ms=ms[ sample (1:nrow(ms), 200000), ] # 1:nrow(df) gives a random sample rather
plot(wingspread~doorspread,ms, pch=".")
plot(wingspread~doorspread,ms, pch=".", type="n")
text( ms$doorspread, ms$wingspread, labels=round(ms$depth), cex=0.6)
points(ms$doorspread, ms$wingspread, pch=".", col="green")


  
per =   net_mensuration.db( DS="perley.database", netswd=netswd )
points(per$doorspread, per$wingspread, pch=".", col="orange")

  

nodepth=master[-i,]


nodepth[sample(1:nrow(master),200000), ]
#per= 
points(per$doorspread, per$wingspread, pch=".", col="orange")

i = grep("NED2014102", master$id)
str(i)
points(master$doorspread[i], master$wingspread[i], pch=".", col="purple")

r = grep("NED2013028", master$id)
str(r)
points(master$doorspread[r], master$wingspread[r], pch=".", col="blue")
  
  
----------------------------------------------------------------------------------  
#Clearance/opening
#cluster around the mean
allids=unique(sm$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(sm$id==id)
  i=sm[test,]
  d=range(i$clearance, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(clearance~opening,i, type = "p", col = "orange", lwd = 1, main =  "clearance/opening", sub=id, xlim=c(0,50), ylim=c(0, 75))
  }
}

#ltspeed, ctspeed
#cluster around the mean
allids=unique(sm$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(sm$id==id)
  i=sm[test,]
  d=range(i$ltspeed, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(ltspeed~ctspeed,i, type = "p", col = "orange", lwd = 1, main =  "speed", sub=id, xlim=c(0,5), ylim=c(0, 8))
  }
}

#ltspeed/fspd
allids=unique(sm$id)
i=sample(1:length(allids),100)
allids=allids[i]

for (id in allids){
  
  test=which(sm$id==id)
  i=sm[test,]
  d=range(i$ltspeed, na.rm=TRUE)
  if(any(is.finite(d))) {
    plot(ltspeed~fspd,i, type = "p", col = "orange", lwd = 1, main =  "speed", sub=id, xlim=c(0,5), ylim=c(0, 8))
  }
}


#global test for doorspread/wingspread
plot(doorspread~wingspread,master, xlim=c(10, 25), ylim=c(50,200), pch=".")

sm$doorspread[which(sm$doorspread>90)]=NA
sm$wingspread[which(sm$wingspread>20)]=NA
sm$doorspread[which(sm$doorspread<0)]=NA
sm$wingspread[which(sm$wingspread<0)]=NA

plot(doorspread~wingspread, sm, pch=".")
o=lm(doorspread~wingspread, sm)
r=rlm(doorspread~wingspread, sm)

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

#global test for doorspread/depth
plot(doorspread~depth, sm)
plot(doorspread~depth,sm, xlim=c(0, 600), ylim=c(0,100), pch=".")


#Computing linear models and robust linear models
plot(doorspread~depth, sm, pch=".")
o=lm(doorspread~depth, sm)
r=rlm(doorspread~depth, sm)

pred=data.frame(depth=0:600)
out=predict(o, newdata=pred, se.fit=T)
out = as.data.frame(out)
out2=predict(r, newdata=pred, se.fit=T)
out2= as.data.frame(out2)
out
out2
out$ub=out$fit+2*out$se.fit
out$lb=out$fit-2*out$se.fit
out