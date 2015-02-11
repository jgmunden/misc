
# missing from marport: "NED2014101.1"  "NED2014101.2"  "NED2014101.3"  "NED2014101.4", "NED2014101.20" "NED2014101.21" "NED2014101.22" "NED2014101.23" "NED2014101.24"
# missing from scanmar:  "NED2014101.44" 

ids = c('NED2014101.5', 'NED2014101.6', 'NED2014101.7', 'NED2014101.8','NED2014101.9', 'NED2014101.10', 'NED2014101.11', 'NED2014101.12', 
        'NED2014101.13','NED2014101.14', 'NED2014101.15', 'NED2014101.16', 'NED2014101.17', 'NED2014101.18', 'NED2014101.19', 'NED2014101.20', 
        'NED2014101.21', 'NED2014101.22','NED2014101.23', 'NED2014101.24', 'NED2014101.25', 'NED2014101.26', 'NED2014101.27', 'NED2014101.28', 
        'NED2014101.29', 'NED2014101.30', 'NED2014101.31', 'NED2014101.32', 'NED2014101.33', 'NED2014101.34', 'NED2014101.35', 'NED2014101.36',  
        'NED2014101.37', 'NED2014101.38', 'NED2014101.39', 'NED2014101.40','NED2014101.41', 'NED2014101.42', 'NED2014101.43', 'NED2014101.45', 
        'NED2014101.46', 'NED2014101.47', 'NED2014101.48', 'NED2014101.49', 'NED2014101.50','NED2014101.51', 'NED2014101.52', 'NED2014101.53', 
        'NED2014101.54')
 
       # 'NED2014101.7','NED2014101.8', 'NED2014101.9', 'NED2014101.10', 'NED2014101.11', 'NED2014101.12', 'NED2014101.13','NED2014101.14', 'NED2014101.15', 'NED2014101.16', 'NED2014101.17', 'NED2014101.18', 'NED2014101.19', 'NED2014101.20'. 'NED2014101.21', 'NED2014101.22','NED2014101.23', 'NED2014101.24', 'NED2014101.25', 'NED2014101.26', 'NED2014101.27', 'NED2014101.28', 'NED2014101.29', 'NED2014101.30', 'NED2014101.31','NED2014101.32', 'NED2014101.33', 'NED2014101.34', 'NED2014101.35', 'NED2014101.36', 'NED2014101.37', 'NED2014101.38', 'NED2014101.39', 'NED2014101.40','NED2014101.41', 'NED2014101.42', 'NED2014101.43', 'NED2014101.45', 'NED2014101.46', 'NED2014101.47', 'NED2014101.48', 'NED2014101.49', 'NED2014101.50','NED2014101.51', 'NED2014101.52', 'NED2014101.53', 'NED2014101.54')

r = data.frame( cbind( id=ids, 
n = NA, d.perc = NA, w.perc = NA, no.door = NA, no.wing = NA, mean.door = NA, 
                 sd.door= NA, mean.wing = NA, sd.wing = NA, no.depth = NA, mean.depth = NA, sd.depth = NA, dep.perc = NA,
                 min.time = as.POSIXct(NA), max.time = as.POSIXct(NA), total.time = as.POSIXct(NA), min.lat = NA, max.lat = NA, mean.lat = NA,  min.lon = NA, 
                 max.lon = NA, mean.lon = NA, no.head = NA, h.perc = NA, mean.head = NA, sd.head = NA, d.source = "sm"), stringsAsFactors = FALSE )  


for (i in 1:nrow(r)){
  test = which(master$id==r[i,"id"])
  scanmar1 = master[test, ]
  
# Number of measurements
    r$n[i]=length(test)

    # Number of doorspread/wingspread/headline/depth measurements
    r$no.door[i] = length(which( is.finite(scanmar1$doorspread)))
    r$d.perc[i] = (length(which( is.finite(scanmar1$doorspread)))/length(test))*100
    r$no.wing[i] = length(which( is.finite(scanmar1$wingspread)))
    r$w.perc[i] = (length(which( is.finite(scanmar1$wingspread)))/length(test))*100

    r$no.head[i] = length(which( is.finite(scanmar1$opening)))
    r$h.perc[i] = (length(which( is.finite(scanmar1$opening)))/length(test))*100

    r$no.depth[i] = length(which( is.finite(scanmar1$depth)))
    r$dep.perc[i] = (length(which( is.finite(scanmar1$depth)))/length(test))*100 
   
    # Calculate mean and standard deviation for doorspread
    r$mean.door[i] = mean(scanmar1$doorspread, na.rm=TRUE)
    r$sd.door[i] = sd(scanmar1$doorspread, na.rm=TRUE) 
    
    # Calculate mean and standard deviation for wingspread
    r$mean.wing[i] = mean(scanmar1$wingspread, na.rm=TRUE)
    r$sd.wing[i] = sd(scanmar1$wingspread, na.rm=TRUE) 
  

    # Calculate mean and standard deviation for opening
    r$mean.head[i] = mean(scanmar1$opening, na.rm=TRUE)
    r$sd.head[i] = sd(scanmar1$opening, na.rm=TRUE) 

    # calculate mean depth
    r$mean.depth[i] = mean(scanmar1$depth, na.rm=TRUE)
    r$sd.depth[i] = sd(scanmar1$depth, na.rm=TRUE)
    
    # time   
    r$min.time[i]= as.character( min(scanmar1$timestamp, na.rm=TRUE) )
    r$max.time[i]=as.character( max(scanmar1$timestamp, na.rm=TRUE) )
    r$total.time [i]= (max(scanmar1$timestamp)) - (min(scanmar1$timestamp))

    # spatial data
    r$min.lat[i] = min(scanmar1$latitude) 
    r$max.lat[i] = max(scanmar1$latitude)
    r$mean.lat[i] = mean(scanmar1$latitude)
    r$min.lon[i] = min(scanmar1$longitude)
    r$max.lon[i] = min(scanmar1$longitude)
    r$mean.lon[i] = min(scanmar1$longitude)

  }
write.table(r, file= "scanmarhh.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

# MARPORT CALCULATIONS
k = data.frame( cbind( mission=ids,
                n = NA, d.perc = NA, w.perc = NA, no.door = NA, no.wing = NA, mean.door = NA, 
                 sd.door= NA, mean.wing = NA, sd.wing = NA, no.depth = NA, mean.depth = NA, sd.depth = NA, dep.perc = NA,
                 min.time = NA, max.time = NA, total.time = NA, min.lat = NA, max.lat = NA, mean.lat = NA,  min.lon = NA, 
                 max.lon = NA, mean.lon = NA, no.head.s = NA, h.perc.s = NA, mean.head.s = NA, sd.head.s = NA,
                d.source = "mp"), stringsAsFactors = FALSE)
 
    for (i in 1:nrow(k)){
      test = which(marport$mission==k[i,"mission"])
      marport1 = marport[test, ]

    k$n[i]=length(test)
    k$no.door[i] = length(which( is.finite(marport1$doorspread)))
    k$d.perc[i] = (length(which( is.finite(marport1$doorspread)))/length(test))*100
    k$no.wing[i] = length(which( is.finite(marport1$wingspread)))
    k$w.perc[i] = (length(which( is.finite(marport1$wingspread)))/length(test))*100
  
    k$no.head.s[i] = length(which( is.finite(marport1$opening.scanmar)))
    k$h.perc.s[i] = (length(which( is.finite(marport1$opening.scanmar)))/length(test))*100
    
    # Calculate mean doorspread and standard deviation
    k$mean.door[i] = mean(marport1$doorspread, na.rm=TRUE)
    k$sd.door[i] = sd(marport1$doorspread, na.rm=TRUE)
    
    # Calcuate mean wingspread and standard deviation
    k$mean.wing[i] = mean(marport1$wingspread, na.rm=TRUE)
    k$sd.wing[i] = sd(marport1$wingspread, na.rm=TRUE) 
    
    # Calcuate mean headline.scanmar and standard deviation
    k$mean.head.s[i] = mean(marport1$opening.scanmar, na.rm=TRUE)
    k$sd.head.s[i] = sd(marport1$opening.scanmar, na.rm=TRUE) 
    
    # Depth
    k$mean.depth[i] = mean(marport1$DepthDoorPort, na.rm=TRUE)
    k$sd.depth[i] = sd(marport1$DepthDoorPort, na.rm=TRUE)
    k$dep.perc[i] = (length(which( is.finite(marport1$DepthDoorPort)))/length(test))*100
    
    # time
    k$min.time[i]=as.character(min(marport1$timestamp))
    k$max.time[i]=as.character(max(marport1$timestamp))
    k$total.time[i]=(max(marport1$timestamp)) - (min(marport1$timestamp))
    
    # spatial data
    k$min.lat[i] = min(marport1$latitude) 
    k$max.lat[i] = max(marport1$latitude)
    k$mean.lat[i] = mean(marport1$latitude)
    k$min.lon[i] = min(marport1$longitude)
    k$max.lon[i] = min(marport1$longitude)
    k$mean.lon[i] = min(marport1$longitude)
    
}


write.table(k, file= "marporttime.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE) 

modern.data=master[which(master$year %in% 2004:2014) , ]
Data QC
check=marport[which(marport$mission == "NED2014101.10") , ]
min(check$timestamp)
max(check$timestamp)
write.table(check, file= "NED2014101.10.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
