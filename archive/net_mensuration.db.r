net_mensuration.db=function( DS, nm=NULL, netswd=getwd() ){
   
  if(DS %in% c("perley.database", "perley.database.merge", "perley.database.datadump" )) {
    # fn1= old data, fn2= new data and fn3= merged data (old and new)
    fn1= file.path(netswd,"scanmar.perley.rdata")
    fn2= file.path(netswd,"scanmarnew.perley.rdata")
    fn3= file.path(netswd,"scanmarmerged.rdata")
  
    if(DS=="perley.database.datadump"){
      # Package RODBC is a platform for interfacing R to database systems
      # Package includes the obdc* commands (access) and sql* functions (read, save, copy, and manipulate data between data frames)
      require(RODBC)
      connect=odbcConnect( "quoddy", uid="cooka", pwd="f43xy21b", believeNRows=F)
      # sqlquery can be used to return part of a table
      scanmar = sqlQuery(connect, "select * from   groundfish.perleyp_SCANMAR", as.is=T) 
      scanmarnew = sqlQuery(connect, "select * from   groundfish.perleyp_NEWSCANMAR", as.is=T) 
      # closing the connection with ODBC
      odbcClose(connect)
      # saving the tables to R memory in zip format 
      save(scanmar, file=fn1, compress=T)
      save(scanmarnew, file=fn2, compress=T)
    }
    
    # Need an explanation of this step
    if(DS=="perley.database"){
      nm = NULL
      if (file.exists(fn3)) load(fn3)
      return(nm)
    }
  
    # Changing scanmarnew names to lowercase 
    if(DS=="perley.database.merge"){
      load(fn1) 
      load(fn2)
      names(scanmarnew)=tolower(names(scanmarnew))      
      
      # nm is the dataset which combines the old and new data (merged)
      # some variables were missing from scanmar to faciliate the merge of scanmarnew
      nm=scanmar
      nm$fspd=NA
      nm$cspd=NA        
      nm$latitude=NA
      nm$longitude=NA
      nm$depth=NA
      nm$empty=NA
      
      # using logtime to create the time variable
      scanmarnew$logtime=scanmarnew$time
      scanmarnew$time=NULL
      
      # creating a matrix (nm2) with nm and scanmarnew
      nm2=matrix(NA,ncol=ncol(nm),nrow=nrow(scanmarnew))
      nm2= as.data.frame(nm2)
      names(nm2)=names(nm)
      
      # making the columns names of nm2 equal to those of scanmarnew
      o =names(scanmarnew)
      for(n in o){
        nm2[,n]=scanmarnew[,n]
      }
      # Combining rows of nm and nm2 to create the data frame nm
      nm=rbind(nm,nm2)      
      
      #This step is creating variables by pasting existing together
      #It is also changing character values to numeric
      nm$uniqueid=paste(nm$mission,nm$setno,sep="_")
      nm$ltspeed=as.numeric(nm$ltspeed)
      nm$ctspeed=as.numeric(nm$ctspeed)
      nm$doorspread=as.numeric(nm$doorspread)
      nm$wingspread=as.numeric(nm$wingspread)
      nm$clearance=as.numeric(nm$clearance)
      nm$opening=as.numeric(nm$opening)
      nm$depth=as.numeric(nm$depth)
      nm$latitude=as.numeric(nm$latitude)
      nm$longitude=as.numeric(nm$longitude)
      nm$year= as.numeric(substring(nm$mission,4,7))
      nm$fspd=as.numeric(nm$fspd)
      nm$cspd= as.numeric(nm$cspd)
       
      save(nm,file=fn3,compress=TRUE)
    }
  }
  
    # Correcting the date/time problem
    if (DS %in% c("nm.timestamp", "nm.timestamp.redo" )) {
      
      fn = file.path(netswd,"scanmar.nm.timestamp.rdata")
       
      if(DS=="nm.timestamp"){
        nm = NULL
        if (file.exists(fn)) load(fn)
        return(nm)
      }
      
      if(DS=="nm.timestamp.redo"){
         
        nm$id=paste(nm$mission, nm$setno, sep=".")
        
        # Correcting for data which contains NA in the time slot by identifying and deleting it
        strangedata = which(is.na(nm$logtime))
        if(length(strangedata)>0) nm=nm[-strangedata,]
        
        # load groundfish inf table which has timestamps of start/stop times and locations
        gsinf = groundfish.db( DS="gsinf" )
        gsinfvars=c("id", "sdate", "time", "dist", "settype")
        
        # merge 
        nm = merge( nm, gsinf[,gsinfvars], by="id", suffixes=c(".nm", ""), all.x=TRUE, all.y=FALSE)
        
        # fix some time values that have lost the zeros due to numeric conversion
        nm$logtime=gsub(":", "", nm$logtime)      
        j=nchar(nm$logtime)
        tooshort=which(j==5)
        nm$logtime[tooshort]=paste("0",nm$logtime[tooshort],sep="")
        
        tooshort=which(j==4)
        nm$logtime[tooshort]=paste("00",nm$logtime[tooshort],sep="")
        
        tooshort=which(j==3)
        nm$logtime[tooshort]=paste("000",nm$logtime[tooshort],sep="")
        
        tooshort=which(j==2)
        nm$logtime[tooshort]=paste("0000",nm$logtime[tooshort],sep="")
        
        tooshort=which(j==1)
        nm$logtime[tooshort]=paste("00000",nm$logtime[tooshort],sep="")
        
        nm$hours=substring(nm$logtime,1,2)
        
        nm$min=substring(nm$logtime,3,4)
        
        nm$sec=substring(nm$logtime,5,6)
   
        nm$day = day( nm$sdate )
        nm$mon = month( nm$sdate )
      
        i=which(!is.finite(nm$day))
        nm = nm[ -i, ]
        
        nm$timestamp= paste(nm$year,nm$mon, nm$day, nm$hours, nm$min, nm$sec, sep="-" )

      
        #lubridate function 
        nm$timestamp = ymd_hms(nm$timestamp) 
        
        nm$uniqueid=NULL
        nm$mission=NULL
        nm$cruno=NULL
        nm$setno=NULL
        nm$day=NULL
        nm$mon=NULL
        nm$year=NULL
        nm$hours=NULL
        nm$min=NULL
        nm$sec=NULL
        nm$sdate=NULL
        nm$time=NULL
        
        # fix sets that cross midnight and list
        uniqueid = unique(nm$id)
        #uniqueid =c("NED1992165_61","NED2009027_214", "NED1992165_54", "NED2009027_197", "NED1992165_50", "NED2009027_121", "NED2009027_170",   )
        
        print ("The following have sets that cross midnight and require days to be adjusted" )
        
        hr = 60*60
        tocorrect=NULL
        for (id in uniqueid){
          i = which(nm$id==id)
          r=range(nm$timestamp[i], na.rm=TRUE)
          y=as.duration(new_interval(r[1],r[2]))
          if(y>hr) {
            print(id) #if you don't want the trips listed, remove this line
            hrs=hour(nm$timestamp[i])
            z=which(hrs<2)
            if(length(z)>0){
              tocorrect=c(tocorrect, i[z])
            }
          }  
        }
        
        day(nm$timestamp[tocorrect]) = day(nm$timestamp[tocorrect])+1
        
        save(nm, file=fn, compress=TRUE)
  }
  
 }

# Step to filter data  
 if(DS %in% c("range.checks", "range.checks.redo") ) {
   
   fn = file.path( netswd, "nm.sanity.checked")
   if(DS=="range.checks") {
     nm = NULL
     if (file.exists(fn)) load(fn)
     return(nm)
   }
    
   nm = net_mensuration.db( DS="nm.timestamp", netswd=netswd ) 
    
     #door spread sanity check
     i = which( (nm$doorspread > 90) & (nm$doorspread < 0) ) 
     if (length(i) > 0 ) {
       nm$doorspread[i]=NA
     }
    

    nm$wingspread[which(nm$wingspread>20)]=NA
    
    nm$wingspread[which(nm$wingspread<0)]=NA
    
    save( nm, file=fn, compress=TRUE)
  }
 }
  # either & or | can be used to add conditions