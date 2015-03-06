# Can we make this a function and put in in net_mensuration.db.r?


# removing the trips that fish the US trawl 
i = grep("NED2014102", master$id)
master = master[-c(i),]


library(xlsx)
mydata <- read.xlsx("C:/Users/mundenj/Desktop/Scanmar/NED2013028.xlsx", 1)
names(mydata)=tolower(names(mydata))
mydata$id=paste(mydata$mission,mydata$setno,sep=".")
l = mydata$id
master <- master[ ! master$id %in% l, ]

# check
master$year=as.numeric(substring(master$id,4,7))
str(master)
master.2013=master[which(master$year == '2013'),]
unique(master.2013$id)

mission="NED2013028"
setno =c(1, 4, 5, 8, 9, 12, 13, 16, 17, 20, 21, 24, 25, 28, 
         33, 34, 37, 38, 41, 42, 45, 46, 49, 50, 54, 55, 
         58, 59, 60, 64, 65, 68, 69, 72, 73, 76, 77, 80, 81, 
         84, 85, 88, 89, 92, 93, 96, 97, 100, 101, 104, 105, 
         108, 109, 112, 113, 116, 117, 120, 121, 124, 125, 
         128, 129, 132, 133, 136, 137, 140, 141, 144, 145, 
         148, 149, 152, 153,  156,  157,  160,  161,  164,  
         165,  168,  169,  174,  175,  178,  179,  
         182,  183,  186,  187,  190)
