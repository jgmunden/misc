
sm$hours=substring(sm$logtime,1,2)

sm$min=substring(sm$logtime,3,4)

sm$sec=substring(sm$logtime,5,6)

sm$timestamp= paste(sm$year,"01", "01", sm$hours, sm$min, sm$sec, sep="-" )

library(lubridate)

sm$timestamp = ymd_hms(sm$timestamp)