## Generating a data frame (r) with the percent of doorspread and wingspread values per set (id) 
allids=unique(master$id)

allids=unique(modern.data$id)
allids=unique(historical.data$id)
length(allids)

# create a null df which the for loop below will populate
r = data.frame(id = allids, d.perc = NA, w.perc = NA, stringsAsFactors = FALSE)

# To calculate the percent data generated for wingspread and doorspread per set and insert them into a df (r)
for (i in 1:nrow(r)){
  test = which(master$id==r[i,"id"])
  mm = master[test, ]
  n=length(test)
  dsval = length(which( is.finite(mm$doorspread)))
  r$d.perc[i] = (dsval/n)*100
  wsval = length(which( is.finite(mm$wingspread)))
  r$w.perc[i] = (wsval/n)*100
}

# save r dataframe
file="r.perc.RData"
save(r, file="r.perc.RData", compress=T)
load("r.perc.RData")

# Remove zeros froms from d.perc and w.perc
i = which(r$d.perc == 0)
b = which(r$w.perc == 0)


# Summaries and boxplots with the zeros removed
summary(r$d.perc[-i])
summary(r$w.perc[-b])
boxplot(r$d.perc[-i], main = "percent observations for doorspread")
boxplot(r$w.perc[-b], main = "percent observations for wingspread")

r1 = subset()



## create df (r.filter) which contains only those sets that have wingspread and doorspread data
r.d.filter = data.frame(id = r$id[-i], d.perc = r$d.perc[-i])
r.w.filter = data.frame(id = r$id[-b], w.perc = r$w.perc[-b])
str(r.d.filter)
summary(r.d.filter)
length(unique(r.d.filter$id))
str(r.w.filter)
summary(r.w.filter)
length(unique(r.w.filter$id))



## create a df with the ids from r.filter, containing d.perc and w.perc
r.filter.50 = data.frame(id = r.w.filter$id, d.perc = r.d.filter$d.perc, w.perc = r$w.perc)

## create df (r.d.filter) which contains set(id) and d.perc when d.perc does not equal zero
r.d.filter = data.frame(id = allids[-i], d.perc = r$d.perc[-i])
# create df (r.w.filter) which contains set(id) and w.perc when w.perc does not equal zero
r.w.filter = data.frame(id = allids[-b], w.perc = r$w.perc[-b])
# create df (r.dw.filter) which contains set(id), d.perc and w.perc, with no zero's incld
r.dw.filter = merge(r.d.filter, r.w.filter, by = "id") 


# select those sets with >= 50 d.perc and w.perc
r.filter.50 = NULL
r.dw.filter.50 = r.filter[which(r.filter$d.perc >= 50 & r.filter$w.perc >= 50),]
summary(r.dw.filter.50)

# Select the sets listed in df(r.dw.filter.50) and make a new df which these sets are subsetted from master (tdw)
allids2=r.dw.filter.50$id
tdw <- master[master$id == allids2, ]

## Calculate the linear model
dw.lm = lm(wingspread~doorspread, data=tdw)
summary(dw.lm)

# save linear model
file="wing.door.lm.RData"
save(dw.lm, file="wing.door.lm.RData", compress=T)
load("wing.door.lm.RData")

# save tdw
file="tdw.RData"
save(tdw, file="tdw.RData", compress=T)
load("tdw.RData")
tdw=(tdw[which(tdw$year %in% 2004:2014) , ]
     tdw = which( is.finite(tdw$doorspread) & tdw$wingspread > 0 )
     
     write.table(tdw, file= "Tdw_trawl.data.1.csv", sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE