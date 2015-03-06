## Generating a data frame (r) with the percent of doorspread and wingspread values per set (id) 
allids=unique(master$id)
allids=unique(modern.data$id)
allids=unique(historical.data$id)
length(allids)

# create a null df which the for loop below will populate
r = data.frame(id = allids, d.perc = NA, w.perc = NA, h.perc = NA, stringsAsFactors = FALSE)
uu = data.frame(id = allids, w.perc = NA, h.perc = NA, stringsAsFactors = FALSE)


# To calculate the percent data generated for wingspread and doorspread per set and insert them into a df (r)
for (i in 1:nrow(uu)){
  test = which(master$id==uu[i,"id"])
  mm = master[test, ]
  n=length(test)
  wsval = length(which( is.finite(mm$wingspread)))
  uu$w.perc[i] = (wsval/n)*100
  hhval = length(which( is.finite(mm$opening)))
  uu$h.perc[i] = (hhval/n)*100
}

# save r dataframe
file="r.perc.RData"
save(r, file="r.perc.RData", compress=T)
load("r.perc.RData")

# save uu dataframe
file="uu.perc.RData"
save(uu, file="uu.perc.RData", compress=T)
load("uu.perc.RData")

# Remove zeros froms from d.perc and w.perc
i = which(r$d.perc == 0)
b = which(uu$w.perc == 0)
c = which(uu$h.perc == 0)


# Summaries and boxplots with the zeros removed
summary(uu$h.perc[-c])
summary(uu$w.perc[-b])
boxplot(r$d.perc[-i], main = "percent observations for doorspread")
boxplot(uu$w.perc[-b], main = "percent observations for wingspread")
boxplot(uu$h.perc[-c], main = "percent observations for headline")

# data.frame (r1) which percentages of wingspread and doorspread greater than zero
r1 = subset(r, d.perc > 0 & w.perc > 0)
summary(r1)
# data.frame (r2) which percentages of wingspread and headline greater than zero
uu1 = subset(uu, h.perc > 0 & w.perc > 0)
summary(uu1)

# select those sets with >= 50 d.perc and w.perc
r.filter.50 = NULL
r.filter.50 = r1[which(r1$d.perc >= 50 & r1$w.perc >= 50),]
summary(r.filter.50)

# select those sets with >= 50 h.perc and w.perc
uu.50 = NULL
uu.50 = uu1[which(uu1$h.perc >= 50 & uu1$w.perc >= 50),]

# save uu.50 (based on 50% winsgpread and headline values)
file="uu.50.RData"
save(uu.50, file="uu.50.RData", compress=T)
load("uu.50.RData")

# save uu dataframe
file="uu.perc.RData"
save(uu, file="uu.perc.RData", compress=T)
load("uu.perc.RData")

head(uu.50)
ids = uu.50$id
length(ids)

goose = master[ which( (master$id %in% ids)  ), ]

# save goose (based on 50% winsgpread and headline values)
file="goose.RData"
save(goose, file="goose.RData", compress=T)
load("goose.RData")

##########################################

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