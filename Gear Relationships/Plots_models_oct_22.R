
# Relational plots

# global wingspread vs doorspread plot 
plot(doorspread~wingspread,nm, pch=20)

# global opening vs wingspread plot
plot(wingspread~opening,nm, pch= 20)

# global clearance vs opening
plot(clearance~opening,nm, pch= 21)

# global depth vs doorspread 
plot(doorspread~depth,nm, pch= 21)
plot(doorspread~depth,nm, xlim=c(0, 750), ylim=c(0,90), pch= 21)

# global depth vs wingspread 
plot(wingspread~depth,nm, pch= 21)
plot(wingspread~depth,nm, xlim=c(0, 200), ylim=c(0,15), pch= 21)



# Modeling

# wingspread, doorspread
o=lm(doorspread~wingspread, nm)
summary (o)
require(MASS)
r= rlm(doorspread~wingspread, nm)
summary (r)

cor(nm$wingspread, nm$doorspread, na.rm = TRUE, method = c(pearson))

# depth, doorspread
t=lm(doorspread~depth, nm)
summary (t)
require(MASS)
z= rlm(doorspread~depth, nm)
summary (r)

# loooking at the data within 2 S.E of the mean
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