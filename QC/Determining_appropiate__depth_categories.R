# Summary of depth/doorspread prior to filtering
summary(modern.data$depth)
summary(modern.data$doorspread)


# Filtering
# depth sanity check
i = which( (modern.data$depth > 750) | (modern.data$depth < 0) ) 
if (length(i) > 0 ) {
  modern.data$depth[i]=NA
}
# doorspread sanity check
i = which( (modern.data$doorspread > 90) | (modern.data$doorspread < 0) ) 
if (length(i) > 0 ) {
  modern.data$doorspread[i]=NA
}


# Summary to depth/doorspread after filtering

#depth
summary(modern.data$depth)
# [number of NA's before filtering] - [number of NA's after filtering]
68751-49096 
# [number of NA's after filtering] / [number of NA's before filtering] = i
49096/68751 <- i
# 100 - 
100-0.7141132*100
# filtering increases the number of NA's from 49,096 to 68,751 (increase by 19,655, 28.59%)

# doorspread
summary(modern.data$doorspread)
384018-79648
49096/384018
100-0.1278482*100
# filtering increases the number of NA's from 79,648 to 384,018 (increase by 304,370, 87.21%)

modern.data=modern.data[sample (50000), ] 
plot(doorspread~depth, modern.data)
