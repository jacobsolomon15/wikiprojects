d = read.csv("~/Documents/Wikiprojects/wikiprojects_time.csv", header = T)
d = subset(d, week4_revs > 0) #a little data cleaning- removes projects that only ever get 1 revision


year_revs = c("month12_revs", "month24_revs", "year3_revs", "year4_revs", "year5_revs")
year_size = c("month12_size", "month24_size", "year3_size", "year4_size", "year5_size")
year_editors = c("month12_editors", "month24_editors", "year3_editors", "year4_editors", "year5_editors")
year_talk = c("month12_talk", "month24_talk", "year3_talk", "year4_talk", "year5_talk")

month_revs = c("month1_revs", "month2_revs", "month3_revs", "month4_revs", "month5_revs", "month6_revs", "month9_revs", "month12_revs", "month15_revs", "month18_revs", "month21_revs", "month24_revs")
month_size = c("month1_size", "month2_size", "month3_size", "month4_size", "month5_size", "month6_size", "month9_size", "month12_size", "month15_size", "month18_size", "month21_size", "month24_size")
month_editors = c("month1_editors", "month2_editors", "month3_editors", "month4_editors", "month5_editors", "month6_editors", "month9_editors", "month12_editors", "month15_editors", "month18_editors", "month21_editors", "month24_editors")
month_talk = c("month1_talk", "month2_talk", "month3_talk", "month4_talk", "month5_talk", "month6_talk", "month9_talk", "month12_talk", "month15_talk", "month18_talk", "month21_talk", "month24_talk")

week_revs = c("day7_revs", "week2_revs", "week3_revs", "week4_revs")
week_size = c("day7_size", "week2_size", "week3_size", "week4_size")
week_editors = c("day7_editors", "week2_editors", "week3_editors", "week4_editors")
week_talk = c("day7_talk", "week2_talk", "week3_talk", "week4_talk")

day_revs = c("day1_revs", "day2_revs", "day3_revs", "day4_revs", "day5_revs", "day6_revs", "day7_revs")
day_editors = c("day1_editors", "day2_editors", "day3_editors", "day4_editors", "day5_editors", "day6_editors", "day7_editors")
day_size = c("day1_size", "day2_size", "day3_size", "day4_size", "day5_size", "day6_size", "day7_size")
day_talk = c("day1_talk", "day2_talk", "day3_talk", "day4_talk", "day5_talk", "day6_talk", "day7_talk")

revs = c("day1_revs", "day2_revs", "day3_revs", "day4_revs", "day5_revs", "day6_revs", "day7_revs", "week2_revs", "week3_revs", "week4_revs","month1_revs", "month2_revs", "month3_revs", "month4_revs", "month5_revs", "month6_revs", "month9_revs", "month12_revs", "month15_revs", "month18_revs", "month21_revs", "month24_revs","year3_revs", "year4_revs", "year5_revs")
size = c("day1_size", "day2_size", "day3_size", "day4_size", "day5_size", "day6_size", "day7_size", "week2_size", "week3_size", "week4_size","month1_size", "month2_size", "month3_size", "month4_size", "month5_size", "month6_size", "month9_size", "month12_size", "month15_size", "month18_size", "month21_size", "month24_size","year3_size", "year4_size", "year5_size")
editors = c("day1_editors", "day2_editors", "day3_editors", "day4_editors", "day5_editors", "day6_editors", "day7_editors", "week2_editors", "week3_editors", "week4_editors","month1_editors", "month2_editors", "month3_editors", "month4_editors", "month5_editors", "month6_editors", "month9_editors", "month12_editors", "month15_editors", "month18_editors", "month21_editors", "month24_editors","year3_editors", "year4_editors", "year5_editors")
talk = c("day1_talk", "day2_talk", "day3_talk", "day4_talk", "day5_talk", "day6_talk", "day7_talk", "week2_talk", "week3_talk", "week4_talk","month1_talk", "month2_talk", "month3_talk", "month4_talk", "month5_talk", "month6_talk", "month9_talk", "month12_talk", "month15_talk", "month18_talk", "month21_talk", "month24_talk","year3_talk", "year4_talk", "year5_talk")

other_vars = c("id", "active", "name")

d.revs = d[c(revs, other_vars)]
d.size = d[c(size, other_vars)]
d.editors = d[c(editors, other_vars)]
d.talk = d[c(talk,other_vars)]


timepoints = c(1,2,3,4,5,6,7,14,21,28,30,60,90,120,150,180,270,365,455,545,635,730,1095,1460,1825)
months = c(1,2,3,4,5,6,9,12,15,18,21,24)
days = c(1,2,3,4,5,6,7)
weeks = c(1,2,3,4)
years = c(1,2,3,4,5)

plot(1:7, mean(d[day_revs][1:7]), type = 'l', xlab = "Days", ylab = "Average Number of Revisions", main = "First Week")
plot(1:4, mean(d[week_revs][1:4]), type = 'l', xlab = "Weeks", ylab = "Average Number of Revisions", main = "First Month (weekly)")
plot(c(1:6, 9,12,15,18,21,24), mean(d[month_revs][1:12]), type = 'l', xlab = 'Months', ylab = 'Average Number of Revisions', main = 'First 2 years (monthly)')
plot(1:5, mean(d[year_revs][1:5]), type = 'l', xlab = 'Years', ylab = "Average Number of Revisions", main = "5 years")

plot(timepoints, mean(d[revs][1:length(timepoints)]), type = 'l', xlab = "Days", ylab = "Average Number of Revisions", main = "Wikiproject Growth")

#Pretty interesting plots. Growth is very linear, only a very mild little curve in the first week and after that, very linear growth
summary(lm(mean(d[revs][1:length(timepoints)]) ~ timepoints))

#Wow, r-squared of .99. 8 posts a day for 5 years, confirms that a linear model is best, maybe no need for non-linear modeling

#lets look at the size of projects
plot(1:7, mean(d[day_size][1:7]), type = 'l', xlab = "Days", ylab = "Average Size of Project (characters)", main = "First Week")
plot(1:4, mean(d[week_size][1:4]), type = 'l', xlab = "Weeks", ylab = "Average Size of Project (characters)", main = "First Month (weekly)")
plot(c(1:6, 9,12,15,18,21,24), mean(d[month_size][1:12]), type = 'l', xlab = 'Months', ylab = "Average Size of Project (characters)", main = 'First 2 years (monthly)')
plot(1:5, mean(d[year_size][1:5]), type = 'l', xlab = 'Years', ylab = "Average Size of Project (characters)", main = "5 years")

plot(timepoints, mean(d[size][1:length(timepoints)]), type = 'l', xlab = "Days", ylab = "Average Size of Project (characters)", main = "Wikiproject Growth")


#Now editors
plot(1:7, mean(d[day_editors][1:7]), type = 'l', xlab = "Days", ylab = "Average Number of Contributors", main = "First Week")
plot(1:4, mean(d[week_editors][1:4]), type = 'l', xlab = "Weeks", ylab = "Average Number of Contributors", main = "First Month (weekly)")
plot(c(1:6, 9,12,15,18,21,24), mean(d[month_editors][1:12]), type = 'l', xlab = 'Months', ylab = "Average Number of Contributors", main = 'First 2 years (monthly)')
plot(1:5, mean(d[year_editors][1:5]), type = 'l', xlab = 'Years', ylab = "Average Number of Contributors", main = "5 years")

plot(timepoints, mean(d[editors][1:length(timepoints)]), type = 'l', xlab = "Days", ylab = "Average Number of Contributors", main = "Wikiproject Growth")

#Now talk
plot(1:7, mean(d[day_talk][1:7]), type = 'l', xlab = "Days", ylab = "Average Number of Talk Revisions", main = "First Week")
plot(1:4, mean(d[week_talk][1:4]), type = 'l', xlab = "Weeks", ylab = "Average Number of Talk Revisions", main = "First Month (weekly)")
plot(c(1:6, 9,12,15,18,21,24), mean(d[month_talk][1:12]), type = 'l', xlab = 'Months', ylab = "Average Number of Talk Revisions", main = 'First 2 years (monthly)')
plot(1:5, mean(d[year_talk][1:5]), type = 'l', xlab = 'Years', ylab = "Average Number of Talk Revisions", main = "5 years")

plot(timepoints, mean(d[talk][1:length(timepoints)]), type = 'l', xlab = "Days", ylab = "Average Number of Talk Revisions", main = "Wikiproject Growth")


#Plot the same graphs using medians instead of means
plot(1:7, c(0,1,2,3,4,5,6), type = 'n', xlab = "Days", ylab = "Average Number of Revisions", main = "First Week")
for (i in 1:7){
  points(i, median(t(d[day_revs])[i,]), type = 'p')
}

plot(1:4, seq(from = 0, to = 10, by = (10 / 3)), type = "n", ylab = "Median number of revisions", xlab = "Weeks")
for (i in 1:4){
  points(i, median(t(d[week_revs])[i,]), type = 'p')
}

plot(months, seq(from = 0, to = 100, by = (100 / (length(months)-1))), type = "n", ylab = "Median number of revisions", xlab = "Months")
for (i in 1:length(months)){
  points(months[i], median(t(d[month_revs])[i,]), type = 'p')
}


plot(years, seq(from = 0, to = 400, by = (400 / (length(years)-1))), type = "n", ylab = "Median number of revisions", xlab = "Years")
for (i in 1:length(years)){
  lines(years[i], median(t(d[year_revs])[i,]), type = 'b')
}

plot(timepoints, seq(from = 0, to = 400, by = (400 / (length(timepoints)-1))), type = 'n', ylab = "Median revisions", xlab = 'Days')
for (i in 1:length(timepoints)) {
  lines(timepoints[i], median(t(d[revs])[i,]), type = 'b')
}


# Now use ggplot to see not only the trend but the standard error as well

# First, to really do this, you need to transform the data frame so that each row is a single observation (i.e. number of revs in one project at 3 days)
# The melt() function in the library reshape does this-
library(reshape)
#drops = c("active", "name")
#d.m = melt(d[,!names(d) %in% drops], id = 'id')

#you need variables that state the max number of each type of measurement
d$max_revs = d$year5_revs
d$max_size = d$year5_size
d$max_editors = d$year5_editors
d$max_talk = d$year5_talk

#Clean the "active" variable
d$active = ifelse(d$active == "yes", 1, ifelse(d$active == "yes}}", 1,0))


d.m = melt(d, id = c("id", "active", "name", "max_revs", "max_size", "max_editors", "max_talk"))
d.m$variable = as.character(d.m$variable)
d.m$id.n = as.numeric(d.m$id)

#This ^ created a data frame with just three variable- the id of the project, the name of the variable (i.e. day_5_revs) and the value of the measurement
# I still need to distinguish between different types of times and also different types of measurements
# So I will create a column to indicate the time of the measurement in number of days, as well as the type (revs, editors, talk)

# library(hash) # a library for creating hash tables or python-like dictionaries
# time_names = c("day1", "day2", "day3", "day4", "day5", "day6", "day7", "week2", "week3", "week4","month1", "month2", "month3", "month4", "month5", "month6", "month9", "month12", "month15", "month18", "month21", "month24","year3", "year4", "year5")
# time_dict = hash(keys = time_names, values = timepoints)

d.m$days = 0
d.m$time_name = NA
d.m$type = NA

substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}

getTime_Name <- function(x){
  strsplit(x, "_")[[1]][1]
}

getType <- function(x){
  strsplit(x, "_")[[1]][2]
}

getDay <- function(x){
  ifelse(substr(as.character(x$time_name),1,1) == "d", as.numeric(substrRight(x$time_name, 1)),
         ifelse(substr(as.character(x$time_name),1,1) == "w", as.numeric(substrRight(x$time_name, 1)) * 7,
                ifelse(substr(as.character(x$time_name),1,1) == "m", as.numeric(substr(x$time_name, 6, nchar(x$time_name))) * 30,
                       as.numeric(substrRight(x$time_name, 1)) * 365)))
}

getNumberOfGroups <- function(x) {
  wss <- (nrow(x)-1)*sum(apply(x,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(x, 
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


d.m$time_name = lapply(d.m$variable, getTime_Name)
d.m$type = lapply(d.m$variable, getType)
d.m$day = ifelse(substr(as.character(d.m$time_name),1,1) == "d", as.numeric(substrRight(d.m$time_name, 1)),
                 ifelse(substr(as.character(d.m$time_name),1,1) == "w", as.numeric(substrRight(d.m$time_name, 1)) * 7,
                        ifelse(substr(as.character(d.m$time_name),1,1) == "m", as.numeric(substr(d.m$time_name, 6, nchar(d.m$time_name))) * 30,
                               as.numeric(substrRight(d.m$time_name, 1)) * 365)))




#Now do some visualizing
library(ggplot2)

qplot(day, value, data = subset(d.m, type == 'revs'), geom = c("point","smooth"))
qplot(day, value, data = subset(d.m, type == 'size'), geom = c("point","smooth"))
qplot(day, value, data = subset(d.m, type == 'editors'), geom = c("point","smooth"))
qplot(day, value, data = subset(d.m, type == 'talk'), geom = c("point","smooth"))



#We have to make a small adjustment to the value in order to get the log- adding .00001 so that there are no zero's
d.m$value.a = d.m$value + .00001

qplot(log(value), data=subset(d.m, type == 'revs'), geom = 'histogram')
qplot(log(value), data=subset(d.m, type == 'size'), geom = 'histogram')
qplot(log(value), data=subset(d.m, type == 'editors'), geom = 'histogram')
qplot(log(value), data=subset(d.m, type == 'talk'), geom = 'histogram') #won't display for some reason
hist(log(subset(d.m, type == 'talk')$value))

qplot(day, log(value.a), data = subset(d.m, type == 'revs'), geom = c("point","smooth"), method = "gam")
qplot(day, log(value.a), data = subset(d.m, type == 'size'), geom = c("point","smooth"), method = "gam")
qplot(day, log(value.a), data = subset(d.m, type == 'editors'), geom = c("point","smooth"), method = "gam")
qplot(day, log(value.a), data = subset(d.m, type == 'talk'), geom = c("point","smooth"), method = "rlm")

#create a separate data frame for each type of measurement
# revs = subset(d.m, type == 'revs')
# size = subset(d.m, type == 'size')
# editors = subset(d.m, type == 'editors')
# talk = subset(d.m, type == 'talk')

# One issue is that the scale of each project seems to be different even though the shape of the growth is the same
# Maybe I should transform the value into a percentage of their 5 year value

revs$percentage = revs$value / revs$max_revs
size$percentage = size$value / size$max_size
editors$percentage = editors$value / editors$max_editors
talk$percentage = talk$value / talk$max_talk

p1 <- qplot(day, percentage, data=revs)
qplot(day, percentage, data = size, geom = c("point", "smooth"))
qplot(day, percentage, data=editors, geom = c("point","smooth"))
qplot(day, percentage, data = talk, geom = c("point", "smooth"))

revs.s = subset(revs, max_revs > 1)



# A cluster analysis might tell me if there are common patterns
# Start with the revs

# create a distance matrix-
# Canberra distance is probably the best for this kind of data
# Canberra distance is measured by taking the absolute difference between two projects at each time point, 
# then dividing by the sum of the two projects at each time point to create a ratio, then summing the ratios 
# for all time points:

# sum_for_all_i(|P1_i - P2_i| / (|P1_i| + |P2_i|))

# This method seems like it should work well for data measured in counts, where there are never values below 0.
# Since we are measuring growth, which never goes down, this should be useful

revs.z = dist(d[revs], method = "canberra")

# Now I have to figure out how many clusters there are-
# Below is a method for determining the right number of clusters

# Determines the within-timepoint sum of square
wss <- (nrow(d[revs])-1)*sum(apply(d[revs],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(d[revs], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Interesting- it looks like the best choices are either 4-5 or 9.
# The plot turns horizontal at about 4 or 5, then takes a sudden dive after 8 but immediately goes horizontal again
# I can try both ways to see which will work best

f = hclust(revs.z, method = 'ward')
groups.revs.5 = cutree(f, k = 5)
plot(f)
rect.hclust(f, k = 5, border = 'red')

# Add the group number to the original data frame, then remelt
d$group.revs.5 = groups.revs.5
d.m = merge(d.m, d[c("id", "group.revs.5")], by = "id")


qplot(day, value, data = subset(d.m, type == 'revs' & group.revs.5 == 1), geom = c("point", "smooth"))

# I don't think 5 groups was enough
# Going to try 9

groups.revs.9 = cutree(f, k = 9)
d$group.revs.9 = groups.revs.9
d.m = merge(d.m, d[c("id", "group.revs.9")], by = "id")

qplot(day, value, data = subset(d.m, type == 'revs' & group.revs.9 == 1), geom = c("point", "smooth"))


# I think I need to standardize the measurements by the project
standardize <- function(x) {
  (x - mean(x)) / (sd(x))
}

d.revs.s = as.data.frame(t(apply(d[revs], 1, standardize)))
d.revs.s[other_vars] = d.revs[other_vars]

#Remove the NaN rows, make the day variable and melt the data
d.revs.s = d.revs.s[complete.cases(d.revs.s),]


f.revs = hclust(dist(d.revs.s[revs]), method = 'ward')

getNumberOfGroups(d.revs.s[revs])

# I'm going to start with 4 groups
gr4 = cutree(f.revs, k = 4)
d.revs.s$group = gr4
d.revs$group = gr4
d.revs.s.m = melt(d.revs.s, id = c(other_vars, "group"))
d.revs.s.m$time_name = lapply(as.character(d.revs.s.m$variable), getTime_Name)
d.revs.s.m$type = lapply(as.character(d.revs.s.m$variable), getType)
d.revs.s.m$day = getDay(d.revs.s.m)

g1.revs <- ggplot(subset(d.revs.s.m, group == 1), aes(day, value)) + opts(title = "Revisions- Group 1") + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g1.revs
# Group 1 starts of with slow growth, then it starts to really pick up after about a year and a half
g2.revs <- ggplot(subset(d.revs.s.m, group == 2), aes(day, value)) + opts(title = "Revisions- Group 2")
g2.revs <- g2.revs + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g2.revs
# Group 2 starts fairly quickly, slows down after a month but maintains steady growth, then slows a little more after 1.5 years but still continues to grow
g3.revs <- ggplot(subset(d.revs.s.m, group == 3), aes(day,value))
g3.revs <- g3.revs + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Revisions- Group 3")
g3.revs
# Group 3 is fairly linear growth from start to finish
g4.revs <- ggplot(subset(d.revs.s.m, group == 4), aes(day,value)) + opts(title = "Revisions- Group 4")
g4.revs <- g4.revs + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g4.revs
# Group 4 grows really fast at the beginning, then flattens off

#put put the standardized dataset with the groups back with the original dataset
d.revs = merge(d.revs, d.revs.s, by = "id")

#Look at which group ends up with the most revisions
summary(lm(year5_revs.x~as.factor(group), d.revs))
# Group 1 ends up with 772, Group 3 with 570, group 2 with 329, and group 4 with 150
hist(d.revs$year5_revs.x)
# But we have a power law distribution- so ols isn't reliable. Probably I need to use a non-parametric method
# trying a kruskal wallis test- a non paremetric anova. As best I can tell the data satisfy all assumptions for this test

kruskal.test(year5_revs.x ~ as.factor(group), data = d.revs)
median(subset(d.revs, group == 1)$year5_revs.x) # 592
median(subset(d.revs, group == 2)$year5_revs.x) # 187.5
median(subset(d.revs, group == 3)$year5_revs.x) # 386
median(subset(d.revs, group == 4)$year5_revs.x) # 82

median(subset(d.revs, group == 1)$year3_revs.x) # 238
median(subset(d.revs, group == 2)$year3_revs.x) # 171.5
median(subset(d.revs, group == 3)$year3_revs.x) # 253
median(subset(d.revs, group == 4)$year3_revs.x) # 79

median(subset(d.revs, group == 1)$month12_revs.x) # 24
median(subset(d.revs, group == 2)$month12_revs.x) # 94.5
median(subset(d.revs, group == 3)$month12_revs.x) # 75
median(subset(d.revs, group == 4)$month12_revs.x) # 71


# Now lets do talk pages

d.talk = d[talk]
d.talk.s = as.data.frame(t(apply(d[talk], 1, standardize)))
d.talk.s[other_vars] = d[other_vars]
d.talk.s = d.talk.s[complete.cases(d.talk.s),]

f.talk = hclust(dist(d.talk.s[talk]), method = 'ward')
getNumberOfGroups(d.talk.s[talk])

# 5 or 6 might be better, but for now I am just going to use 4 to keep it consistent with revs

gt4 = cutree(f.talk, k = 4)
d.talk.s$gt4 = gt4

d.talk.s.m = melt(d.talk.s, id = c(other_vars, "gt4"))
d.talk.s.m$time_name = lapply(as.character(d.talk.s.m$variable), getTime_Name)
d.talk.s.m$type = lapply(as.character(d.talk.s.m$variable), getType)
d.talk.s.m$day = getDay(d.talk.s.m)

g1.talk <- ggplot(subset(d.talk.s.m, gt4 == 1), aes(day,value))
g1.talk <- g1.talk + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Talk revisions- Group 1")
g1.talk
# Flat for 500 days, then strong linear growth

g2.talk <- ggplot(subset(d.talk.s.m, gt4 == 2), aes(day, value)) + opts(title = "Talk revisions- Group 2")
g2.talk <- g2.talk + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g2.talk
# Looks like group 4 from rev, very fast start flattens out quickly
g3.talk <- ggplot(subset(d.talk.s.m, gt4 == 3), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Talk revisions- Group 3")
g3.talk
# A similar pattern to group 1 talk, except the growth spurt starts later, more like 2 and a half years
g4.talk <- ggplot(subset(d.talk.s.m, gt4 == 4), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Talk revisions- Group 4")
g4.talk
# The closest to straight linear growth, but still has a slow decay
d.talk[other_vars] = d[other_vars]
d.talk = merge(d.talk, d.talk.s, by = "id")
CrossTable(d.talk$active.x, d.talk$gt4)

#Get medians for each group

median(subset(d.talk, gt4 == 1)$year5_talk.x) # 129
median(subset(d.talk, gt4 == 2)$year5_talk.x) # 51.5
median(subset(d.talk, gt4 == 3)$year5_talk.x) # 84
median(subset(d.talk, gt4 == 4)$year5_talk.x) # 74

median(subset(d.talk, gt4 == 1)$year3_talk.x) # 86
median(subset(d.talk, gt4 == 2)$year3_talk.x) # 47
median(subset(d.talk, gt4 == 3)$year3_talk.x) # 2
median(subset(d.talk, gt4 == 4)$year3_talk.x) # 52

median(subset(d.talk, gt4 == 1)$month12_talk.x) # 0
median(subset(d.talk, gt4 == 2)$month12_talk.x) # 37.5
median(subset(d.talk, gt4 == 3)$month12_talk.x) # 0
median(subset(d.talk, gt4 == 4)$month12_talk.x) # 11

# Now Editors

d.editors = d[editors]
d.editors.s = as.data.frame(t(apply(d[editors], 1, standardize)))
d.editors.s[other_vars] = d[other_vars]
d.editors.s = d.editors.s[complete.cases(d.editors.s),]

f.editors = hclust(dist(d.editors.s[editors]), method = 'ward')
getNumberOfGroups(d.editors.s[editors]) # 4 still works probably

ge4 = cutree(f.editors, k = 4)
d.editors.s$ge4 = ge4

d.editors.s.m = melt(d.editors.s, id = c(other_vars, "ge4"))
d.editors.s.m$time_name = lapply(as.character(d.editors.s.m$variable), getTime_Name)
d.editors.s.m$type = lapply(as.character(d.editors.s.m$variable), getType)
d.editors.s.m$day = getDay(d.editors.s.m)

g1.editors <- ggplot(subset(d.editors.s.m, ge4 == 1), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Editors- Group 1")
g1.editors
# Linear more or less
g2.editors <- ggplot(subset(d.editors.s.m, ge4 == 2), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Editors- Group 2")
g2.editors
# slow growth then a jumpstart at 500 days
g3.editors <- ggplot(subset(d.editors.s.m, ge4 == 3), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Editors- Group 3")
g3.editors
# fast start then decay and flattening
g4.editors <- ggplot(subset(d.editors.s.m, ge4 == 4), aes(day, value)) + stat_summary(fun.data = "mean_sdl", geom = "smooth") + opts(title = "Editors- Group 4")
g4.editors
# a slower gradual decay

# The conclusion here is that there isn't much variation in how projects grow editors, it is pretty much linear growth from start to finish, just maybe a difference in how fast they grow

d.editors[other_vars] = d[other_vars]
d.editors = merge(d.editors, d.editors.s, by = "id")

median(subset(d.editors, ge4 == 1)$year5_editors.x) # 97
median(subset(d.editors, ge4 == 2)$year5_editors.x) # 139.5
median(subset(d.editors, ge4 == 3)$year5_editors.x) # 33
median(subset(d.editors, ge4 == 4)$year5_editors.x) # 67

median(subset(d.editors, ge4 == 1)$year3_editors.x) # 66
median(subset(d.editors, ge4 == 2)$year3_editors.x) # 63.5
median(subset(d.editors, ge4 == 3)$year3_editors.x) # 30.5
median(subset(d.editors, ge4 == 4)$year3_editors.x) # 55

median(subset(d.editors, ge4 == 1)$month12_editors.x) # 14
median(subset(d.editors, ge4 == 2)$month12_editors.x) # 8
median(subset(d.editors, ge4 == 3)$month12_editors.x) # 19
median(subset(d.editors, ge4 == 4)$month12_editors.x) # 23

# Now Size

d.size = d[size]
d.size.s = as.data.frame(t(apply(d[size], 1, standardize)))
d.size.s[other_vars] = d[other_vars]
d.size.s = d.size.s[complete.cases(d.size.s),]

f.size = hclust(dist(d.size.s[size]), method = 'ward')
getNumberOfGroups(d.size.s[size]) # 4 still works probably

gs4 = cutree(f.size, k = 4)
d.size.s$gs4 = gs4

gs8 = cutree(f.size, k = 8)
d.size.s$gs8 = gs8

d.size.s.m = melt(d.size.s, id = c(other_vars, "gs4","gs8"))
d.size.s.m$time_name = lapply(as.character(d.size.s.m$variable), getTime_Name)
d.size.s.m$type = lapply(as.character(d.size.s.m$variable), getType)
d.size.s.m$day = getDay(d.size.s.m)

g1.size <- ggplot(subset(d.size.s.m, gs4 == 1), aes(day, value)) + opts(title = "Size- Group 1") + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g1.size
# flat until 500, then a strong upward swing
g2.size <- ggplot(subset(d.size.s.m, gs4 == 2), aes(day, value)) + opts(title = "Size- Group 2") + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g2.size
# flat until 900, then upward
g3.size <- ggplot(subset(d.size.s.m, gs4 == 3), aes(day, value)) + opts(title = "Size- Group 3") + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g3.size
# decaying growth
g4.size <- ggplot(subset(d.size.s.m, gs4 == 4), aes(day, value)) + opts(title = "Size- Group 4") + stat_summary(fun.data = "mean_sdl", geom = "smooth")
g4.size
# closer to linear growth

d.size[other_vars] = d[other_vars]
d.size = merge(d.size, d.size.s, by = "id")

median(subset(d.size, gs4 == 1)$year5_size.x) # 4299784
median(subset(d.size, gs4 == 2)$year5_size.x) # 5732221
median(subset(d.size, gs4 == 3)$year5_size.x) # 1644647
median(subset(d.size, gs4 == 4)$year5_size.x) # 4360300

median(subset(d.size, gs4 == 1)$year3_size.x) # 2112930
median(subset(d.size, gs4 == 2)$year3_size.x) # 849181.5
median(subset(d.size, gs4 == 3)$year3_size.x) # 1317850
median(subset(d.size, gs4 == 4)$year3_size.x) # 2996877

median(subset(d.size, gs4 == 1)$month12_size.x) # 59606
median(subset(d.size, gs4 == 2)$month12_size.x) # 26292.5
median(subset(d.size, gs4 == 3)$month12_size.x) # 815960.5
median(subset(d.size, gs4 == 4)$month12_size.x) # 564769






# merge the final groups back into the original data

d.final = merge(d[other_vars], d.revs[c("id", "group")], by = "id", all.x = T)
d.final = merge(d.final, d.size.s[c("id", "gs4")], by = "id", all.x = T)
d.final = merge(d.final, d.editors.s[c("id", "ge4")], by = "id", all.x = T)
d.final = merge(d.final, d.talk.s[c("id", "gt4")], by = "id", all.x = T)
plot(as.factor(d.final$group), as.factor(d.final$gt4))
plot(as.factor(d.final$group), as.factor(d.final$ge4))


# From the plot, it looks like there is pretty good correlation between shapes of revision and shapes of talk growth curves
# 

CrossTable(as.factor(d.final$group), as.factor(d.final$gt4))
chisq.test(as.factor(d.final$group), as.factor(d.final$gt4))


# You need to give the shapes names to make things consisten across measurements

# ss- slow starters- flat for 500+ days then a sudden upward swing
# linear- generally linear growth for 5 years
# gd- gradual decay- a slow negative growth decay
# ep- early plateau- grows very fast then suddenly levels off

d.final$shape.revs = ifelse(d.final$group == 1, "ss",
                            ifelse(d.final$group == 2,"gd",
                                   ifelse(d.final$group == 3, "linear",
                                          ifelse(d.final$group == 4, "ep",d.final$group))))

d.final$shape.talk = ifelse(d.final$gt4 == 1, "ss",
                            ifelse(d.final$gt4 == 2,"ep",
                                   ifelse(d.final$gt4 == 3, "ss",
                                          ifelse(d.final$gt4 == 4, "gd",d.final$gt4))))

d.final$shape.editors = ifelse(d.final$ge4 == 1, "linear",
                          ifelse(d.final$ge4 == 2,"ss",
                                 ifelse(d.final$ge4 == 3, "ep",
                                        ifelse(d.final$ge4 == 4, "gd",d.final$ge4))))

d.final$shape.size = ifelse(d.final$gs4 == 1, "ss",
                               ifelse(d.final$gs4 == 2,"ss",
                                      ifelse(d.final$gs4 == 3, "gd",
                                             ifelse(d.final$gs4 == 4, "linear",d.final$gs4))))
shape = c("shape.revs", "shape.talk", "shape.editors", "shape.size")
d.final$shape.revs = as.factor(d.final$shape.revs)
d.final$shape.talk = as.factor(d.final$shape.talk)
d.final$shape.editors = as.factor(d.final$shape.editors)
d.final$shape.size = as.factor(d.final$shape.size)



plot(d.final$shape.revs, d.final$shape.talk, xlab = "Revisions", ylab = "Talk")
plot(d.final$shape.revs, d.final$shape.editors, xlab = 'Revisions', ylab = 'Editors')
plot(d.final$shape.revs, d.final$shape.size, xlab = 'Revisions', ylab = "Size")
plot(d.final$shape.talk, d.final$shape.editors, xlab = "Talk", ylab = "Editors")
plot(d.final$shape.talk, d.final$shape.size, xlab = "Talk", ylab = "Size")
plot(d.final$shape.size, d.final$shape.editors, xlab = "Size", ylab = "Editors")

plot(as.factor(d.final$active), d.final$shape.revs, xlab = "Active", ylab = "Shape of Revision Growth")
plot(as.factor(d.final$active), d.final$shape.talk, xlab = "Active", ylab = "Shape of Talk Growth")
plot(as.factor(d.final$active), d.final$shape.editors, xlab = "Active", ylab = "Shape of Editors Growth")
plot(as.factor(d.final$active), d.final$shape.size, xlab = "Active", ylab = "Shape of Size Growth")



d.revs = merge(d.revs, d.final[c('id', 'shape.revs')], by = 'id')







