con <- dbConnect(MySQL(), user="wikip_user", pass="wikiprojects1234", dbname="wikiprojects", host = "jacobsolomon.info")
d <- dbGetQuery(con, "select * from growth")

d <- subset(d, week <= (52*5+1))
d <- subset(d, project_id != 709)

temp <- tapply(d$cum_revisions, d$project_id, max)
d$max_revisions <- rep(temp, each=52*5+2)
d$perc_revisions <- d$cum_revisions / d$max_revisions

temp <- tapply(d$cum_talks, d$project_id, max)
d$max_talks <- rep(temp, each=52*5+2)
d$perc_talks <- d$cum_talks / d$max_talks

temp <- tapply(d$cum_regulars, d$project_id, max)
d$max_regulars <- rep(temp, each=52*5+2)
d$perc_regulars <- d$cum_regulars / d$max_regulars

temp <- tapply(d$cum_editors, d$project_id, max)
d$max_editors <- rep(temp, each=52*5+2)
d$perc_editors <- d$cum_editors / d$max_editors

temp <- tapply(d$cum_size, d$project_id, max)
d$max_size <- rep(temp, each=52*5+2)
d$perc_size <- d$cum_size / d$max_size

#remove projects that get fewer than 10 revisions in 5 years
d <- subset(d, max_revisions > 10)


d$cum_perc_new = unlist(tapply(d$perc_new, d$project_id, cumsum))
d$cum_editors_lost = unlist(tapply(d$editors_lost, d$project_id, cumsum))
d$cum_activity = unlist(tapply(d$active_editors, d$project_id, cumsum))
d$axl = d$cum_editors * d$cum_editors_lost

projects = as.data.frame(cbind(subset(d, week == 52)$project_id, subset(d, week == 208)$cum_revisions, subset(d, week == 52)$cum_perc_new, subset(d, week == 208)$cum_editors, subset(d, week == 52)$axl, subset(d, week == 52)$cum_editors, subset(d, week == 52)$cum_editors_lost, subset(d, week == 52)$cum_activity))
names(projects) = c("project_id", "total_revisions", 'y1_turnover', 'total_editors', 'y1_axl', 'y1_cum_editors', 'y1_cum_editors_lost', 'y1_cum_activity')

summary(lm(total_revisions~y1_turnover + total_editors, projects))

plot(projects$y1_turnover, projects$total_revisions)

d$cum_jaccard = unlist(tapply(d$jaccard, d$project_id, cumsum))
projects$y1_jaccard = subset(d, week == 52)$cum_jaccard


temp = c()

for (i in 1:208){
  data = subset(d, week == i)
  m = lm(sqrt(max_revisions)~I(log(cum_editors / cum_activity)), data)
  coef = m$coef[2]
  temp = c(temp, coef)
}

projects$y1_jaccard.m = unlist(tapply(subset(d, week < 53)$jaccard, subset(d, week < 53)$project_id, mean))
projects$y1_perc_new.m = unlist(tapply(subset(d, week < 53)$perc_new, subset(d, week < 53)$project_id, mean))

projects$to = projects$y1_cum_editors / projects$y1_cum_activity




# some analysis on the editors themselves

e = dbGetQuery(con, "select * from editors where first_post_week < 208")
e$duration = e$last_post_week - e$first_post_week

projects = merge(projects, allfits, by = "project_id")



projects = merge(projects, allfits, by = 'project_id')
p = dbGetQuery(con, "select * from projects")

projects = merge(p, projects, by.x = 'id', by.y = 'project_id')
names(projects)[20] = "growth_coef"




# This model shows that diversity in the first year has a huge effect on the eventual size of the project after 5 years, even when controlling for the number of editors and revisions at one year
# We use the square root of total revisions to normalize the distribution
m = lm(sqrt(y5_revisions)~diversity+y1_revisions + y1_editors, p)
# The model passed a breusch-pagan test for homoscedasticity and the visual inspection of normally distributed errors

library(stargazer)
stargazer(m)
library(ggplot2)
ggplot(p, aes(x=diversity, y=sqrt(y5_revisions))) + 
  geom_point(shape=19) + 
  geom_smooth(method = lm, lwd=2) + 
  theme_bw() + 
  ylab("Revisions after 5 years (square root)") + 
  xlab("Shannon Diversity Index in First Year")



projects.2 = dbGetQuery(con, "select id, breadth, diversity_projects from projects")
projects = merge(projects, projects.2, by = 'id')


library(gsl)
library(numDeriv)

source("powerlaw.R")

# A function for finding the elbow of a fitted powerlaw curve for a particular project

cutoff <- function(x) {
  # fit a power law curve
  m <- plm(x, xmin=1)
  alpha = m$alpha
  max_revisions = max(x)
  
  # If a power law doesn't fit, return NA
  if (summary(m)$p < .05){
    NA
  }
  
  # Fit the probability distribution and scale it to the data
  
  fit = (alpha - 1)*c(1:max_revisions)^(((-1*alpha)+1))
  
  #Create a second vector that represents the number of editors at each number of revision
  fit.s = fit*length(x)
  #print(fit.s)
  #Establish the min and max coordinates
  y.min = min(fit.s)
  #y.max = max(fit)
  
  #Loop through all points on the curve and find the one closest to (1, y.min)
  distances = c()
  for (i in 2:max_revisions-1) {
    dis = sqrt((i-1)^2 + (fit.s[i] - y.min)^2)
    distances = c(distances, dis)
  }
  
  which.min(distances) + 1
  
}

# Note- this takes a half an hour to run
cutoffs = c()
for (p in pids) {
  data = subset(e, project_id == p & y1_revisions > 0)$y1_revisions
  cutoff_point = cutoff(data)
  cutoffs = c(cutoffs, cutoff_point)
  print(p)
}


hist(cutoffs)
projects$cutoffs = cutoffs

# Let's put this into mysql so I don't have to run it ever again
pjcts = as.data.frame(cbind(pids, cutoffs))
dbWriteTable(con, name = 'projects_tmp', pjcts, row.names = T, field.types = NULL)
dbGetQuery(con, "update projects p, projects_tmp p2 set p.cutoffs = p2.cutoffs where p.id = p2.pids")


projects.2 = dbReadTable(con, "projects")

dbGetQuery(con, "update editors e set power_user = (select e.y1_revisions > projects.cutoffs from projects where e.project_id = projects.id)")


pu = dbGetQuery(con, "select project_id, count(*) as power_users from editors where power_user = 1 group by project_id")
projects.2 = merge(projects.2, pu, by.x = 'id', by.y = 'project_id')

names(allfits) = c("intercept", "B1", "growth_coef", "p", "r2", "category", "project_id")
dbWriteTable(con, name = "projects_fits", allfits, row.names = F)


projects.3 = dbGetQuery(con, "select * from projects join projects_fits on projects.id = projects_fits.project_id")
projects.3 = projects.3[,c(1:2,4:23)]

projects.3$accelerating = ifelse(projects.3$category == 'Increasing', 1,0)
projects.3$linear = ifelse(projects.3$category == 'Linear', 1,0)
projects.3$decelerating = ifelse(projects.3$category == "Decreasing",1,0)

projects.3 = merge(projects.3, pu, by.x = 'id', by.y = 'project_id')

dbGetQuery(con, "update projects set pu_revisions = (select sum(y1_revisions) from editors where project_id = projects.id and power_user = 1)")

hist(projects.3$non_power_users)


projects.3$pu_influence = projects.3$pu_revisions/projects.3$y1_revisions



############
# negative binomial regression works better

# The model comparing value of revisions to editors
# It is still useful to show the full model since it is easier to interpret, but instead of a square root transform use poisson regression

library(sandwich) #estimates robust standard errors for poisson regression. Necessary because of overdispersion
library(MASS)

m1 = lm(y5_revisions~y1_revisions*y1_editors, projects.3)

m2 = glm.nb(y5_revisions~y1_revisions*y1_editors, projects.3)

stargazer(m1,m2)

#coeftest(m2, vcov=sandwich) #This gives the correct (robust) standard errors and p-values



# Breadth of interest in other projects
m3 = glm.nb(y5_revisions~breadth + y1_editors, projects.3)
#coeftest(m3, vcov=sandwich)

#relative influence of power users to non power users and their revisions

m4 = glm.nb(y5_revisions~pu_revisions + npu_revisions, projects.3)
coeftest(m4, vcov=sandwich)




hist(projects.3$cutoffs)
ggplot(projects.3, aes(cutoffs)) + geom_histogram(binwidth=1) + theme_bw() + xlab("Cutoff points for power users")


a = dbGetQuery(con, "select project_id, max(active_editors) as max_active from growth where week < 52 group by project_id")
projects.3 = merge(projects.3, a, by='project_id')







## Some new graphs for the icswm poster

# Graph the relative effect of revisions vs. editors based on the negative binomial model
# I will plot this at at median level of each variable- so for the plot of the effect of revisions, what is the median number of editors after one year, and vice versa

# model = log(revisions at 5 years) = 6.57 + (.0002 * revisions_1yr) + (.020 * editors_1yr) + (-.00001 * editors * revisions)
median(subset(d, week == 52)$cum_revisions) # 80
median(subset(d, week == 52)$cum_editors) #36


editors.p = c()

for (i in 1 : 50){
  value = exp(6.57 + (80 * .0002) + (.020 * i) + (-.00001 * 80 * i))
  editors.p = c(editors.p, value)
}

revisions.p = c()
for (i in 1:75){
  value = exp(6.57 + (i * .0002) + (.020 * 36) + (-.00001 * i * 36))
  revisions.p = c(revisions.p, value)
}
yhat = c()
yhat = c(editors.p,revisions.p)
e.tmp = rep("editors", 50)
e2.tmp = c(1:50)
r.tmp = rep("revisions", 150)
r2.tmp = c(1:150)
variable = c(e.tmp,r.tmp)
number = c(e2.tmp, r2.tmp)

predicted = as.data.frame(cbind(yhat,variable, number))
names(predicted) = c("value", "type", "total")


library(ggplot2)

ggplot() + geom_line(aes(x = 1:75, y = revisions.p), color = "red") + geom_line(aes(x = 1:50, y = editors.p))


e.1 = c()
e.5 = c()
e.20 = c()
e.35 = c()
e.50 = c()
e.65 = c()
for (i in 1 : 150) {
  v.1 = exp(6.57 + (i * .0002) + (1 * .02) + (i * 1 * (-.00001)))
  v.5 = exp(6.57 + (i * .0002) + (5 * .02) + (i * 5 * (-.00001)))
  v.20 = exp(6.57 + (i * .0002) + (20 * .02) + (i * 20 * (-.00001)))
  v.35 = exp(6.57 + (i * .0002) + (35 * .02) + (i * 35 * (-.00001)))
  v.50 = exp(6.57 + (i * .0002) + (50 * .02) + (i * 50 * (-.00001)))
  v.65 = exp(6.57 + (i * .0002) + (65 * .02) + (i * 65 * (-.00001)))
  
  e.1 = c(e.1,v.1)
  e.5 = c(e.5,v.5)
  e.20 = c(e.20,v.20)
  e.35 = c(e.35,v.35)
  e.50 = c(e.50,v.50)
  e.65 = c(e.65,v.65)
}


ggplot() + theme_bw() + 
  geom_line(aes(x = 1:150, y = e.1), color = "black", size = 3) + 
  geom_line(aes(x = 5:150, y = e.5[5:150]), color = "orange", size = 3) + 
  geom_line(aes(x = 20:150, y = e.20[20:150]), color = "red", size = 3) + 
  geom_line(aes(x = 35:150, y = e.35[35:150]), color = "blue", size = 3) + 
  geom_line(aes(x = 50:150, y = e.50[50:150]), color = "green", size = 3) + 
  geom_line(aes(x = 65:150, y = e.65[65:150]), color = "purple", size = 3) 
  


# Power users graph

#model = 7.046 - (.001 * pu) + (.005 * npu)

ppu.60 = c()
ppu.80 = c()
ppu.100 = c()

for (i in 0:100){  
  value.60 = exp(7.046 - ((1 - (i/100)) *60 * .001) + ((i/100) * 60 * .005))
  value.80 = exp(7.046 - ((1 - (i/100)) *80 * .001) + ((i/100) * 80 * .005))
  value.100 = exp(7.046 - ((1 - (i/100)) * 100 * .001) + ((i/100) * 100 * .005))
  
  
  ppu.60 = c(ppu.60, value.60)
  ppu.80 = c(ppu.80, value.80)
  ppu.100 = c(ppu.100, value.100)
  
}
percentage = c(0:100)
tmp_frame = as.data.frame(cbind(ppu.60, ppu.80, ppu.100, percentage))
tmp_frame = melt(tmp_frame, id = "percentage")
tmp_frame$variable = ifelse(tmp_frame$variable == 'ppu.60', "60 edits in first year", ifelse(tmp_frame$variable == "ppu.80", "80 edits in first year", "100 edits in first year"))

ggplot(tmp_frame, aes(x = percentage, y = value, group = variable, color = variable)) + theme_bw() +
  geom_line(size = 3)


decel = c(197, 8,0)
lin = c(66,23,8)
accel = c(400,144,223)
type = c("Decelerating", "Linear", "Accelerating")
c_table = as.data.frame(cbind(decel,lin,accel,type))
names(c_table) = c("Decelerating", "Linear", "Accelerating", "Editor_shape")

editors_shape = c()
revision_shape = c()

for (i in 1:197){
  editors_shape = c(editors_shape, "Decelerating")
  revision_shape = c(revision_shape, "Decelerating")
}
for (i in 1:8){
  editors_shape = c(editors_shape, "Linear")
  revision_shape = c(revision_shape, "Decelerating")
}
for (i in 1:66){
  editors_shape = c(editors_shape, "Decelerating")
  revision_shape = c(revision_shape, "Linear")
}
for (i in 1:23){
  editors_shape = c(editors_shape, "Linear")
  revision_shape = c(revision_shape, "Linear")
}
for (i in 1:8){
  editors_shape = c(editors_shape, "Linear")
  revision_shape = c(revision_shape, "Accelerating")
}
for (i in 1:400){
  editors_shape = c(editors_shape, "Decelerating")
  revision_shape = c(revision_shape, "Accelerating")
}
for (i in 1:144){
  editors_shape = c(editors_shape, "Linear")
  revision_shape = c(revision_shape, "Accelerating")
}
for (i in 1:223){
  editors_shape = c(editors_shape, "Accelerating")
  revision_shape = c(revision_shape, "Accelerating")
}


v = as.data.frame(cbind(editors_shape, revision_shape))
mosaic(v$revision_shape ~ v$editors_shape)
