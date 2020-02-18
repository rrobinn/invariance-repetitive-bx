#For longitudinal data, MNLFA requires a cross-sectional
#calibration sample. This code generates multiple calibration samples that have
#a similar age distribution

input.object = ob # MNLFA object
dir = input.object$dir
mrdata = input.object$mrdata
myindicators = input.object$indicators
myMeanImpact = input.object$meanimpact
myVarImpact = input.object$varimpact
myMeasInvar = input.object$measinvar
mytime = input.object$time
myauxiliary = input.object$auxiliary
myID = input.object$ID
varlist<-c(myID,myauxiliary,myindicators,myMeasInvar,myMeanImpact,myVarImpact)
varlist<-unique(varlist)

# Calculate mean and sd of longitudinal sample
# (AGE18 = age centered at 18mo)
my_mean = mean(mrdata$AGE18, na.omit = TRUE)
my_sd = sd(mrdata$AGE18)

# output variables
ran.list = list()
mean.list = list()
sd.list = list()

# Generate 1000 random samples
for (i in c(1:1000)) { 
  ranuni = stats::runif(dim(mrdata)[1], min = 0, max = 1)
  
  mrdata<-mrdata[order(mrdata[myID], ranuni),] # randomy shuffles based on ID 
  srdata<-mrdata[!duplicated(mrdata[myID]),]
  srdata<-srdata[varlist]
  
  mean.list[i] = mean(srdata$AGE18, na.omit = TRUE)
  sd.list[i] = sd(srdata$AGE18)
  ran.list[[i]] = ranuni
}
# turns lists into data.frames 
m = do.call(rbind, mean.list)
s = do.call(rbind, sd.list)
matches = data.frame(mean_age = m, mean_sd = s, i = c(1:length(m)))

#  find closest match 
matches$dist_from_mean = abs(matches$mean_age - my_mean)
matches$dist_from_sd = abs(matches$mean_sd - my_sd)
matches = matches %>% arrange(dist_from_mean, dist_from_sd)
matches[1:40,]
# potentially good ones: i = 845, 724, 521

##############################
# Optional code for examining sample age distributions
##############################
# test the samples to make sure they're not too similar
s1 = ran.list[[845]]
mrdata$ranuni = s1
mrdata<-mrdata[order(mrdata[myID],mrdata$ranuni),] # randomy shuffles based on ID 
srdata<-mrdata[!duplicated(mrdata[myID]),]
list1 =  paste(srdata$ID2, srdata$AGE, sep ='_')
dat1 =srdata %>% dplyr::select(ID2, AGE18_1 = AGE18)


s2 = ran.list[[724]]
mrdata$ranuni = s2
mrdata<-mrdata[order(mrdata[myID],mrdata$ranuni),] # randomy shuffles based on ID 
srdata<-mrdata[!duplicated(mrdata[myID]),]
list2 =  paste(srdata$ID2, srdata$AGE, sep ='_')
dat2 = srdata %>% dplyr::select(ID2, AGE18_2 = AGE18)

s3 = ran.list[[521]]
mrdata$ranuni = s3
mrdata<-mrdata[order(mrdata[myID],mrdata$ranuni),] # randomy shuffles based on ID 
srdata<-mrdata[!duplicated(mrdata[myID]),]
list3 =  paste(srdata$ID2, srdata$AGE, sep ='_')
dat3= srdata %>% dplyr::select(ID2, AGE18_3 = AGE18)


# look at overlap between samples 
length( setdiff(list1,list2) ) / length(list1) 
length( setdiff(list2,list1) ) / length(list1)

length( intersect(list1,list2) ) / nrow(srdata)   # 38% overlap
length( intersect(list1,list3) ) / nrow(srdata)   # 40% overlap
length( intersect(list2,list3) ) / nrow(srdata)   # 37% overlap

dat1 = dat1 %>% arrange(ID2)
dat2 = dat2 %>% arrange(ID2)
dat3 = dat3 %>% arrange(ID2)

# visualize correlation in age for each kid 
plot_data = merge(dat1, dat2, by = 'ID2')

ggplot(data = plot_data, aes(x = AGE18_1, y =AGE18_2)) +
  geom_point()
# compare age distributions to sample mean
ggplot(data = mrdata, aes(x = AGE18)) + 
  geom_histogram(color = 'black')

ggplot(data = dat1, aes(x = AGE18_1)) + 
  geom_histogram(color = 'black')

ggplot(data = dat2, aes(x = AGE18_2)) + 
  geom_histogram(color = 'black')

ggplot(data = dat3, aes(x = AGE18_3)) + 
  geom_histogram(color = 'black')

# 1 and 3

ru1 = ran.list[[845]]
ru2 = ran.list[[521]]
ru3 = ran.list[[724]]

ru1 = data.frame(ru1)
colnames(ru1)[1] = 'ru'

ru2 = data.frame(ru2)
colnames(ru2)[1] = 'ru'

ru3 = data.frame(ru3)
colnames(ru3)[1] = 'ru'

# Save a few calibration samples
write_csv(ru1, '/Users/sifre002/Desktop/Invariance/calib_sample1.csv')
write_csv(ru2, '/Users/sifre002/Desktop/Invariance/calib_sample2.csv')
write_csv(ru3, '/Users/sifre002/Desktop/Invariance/calib_sample3.csv')
