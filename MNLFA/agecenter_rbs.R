##################################
# plot data to determine where to center age
##################################
df <- read.csv("/Users/sifre002/Box/sifre002/7_Rscripts/LongitudinalRBS/aMNLFA/2019-05-22cleanData_MNLFA.csv",
               header=TRUE)

myID = "ID2"
df$ranuni<-stats::runif(dim(df)[1], min = 0, max = 1)
df<-df[order(df[myID],df$ranuni),]
srdata<-df[!duplicated(df),]


# make means to plot 
rep_mean = srdata %>% select(REP1:REP9) %>%
  rowMeans(., na.rm=TRUE)
self_mean = srdata %>% select(SELF1:SELF7) %>%
  rowMeans(., na.rm=TRUE)
res_mean = srdata %>% select(RES1:RES8) %>%
  rowMeans(., na.rm = TRUE)
rit_mean = srdata %>% select(RIT1:RIT10) %>%
  rowMeans(., na.rm = TRUE)
res_rit_mean = srdata %>% select(RES1:RIT10) %>%
  rowMeans(., na.rm=TRUE)

plotData = srdata %>% 
  mutate(rep_mean, self_mean, rit_mean, res_mean, res_rit_mean)



ggplot(data = plotdata, aes(x = AGE)) + 
  geom_histogram(binwidth = 3, color = 'black') +
  facet_wrap(~PROJECT, nrow = 3) +
  scale_x_continuous(breaks = seq(0, 40, 3))


ggplot(data = plotData, aes(x = round(AGE), y = rep_mean)) + 
  geom_jitter(aes(color = PROJECT)) +
  scale_x_continuous(breaks = seq(9,40,2))

ggplot(data = plotdata, aes(x = round(AGE), y = self_mean)) + 
  geom_jitter(aes(color = PROJECT)) +
  scale_x_continuous(breaks = seq(2,40,2))

ggplot(data = plotdata, aes(x = round(AGE), y = res_rit_mean)) + 
  geom_jitter(aes(color = PROJECT)) +
  scale_x_continuous(breaks = seq(2,40,2))

ggplot(data = dichotData, aes(x = round(AGE), y = res_mean)) + 
  geom_jitter(aes(color = PROJECT))  + 
  scale_x_continuous(breaks = seq(9,40,2))

# center age where you see high variability 
plotdata = plotdata %>%
  mutate(AGE18 = AGE - 18,
         AGE8 = plotdata$AGE - min(plotdata$AGE) )


ggplot(data = plotdata, aes(x = round(AGE18), y = rep_mean)) + 
  geom_jitter(aes(color = PROJECT)) #+
scale_x_continuous(limits = c(-4,4))
# 18 looks like good age

ggplot(data = plotdata, aes(x = round(AGE2), y = self_mean)) + 
  geom_jitter(aes(color = PROJECT)) 

ggplot(data = plotdata, aes(x = round(AGE2), y = res_rit_mean)) + 
  geom_jitter(aes(color = PROJECT)) 

