library(lubridate); library(dummies); library(randomForest); library(rpart); library(rattle)
library(rpart.plot); library(RColorBrewer)

#----------------
# PREPROCESSING 
#----------------

rm(list=ls())
d1 = read.csv("./raw_data/99.csv"); 
d2 = read.csv("./raw_data/100.csv"); d2$casual<-d2$registered<-d2$count<-NA; 
d = rbind(d1,d2); rm(d1,d2)
train<-which(!is.na(d$count)); test<-which(is.na(d$count))

# aggiusto stagioni
d$season <- as.factor(d$season)
levels(d$season) <- c('spring', 'summer', 'fall', 'winter')

# VARIABILI TEMPORALI
d$hour<-as.integer(substr(d$datetime,12,13))
d$day<-as.integer(substr(d$datetime,9,10))
d$wday<-wday(substr(d$datetime,1,10),label = T)
d$month<-as.integer(substr(d$datetime, 6,7))
d$year<-as.integer(substr(d$datetime, 1,4))
d$datetime<-NULL

# WEATHER=4 HA SOLO 3 OBS
d$weather[d$weather==4]<-3

# DIVIDO 'ORA' IN BASE A REGISTERED/CASUAL
# windows()

fit.tree=rpart(registered~hour,data=d[train,])
# fancyRpartPlot(fit.tree)
d$hour.cut.reg <- 0
d$hour.cut.reg[d$hour<7] <- 1
d$hour.cut.reg[d$hour>=21] <- 2
d$hour.cut.reg[d$hour<=16&d$hour>=9] <- 3
d$hour.cut.reg[d$hour==7] <- 4
d$hour.cut.reg[d$hour==8] <- 5
d$hour.cut.reg[d$hour==19|d$hour==20] <- 6
d$hour.cut.reg[d$hour==17|d$hour==18] <- 7
# table(d$hour.cut.reg)

fit.tree=rpart(casual~hour,data=d[train,])
# fancyRpartPlot(fit.tree)
d$hour.cut.cas <- 0
d$hour.cut.cas[d$hour<=7] <- 1
d$hour.cut.cas[d$hour==8|d$hour==9] <- 2
d$hour.cut.cas[d$hour>=20] <- 3
d$hour.cut.cas[d$hour>=10&d$hour<20] <- 4
# table(d$hour.cut.cas)


# DIVIDO 'TEMP' IN BASE A REGISTERED/CASUAL

fit.tree=rpart(registered~temp,data=d[train,])
# fancyRpartPlot(fit.tree)
d$temp.reg=0
d$temp.reg[d$temp<13]=1
d$temp.reg[d$temp>=13 & d$temp<23]=2
d$temp.reg[d$temp>=23 & d$temp<30]=3
d$temp.reg[d$temp>=30]=4
# table(d$temp.reg)

fit.tree=rpart(casual~temp,data=d[train,])
# fancyRpartPlot(fit.tree)
d$temp.cas=0
d$temp.cas[d$temp<15]=1
d$temp.cas[d$temp>=15 & d$temp<23]=2
d$temp.cas[d$temp>=23 & d$temp<30]=3
d$temp.cas[d$temp>=30]=4
# table(d$temp.cas)


# DIVIDO 'HUMIDITY' IN BASE A REGISTERED/CASUAL

fit.tree=rpart(registered~humidity,data=d[train,])
# fancyRpartPlot(fit.tree)
d$humidity.reg=0
d$humidity.reg[d$humidity>=67]=1
d$humidity.reg[d$humidity>=44 & d$humidity<67]=2
d$humidity.reg[d$humidity<44]=3
# table(d$humidity.reg)

fit.tree=rpart(casual~humidity,data=d[train,])
# fancyRpartPlot(fit.tree)
d$humidity.cas=0
d$humidity.cas[d$humidity>=75]=1
d$humidity.cas[d$humidity>=56 & d$humidity<75]=2
d$humidity.cas[d$humidity>=40 & d$humidity<56]=3
d$humidity.cas[d$humidity<40]=4
# table(d$humidity.cas)

# WEEKDAY/WEEKEND/WORKINGDAY
d$day_type=""
d$day_type[d$holiday==0 & d$workingday==0]="weekend"
d$day_type[d$holiday==1]="holiday"
d$day_type[d$holiday==0 & d$workingday==1]="working day"
#table(d$day_type)

# WEEKEND
d$weekend="no"
d$weekend[d$wday=="dom" | d$wday=="sab" ]='yes'
#table(d$weekend)

# INDICI VARI REGISTERED
d$mean.reg.weekend.hour <- NA
for(w in c('yes','no')){
  for(t in 0:23){
    quali=which(d$weekend==w & d$hour==t)  
    d$mean.reg.weekend.hour[quali]=mean(log1p(d$registered[quali]),na.rm=T)
  }}

# INDICI VARI CASUAL
d$mean.cas.weekend.hour <- NA
for(w in c('yes','no')){
  for(t in 0:23){
    quali=which(d$weekend==w & d$hour==t)  
    d$mean.cas.weekend.hour[quali]=mean(log1p(d$casual[quali]),na.rm=T)
  }}

# DISCRETIZZO VARIABILI
d$holiday <- as.factor(ifelse(d$holiday==1, 'yes', 'no'))
d$workingday <- as.factor(ifelse(d$workingday==1, 'yes', 'no'))
d$weather <- as.factor(d$weather); levels(d$weather)<- letters[1:length(levels(d$weather))]
d$hour.cut.reg <- as.factor(d$hour.cut.reg); levels(d$hour.cut.reg)<- letters[1:length(levels(d$hour.cut.reg))]
d$hour.cut.cas <- as.factor(d$hour.cut.cas); levels(d$hour.cut.cas)<- letters[1:length(levels(d$hour.cut.cas))]
d$temp.reg <- as.factor(d$temp.reg); levels(d$temp.reg)<- letters[1:length(levels(d$temp.reg))]
d$temp.cas <- as.factor(d$temp.cas); levels(d$temp.cas)<- letters[1:length(levels(d$temp.cas))]
d$humidity.reg <- as.factor(d$humidity.reg); levels(d$humidity.reg)<- letters[1:length(levels(d$humidity.reg))]
d$humidity.cas <- as.factor(d$humidity.cas); levels(d$humidity.cas)<- letters[1:length(levels(d$humidity.cas))]
d$weekend <- as.factor(d$weekend)
d$day_type <- as.factor(d$day_type)

# STIMO VALORI MANCANTI WINDSPEED
zeri<-which(d$windspeed==0)
# dwind_train<-d[-zeri,]
# dwind_test<-d[zeri,]; dwind_test$windspeed<-NULL
# myrf_wind <- randomForest(windspeed ~ season+weather+humidity+month+temp+year+atemp, dwind_train, ntree=250, importance=T)
# ywind <- predict(myrf_wind, dwind_test)
# write.csv(ywind, './new_data/windspeed.csv',row.names = F)
d$windspeed[zeri]<-read.csv("./new_data/windspeed.csv")$x


# METTO REGISTERED, CASUAL, COUNT IN FONDO
z<-d$registered; d$registered<-NULL; d$registered<-z; rm(z)
z<-d$casual; d$casual<-NULL; d$casual<-z; rm(z)
z<-d$count; d$count<-NULL; d$count<-z; rm(z)

# ESPORTO
d1<-d[train,]; write.csv(d1, './new_data/d1.csv', row.names = F)
d2<-d[test,]; write.csv(d2, './new_data/d2.csv', row.names = F)

