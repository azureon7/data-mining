---
layout: default
---

[Back](./index.html)

# BikeSharing Competition

### Summary

Our strategy was

1. Predict separately the two variables "Registered" and "Casual", and then "Count" as their sum;
2. Transform the target variables to `log(x+1)`;
3. Fill the "Windspeed" variable missing values with a `randomForest` regression;
4. Discretize continuous attributes with simple `rpart` trees splits, choosing at first "Registered", then "Casual" as response;
5. Create new features, e.g. hourly average of "Registered" and "Casual";
6. Random Forest algorithm, using different sets of variables for "Registered" and "Casual".

### References

* "The caret package": [http://topepo.github.io/caret/index.html](http://topepo.github.io/caret/index.html)
* Feature engineering: [https://www.kaggle.com/gauravjindal2309/bike-sharing-demand/code](https://www.kaggle.com/gauravjindal2309/bike-sharing-demand/code)
* Visualizations: [https://www.kaggle.com/h19881812/data-vizualization](https://www.kaggle.com/h19881812/data-vizualization)

### Models

* Random Forest

### Non-standard R packages

* `randomForest`: preprocessing (missing values imputation) and modeling (final predictions);
* `rpart`: preprocessing (continue variables to discrete);
* `lubridate`: preprocessing (day of the week);
* `dplyr`: preprocessing (function mutate_if() to transform character variables to factor);

### R code to reproduce the last submission:

```r
library(lubridate)
library(randomForest)
library(rpart)
library(dplyr)

start.time <- Sys.time()

d1 = read.csv("http://bee-fore.s3-eu-west-1.amazonaws.com/datasets/99.csv", stringsAsFactors=F)
d2 = read.csv("http://bee-fore.s3-eu-west-1.amazonaws.com/datasets/100.csv", stringsAsFactors=F)
d2$casual<-d2$registered<-d2$count<-NA
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
d$humidity.reg=0
d$humidity.reg[d$humidity>=67]=1
d$humidity.reg[d$humidity>=44 & d$humidity<67]=2
d$humidity.reg[d$humidity<44]=3

fit.tree=rpart(casual~humidity,data=d[train,])
d$humidity.cas=0
d$humidity.cas[d$humidity>=75]=1
d$humidity.cas[d$humidity>=56 & d$humidity<75]=2
d$humidity.cas[d$humidity>=40 & d$humidity<56]=3
d$humidity.cas[d$humidity<40]=4

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
d$windspeed[zeri]<-read.csv("https://raw.githubusercontent.com/azureon7/data-mining/master/source/BikeSharing%20Competition/new_data/windspeed.csv")$x

#--------------------------------------------------------#
# METTO REGISTERED, CASUAL, COUNT IN FONDO
z<-d$registered; d$registered<-NULL; d$registered<-z; rm(z)
z<-d$casual; d$casual<-NULL; d$casual<-z; rm(z)
z<-d$count; d$count<-NULL; d$count<-z; rm(z)

# ESPORTO
d1<-d[train,] 
d2<-d[test,]

# write.csv(d1, './d1.csv', row.names = F)
# write.csv(d2, './d2.csv', row.names = F)

#---------------------
# Modello Random Forest
#---------------------

# d1<-read.csv("./d1.csv", stringsAsFactors = F)
# d2<-read.csv("./d2.csv", stringsAsFactors = F)

d1 <- mutate_if(d1, is.factor, as.character)
d2 <- mutate_if(d2, is.factor, as.character)
d<-rbind(d1,d2); rm(d1,d2)

train<-1:10886; test<-10887:17379

# TRASFORMO IN FACTOR
to.factor <- c()
for(col in colnames(d)) to.factor<-c(to.factor,class(d[,col]))
to.factor <- which(to.factor=='character')
d[,to.factor] <- lapply(d[,to.factor], as.factor); rm(to.factor, col)
d[,c('hour', 'day', 'month', 'year')] <- lapply(d[,c('hour', 'day', 'month', 'year')], as.factor)

# TRAIN E TEST
dtrain<-d[train,]
dtest<-d[test,]; dtest$count<-dtest$registered <- dtest$casual <- NULL

dtrain$registered <- log1p(dtrain$registered)
dtrain$casual <- log1p(dtrain$casual)

formula.reg8 <- registered ~ hour+wday+workingday+holiday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg8 <- c()
set.seed(1)
n <- 10
semi <- sample(1:1000, n, replace=F)
for(i in semi){
  set.seed(i)
  fit.reg <- randomForest(formula.reg8, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg8 <- cbind(pred.reg8,expm1(predict(fit.reg, dtest)))
}
formula.cas8 <- casual~ hour+wday+workingday+holiday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas8 <- c()
set.seed(2)
n <- 10
semi <- sample(1:1000, n, replace=F)
for(i in semi){
  set.seed(i)
  fit.cas <- randomForest(formula.cas8, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.cas)
  pred.cas8 <- cbind(pred.cas8,expm1(predict(fit.cas, dtest)))
}

# Results

pred.cnt8_matrix <- as.matrix(pred.cas8) + as.matrix(pred.reg8)
pred.cnt8 <- apply(X = pred.cnt8_matrix, MARGIN = 1 , FUN = mean, trim = .2)
# write.table(pred.cnt8, file = "./submission/BikeSharing_Submission.txt", row.names = F, col.names = F)

head(pred.cnt8)
```

```
    10887     10888     10889     10890     10891     10892 
10.634176  5.130941  2.959474  2.368902  2.113820  6.549004 
```

```r
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
```

```
Time difference of 17.09633 mins
```
