
# 3. Modeling

``` r
rm(list=ls())
setwd("C:/Users/letym/Desktop/Data_mining/Competition3/BikeSharing Competition")
d1<-read.csv("./new_data/d1.csv", stringsAsFactors = F)
d2<-read.csv("./new_data/d2.csv", stringsAsFactors = F)

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
```

Dati i risultati della **validation** decidiamo di provare a costruire sul test reale i modelli 4, 6, 8 e 9.

MODELLO 4
---------

``` r
# MODELLO PER REGISTERED
formula.reg4 <- registered ~ hour+month+wday+workingday+holiday+day_type+temp+temp.reg+humidity+humidity.reg+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg4 <- c()
set.seed(1)
n <- 100
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg4, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg4 <- cbind(pred.reg4,expm1(predict(fit.reg, dtest)))
}
formula.cas4 <- casual ~ hour+wday+month+workingday+holiday+day_type+temp+temp.cas+humidity+humidity.cas+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas4 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas4, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.cas)
  pred.cas4 <- cbind(pred.cas4,expm1(predict(fit.cas, dtest)))
}

pred.cnt4_matrix <- as.matrix(pred.cas4) + as.matrix(pred.reg4)
pred.cnt4 <- apply(X = pred.cnt4_matrix, MARGIN = 1 , FUN = mean, trim = .2)
write.table(pred.cnt4, file = "pred4_100semi.txt", row.names = F, col.names = F)
```

MODELLO 6
---------

``` r
# MODELLO PER REGISTERED
formula.reg6 <- registered ~ hour+wday+workingday+day_type+temp+temp.reg+humidity+humidity.reg+atemp+windspeed+season+weather+hour.cut.reg+mean.reg.weekend.hour+year
pred.reg6 <- c()
set.seed(1)
n <- 100
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg6, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg6 <- cbind(pred.reg6,expm1(predict(fit.reg, dtest)))
}
formula.cas6 <- casual ~ hour+wday+workingday+day_type+temp+temp.cas+humidity+humidity.cas+atemp+windspeed+season+weather+hour.cut.cas+mean.cas.weekend.hour+year
pred.cas6 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas6, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.cas)
  pred.cas6 <- cbind(pred.cas6,expm1(predict(fit.cas, dtest)))
}

pred.cnt6_matrix <- as.matrix(pred.cas6) + as.matrix(pred.reg6)
pred.cnt6 <- apply(X = pred.cnt6_matrix, MARGIN = 1 , FUN = mean, trim = .2)
write.table(pred.cnt6, file = "pred6_100semi.txt", row.names = F, col.names = F)
```

MODELLO 8
---------

``` r
# MODELLO PER REGISTERED 8

formula.reg8 <- registered ~ hour+wday+workingday+holiday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg8 <- c()
set.seed(1)
n <- 100
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg8, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg8 <- cbind(pred.reg8,expm1(predict(fit.reg, dtest)))
}
formula.cas8 <- casual~ hour+wday+workingday+holiday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas8 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas8, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.cas)
  pred.cas8 <- cbind(pred.cas8,expm1(predict(fit.cas, dtest)))
}

pred.cnt8_matrix <- as.matrix(pred.cas8) + as.matrix(pred.reg8)
pred.cnt8 <- apply(X = pred.cnt8_matrix, MARGIN = 1 , FUN = mean, trim = .2)
write.table(pred.cnt8, file = "pred8_100semi.txt", row.names = F, col.names = F)
```

MODELLO 9
---------

``` r
# MODELLO PER REGISTERED
formula.reg9 <- registered ~ hour+wday+workingday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+hour.cut.reg+mean.reg.weekend.hour+year
pred.reg9 <- c()
set.seed(1)
n <- 100
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg9, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg9 <- cbind(pred.reg9,expm1(predict(fit.reg, dtest)))
}

# MODELLO PER CASUAL
formula.cas9 <- casual~ hour+wday+workingday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+hour.cut.cas+mean.cas.weekend.hour+year
pred.cas9 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas9, data=dtrain, ntree=100, importance=F)
  #varImpPlot(fit.cas)
  pred.cas9 <- cbind(pred.cas9,expm1(predict(fit.cas, dtest)))
}

pred.cnt9_matrix <- as.matrix(pred.cas9) + as.matrix(pred.reg9)
pred.cnt9 <- apply(X = pred.cnt9_matrix, MARGIN = 1 , FUN = mean, trim = .2)
write.table(pred.cnt9, file = "pred9_100semi.txt", row.names = F, col.names = F)
```
