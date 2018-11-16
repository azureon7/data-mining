library(Metrics); library(caret); library(Metrics); library(randomForest)

## Open new data

d1<-read.csv("./new_data/d1.csv", stringsAsFactors = F)
d2<-read.csv("./new_data/d2.csv", stringsAsFactors = F) 

# DATASET COMPLETO
d<-rbind(d1,d2);
train<-1:10886; test<-10887:17379

# TRASFORMO IN FACTOR
to.factor <- c()
for(col in colnames(d)) to.factor<-c(to.factor,class(d[,col]))
to.factor <- which(to.factor=='character')
d[,to.factor] <- lapply(d[,to.factor], as.factor); rm(to.factor, col)
d[,c('hour', 'day', 'month', 'year')] <- lapply(d[,c('hour', 'day', 'month', 'year')], as.factor)

# SUBTRAIN E SUBTEST PER LA VALIDATION
set.seed(1)
d_train <- d[train,]

p <- which(d_train[,"day"] == 15 | d_train[,"day"] == 16 | d_train[,"day"] == 17 | d_train[,"day"] == 18 | d_train[,"day"] == 19)
subtrain <- d_train[-p,]

subtest_registered <- d_train[p,24]
subtest_casual <- d_train[p,25]
subtest_count <- d_train[p,26]
subtest <- d_train[p,-c(24:26)]

subtrain$count <- log1p(subtrain$count)
subtrain$registered <- log1p(subtrain$registered)
subtrain$casual <- log1p(subtrain$casual)

#------------------------------------------------------------------------------------------------------#
## MODELLO PER COUNT 1

formula.tot1 <- count ~ hour+day+wday+month+workingday+holiday+day_type+temp+humidity+humidity.reg+humidity.cas+atemp+windspeed+season+weather+weekend+year+hour.cut.reg+hour.cut.cas+temp.reg+temp.cas+mean.cas.weekend.hour+mean.reg.weekend.hour
rmsle1 <- c()
set.seed(1)
for(i in sample(1:1000, 10, replace=F)){
  set.seed(i)
  fit.tot <- randomForest(formula.tot1, data=subtrain, ntree=50, importance=T)
  pred.tot <- expm1(predict(fit.tot, subtest))
  rmsle1 <- c(rmsle1, rmsle(subtest_count, pred.tot))
}
varImpPlot(fit.tot)
plot(rmsle1, main = "Plot rmsle modello su count")
abline(h = mean(rmsle1))
mean(rmsle1)

#------------------------------------------------------------------------------------------------------#
# MODELLO 2
# MODELLO PER REGISTERED 2

formula.reg2 <- registered ~ hour+day+month+wday+workingday+holiday+day_type+temp+temp.reg+humidity+humidity.reg+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg2 <- c()
set.seed(1)
n <- 10
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg2, data=subtrain, ntree=50, importance=T)
  pred.reg2 <- cbind(pred.reg2,expm1(predict(fit.reg, subtest)))
}

# MODELLO PER CASUAL 2

formula.cas2 <- casual ~ hour+day+wday+month+workingday+holiday+day_type+temp+temp.cas+humidity+humidity.cas+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas2 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
fit.cas <- randomForest(formula.cas2, data=subtrain, ntree=50, importance=T)
pred.cas2 <- cbind(pred.cas2,expm1(predict(fit.cas, subtest)))
}

# COUNT

rmsle_cr2 <- c()
for(i in 1:n) {
  rmsle_cr2 <- c(rmsle_cr2, rmsle(subtest_count,pred.cas2[,i]+pred.reg2[,i]))
}

plot(rmsle_cr2)
abline(h=mean(rmsle_cr2))
mean(rmsle_cr2)

#------------------------------------------------------------------------------------------------------#
## MODELLO PER COUNT 3

formula.tot3 <- count ~ hour+wday+month+workingday+holiday+day_type+temp+humidity+atemp+windspeed+season+weather+weekend+year
rmsle3 <- c()
set.seed(1)
for(i in sample(1:1000, 10, replace=F)){
  set.seed(i)
  fit.tot <- randomForest(formula.tot3, data=subtrain, ntree=50, importance=T)
  pred.tot <- expm1(predict(fit.tot, subtest))
  #varImpPlot(fit.tot)
  rmsle3 <- c(rmsle3, rmsle(subtest_count, pred.tot))
}

plot(rmsle3, main = "Plot rmsle modello su count")
abline(h = mean(rmsle3))
(mean(rmsle3))

#---------------------------------------------------------------------
# MODELLO 4
# MODELLO PER REGISTERED 4
formula.reg4 <- registered ~ hour+month+wday+workingday+holiday+day_type+temp+temp.reg+humidity+humidity.reg+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg4 <- c()
set.seed(1)
n <- 100
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg4, data=subtrain, ntree=100, importance=T)
  #varImpPlot(fit.reg)
  pred.reg4 <- cbind(pred.reg4,expm1(predict(fit.reg, subtest)))
}

# MODELLO PER CASUAL 4

formula.cas4 <- casual ~ hour+wday+month+workingday+holiday+day_type+temp+temp.cas+humidity+humidity.cas+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas4 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas4, data=subtrain, ntree=100, importance=T)
  #varImpPlot(fit.cas)
  pred.cas4 <- cbind(pred.cas4,expm1(predict(fit.cas, subtest)))
}

# COUNT
rmsle_cr4 <- c()
for(i in 1:n) {
  rmsle_cr4 <- c(rmsle_cr4, rmsle(subtest_count,pred.cas4[,i]+pred.reg4[,i]))
}

plot(rmsle_cr4)
abline(h = mean(rmsle_cr4))
mean(rmsle_cr4)

#---------------------------------------------------------------------
# MODELLO 5
# MODELLO PER COUNT 5

formula.tot5 <- count ~ hour+wday+workingday+holiday+day_type+humidity+atemp+windspeed+season+weather+weekend+year
rmsle5 <- c()
set.seed(1)
for(i in sample(1:1000, 10, replace=F)){
  set.seed(i)
  fit.tot <- randomForest(formula.tot5, data=subtrain, ntree=50, importance=T)
  pred.tot <- expm1(predict(fit.tot, subtest))
  #varImpPlot(fit.tot)
  rmsle5 <- c(rmsle5, rmsle(subtest_count, pred.tot))
}

plot(rmsle5, main = "Plot rmsle modello su count")
abline(h = mean(rmsle5))
(mean(rmsle5))

# MODELLO PER REGISTERED 6
n <- 100
formula.reg6 <- registered ~ hour+wday+workingday+day_type+temp+temp.reg+humidity+humidity.reg+atemp+windspeed+season+weather+hour.cut.reg+mean.reg.weekend.hour+year
pred.reg6 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg6, data=subtrain, ntree=100, importance=T)
  #varImpPlot(fit.reg)
  pred.reg6 <- cbind(pred.reg6,expm1(predict(fit.reg, subtest)))
}

# MODELLO PER CASUAL 6
formula.cas6 <- casual ~ hour+wday+workingday+day_type+temp+temp.cas+humidity+humidity.cas+atemp+windspeed+season+weather+hour.cut.cas+mean.cas.weekend.hour+year
pred.cas6 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas6, data=subtrain, ntree=100, importance=T)
  #varImpPlot(fit.cas)
  pred.cas6 <- cbind(pred.cas6,expm1(predict(fit.cas, subtest)))
}

# COUNT
rmsle_cr6 <- c()
for(i in 1:n) {
  rmsle_cr6 <- c(rmsle_cr6, rmsle(subtest_count,pred.cas6[,i]+pred.reg6[,i]))
}

plot(rmsle_cr6)
abline(h = mean(rmsle_cr6))
mean(rmsle_cr6)

# #---------------------------------------------------------------------------------------------------
## MODELLO PER COUNT 7
formula.tot7 <- count ~ hour+wday+workingday+day_type+humidity+atemp+windspeed+season+weather+year
rmsle7 <- c()
set.seed(1)
for(i in sample(1:1000, 10, replace=F)){
  set.seed(i)
  fit.tot <- randomForest(formula.tot7, data=subtrain, ntree=50, importance=T)
  pred.tot <- expm1(predict(fit.tot, subtest))
  #varImpPlot(fit.tot)
  rmsle7 <- c(rmsle7, rmsle(subtest_count, pred.tot))
}

plot(rmsle7, main = "Plot rmsle modello su count")
abline(h = mean(rmsle7))
(mean(rmsle7))


#---------------------------------------------------------------------------------------------------
# MODELLO PER REGISTERED 8
formula.reg8 <- registered ~ hour+wday+workingday+holiday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+hour.cut.reg+weekend+mean.reg.weekend.hour+year
pred.reg8 <- c()
set.seed(1)
n <- 400
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg8, data=subtrain, ntree=50, importance=F)
  #varImpPlot(fit.reg)
  pred.reg8 <- cbind(pred.reg8,expm1(predict(fit.reg, subtest)))
}


# MODELLO PER CASUAL 8
formula.cas8 <- casual~ hour+wday+workingday+holiday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+hour.cut.cas+weekend+mean.cas.weekend.hour+year
pred.cas8 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas8, data=subtrain, ntree=50, importance=F)
  #varImpPlot(fit.cas)
  pred.cas8 <- cbind(pred.cas8,expm1(predict(fit.cas, subtest)))
}

# COUNT
rmsle_cr8 <- c()
for(i in 1:n) {
  rmsle_cr8 <- c(rmsle_cr8, rmsle(subtest_count,pred.cas8[,i]+pred.reg8[,i]))
}

plot(rmsle_cr8)
abline(h = mean(rmsle_cr8))
mean(rmsle_cr8)

#---------------------------------------------------------------------------------------------------
## MODELLO 9
# MODELLO PER REGISTERED
formula.reg9 <- registered ~ hour+wday+workingday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+hour.cut.reg+mean.reg.weekend.hour+year
pred.reg9 <- c()
set.seed(1)
n <- 400
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg9, data=subtrain, ntree=100, importance=F)
  #varImpPlot(fit.reg)
  pred.reg9 <- cbind(pred.reg9,expm1(predict(fit.reg, subtest)))
}

# MODELLO PER CASUAL
formula.cas9 <- casual~ hour+wday+workingday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+hour.cut.cas+mean.cas.weekend.hour+year
pred.cas9 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas9, data=subtrain, ntree=50, importance=F)
  #varImpPlot(fit.cas)
  pred.cas9 <- cbind(pred.cas9,expm1(predict(fit.cas, subtest)))
}

# COUNT
rmsle_cr9 <- c()
for(i in 1:n) {
  rmsle_cr9 <- c(rmsle_cr9, rmsle(subtest_count,pred.cas9[,i]+pred.reg9[,i]))
}

plot(rmsle_cr9)
abline(h = mean(rmsle_cr9))
mean(rmsle_cr9)

#-----------------------------------------------------------------------------------------------#
## MODELLO 10
# MODELLO PER REGISTERED

formula.reg10 <- registered ~ hour+wday+workingday+day_type+temp.reg+humidity+atemp+windspeed+season+weather+mean.reg.weekend.hour+year
pred.reg10 <- c()
set.seed(1)
n <- 10
for(i in sample(1:1000, n, replace=F)){
  fit.reg <- randomForest(formula.reg10, data=subtrain, ntree=50, importance=T)
  varImpPlot(fit.reg)
  pred.reg10 <- cbind(pred.reg10,expm1(predict(fit.reg, subtest)))
}

# MODELLO PER CASUAL

formula.cas10 <- casual~ hour+wday+workingday+day_type+temp.cas+humidity+atemp+windspeed+season+weather+mean.cas.weekend.hour+year
pred.cas10 <- c()
set.seed(1)
for(i in sample(1:1000, n, replace=F)){
  fit.cas <- randomForest(formula.cas10, data=subtrain, ntree=50, importance=T)
  varImpPlot(fit.cas)
  pred.cas10 <- cbind(pred.cas10,expm1(predict(fit.cas, subtest)))
}

# COUNT
rmsle_cr10 <- c()
for(i in 1:n) {
  rmsle_cr10 <- c(rmsle_cr10, rmsle(subtest_count,pred.cas[,i]+pred.reg[,i]))
}

plot(rmsle_cr10)
abline(h = mean(rmsle_cr10))
mean(rmsle_cr10)



