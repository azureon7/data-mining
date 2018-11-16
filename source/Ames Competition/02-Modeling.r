rm(list=ls())

library(glmnet); library(randomForest); library(caret); library(gbm)
library(Metrics)
source("./source/functions.R")

# read csv
d1 <- read.csv("./new_data/train.csv", stringsAsFactors = F)
d2 <- read.csv("./new_data/test.csv", stringsAsFactors = F)

d <- rbind(d1,d2)

# variabili factor nominali
toFactor <- grepl('NOM.',colnames(d1))
d[toFactor] <- lapply(d[toFactor], as.factor)
rm(d1,d2)

# trasformo variabili
f <- function(x) return(log10(x+1))
var = c("Gr.Liv.Area","Total.Bsmt.SF","X1st.Flr.SF","X2nd.Flr.SF")
d[,var] = lapply(d[,var], f)

# sostituisco livello mancante nel train
d$NOM.Neighborhood[d$NOM.Neighborhood=='Landmrk']='NAmes'

# train e test
dtrain <- d[d$SalePrice!=-1,]
dtest <- d[d$SalePrice==-1,]
dtest$SalePrice<-NULL

#==================
# rf
# variabili da rimuovere
via <- c("BsmtFin.SF.2","NOM.Pool","Fence","NOM.Street","NOM.Land.Contour","NOM.Land.Slope","NOM.Condition.1","NOM.Roof.Matl","Low.Qual.Fin.SF","Enclosed.Porch","X3Ssn.Porch","Mo.Sold","Yr.Sold")
set.seed(1)
fit.rf <- randomForest(log10(SalePrice)~., data=dtrain[,-get_positions(dtrain,via)], ntree=5000, importance=T)
ypred <- 10^predict(fit.rf, newdata=dtest[,-get_positions(dtrain,via)])

#==================
# ridge
lambda <- 10^seq(10, -2, length = 100)
x <- data.matrix(dtrain[,-ncol(dtrain)])
y <- log10(dtrain$SalePrice+1)
xtest <- data.matrix(dtest)

ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)

cv.out <- cv.glmnet(x, y, alpha = 0)
bestlam <- cv.out$lambda.min

ypred.ridge <- 10^predict(ridge.mod, s = bestlam, newx = xtest)-1

#==================
# lm
formula <- log10(SalePrice)~ Gr.Liv.Area+Total.Bsmt.SF+Kitchen.Qual+
  X2nd.Flr.SF+Overall.Qual+Overall.Cond+NOM.Condition.1+
  NOM.Sale.Condition+NOM.Neighborhood+NOM.MS.Zoning+Bsmt.Exposure+Bsmt.Full.Bath

fit1 <- lm(formula, data=dtrain[dtrain$Bsmt.Qual!=0,])
ypred.lm <- 10^predict(fit1, newdata=dtest[dtest$Bsmt.Qual!=0,])
# summary(fit1)
# media ponderata
yy <- (ypred*0.6+ypred.ridge*0.4)
# bsmt != 0
yy[dtest$Bsmt.Qual!=0] <- (ypred[dtest$Bsmt.Qual!=0]*0.4 + ypred.ridge[dtest$Bsmt.Qual!=0]*0.3 + ypred.lm*0.3)
#write.table(yy, './predictions/AmesSubmission.txt', row.names = F, col.names = F)

#==================
# gbm
set.seed(1)
fit.gbm <- gbm(log10(SalePrice)~., data=dtrain, distribution = "gaussian",
              n.trees = 10000, shrinkage = 0.001, interaction.depth = 6, n.minobsinnode = 20)
ypred.gbm <- 10^predict(fit.gbm, newdata = dtest, n.trees = 10000)
# media ponderata
yy2 <- (ypred.gbm*0.7+ypred.ridge*0.3)
# bsmt != 0
yy2[dtest$Bsmt.Qual!=0] <- (ypred.gbm[dtest$Bsmt.Qual!=0]*0.5 + ypred.ridge[dtest$Bsmt.Qual!=0]*0.2 + ypred.lm*0.3)

#==================
# sottomissione
write.table(yy2, './predictions/AmesSubmission_gbm_ridge_lm.txt', row.names = F, col.names = F)