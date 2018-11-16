#=================================
#    HOUSE PRICES COMPETITION
#=================================

# (!) set working directory to source file location (!)
rm(list=ls())

library(rpart); library(rattle)
library(rpart.plot); library(RColorBrewer)

# ------------
# read dataset
# ------------

d1 <- read.csv("./raw_data/train.csv", stringsAsFactors = F)
d2 <- read.csv("./raw_data/test.csv", stringsAsFactors = F)

d2$SalePrice <- -1
d <- rbind(d1,d2); rm(d1,d2)
d$Order <- NULL

# image(t(is.na(d)))

# for (j in colnames(d)){
#   if (sum(is.na(d[,j]))>0) print(j)
# }

# VARIABILI CON NA

# [1] "Lot.Frontage"      num
# [1] "Alley"             OK
# [1] "Mas.Vnr.Area"      OK
# [1] "Bsmt.Qual"         OK       
# [1] "Bsmt.Cond"         OK  
# [1] "Bsmt.Exposure"     OK
# [1] "BsmtFin.Type.1"    OK
# [1] "BsmtFin.SF.1"      OK
# [1] "BsmtFin.Type.2"    OK
# [1] "BsmtFin.SF.2"      OK
# [1] "Bsmt.Unf.SF"       OK
# [1] "Total.Bsmt.SF"     OK
# [1] "Bsmt.Full.Bath"    OK
# [1] "Bsmt.Half.Bath"    OK
# [1] "Fireplace.Qu"      OK
# [1] "Garage.Type"       OK
# [1] "Garage.Yr.Blt"     OK
# [1] "Garage.Finish"     OK
# [1] "Garage.Cars"       OK  
# [1] "Garage.Area"       OK     
# [1] "Garage.Qual"       OK
# [1] "Garage.Cond"       OK
# [1] "Pool.QC"           OK
# [1] "Fence"             OK
# [1] "Misc.Feature"      removed

d2 <- data.frame(PID = d$PID)

# previsioni ottenute con random forest (casi con solo 1 missing)
d2$BsmtFin.SF.1 <- d$BsmtFin.SF.1
d2$BsmtFin.SF.2 <- d$BsmtFin.SF.2
d2$Bsmt.Unf.SF <- d$Bsmt.Unf.SF
d2$Total.Bsmt.SF <- d$Total.Bsmt.SF
d2$BsmtFin.SF.1[which(is.na(d2$BsmtFin.SF.1))] <- 255
d2$BsmtFin.SF.2[which(is.na(d2$BsmtFin.SF.2))] <- 84
d2$Bsmt.Unf.SF[which(is.na(d2$Bsmt.Unf.SF))] <- 437
d2$Total.Bsmt.SF[which(is.na(d2$Total.Bsmt.SF))] <- 718

# moda (casi con pochissimi missing)
d2$Mas.Vnr.Area <- d$Mas.Vnr.Area
d2$Bsmt.Full.Bath <- d$Bsmt.Full.Bath
d2$Bsmt.Half.Bath <- d$Bsmt.Half.Bath
d2$Mas.Vnr.Area[is.na(d2$Mas.Vnr.Area)]<-0
d2$Bsmt.Full.Bath[is.na(d2$Bsmt.Full.Bath)] <- 0 
d2$Bsmt.Half.Bath[is.na(d2$Bsmt.Half.Bath)] <- 0 

# Bsmt.Qual
d2$Bsmt.Qual <- d$Bsmt.Qual
d2$Bsmt.Qual[is.na(d2$Bsmt.Qual)|d2$Bsmt.Qual==''] = '0'
d2$Bsmt.Qual[d2$Bsmt.Qual=='Po'] = '1'
d2$Bsmt.Qual[d2$Bsmt.Qual=='Fa'] = '1'
d2$Bsmt.Qual[d2$Bsmt.Qual=='TA'] = '2'
d2$Bsmt.Qual[d2$Bsmt.Qual=='Gd'] = '3'
d2$Bsmt.Qual[d2$Bsmt.Qual=='Ex'] = '4'
d2$Bsmt.Qual <- as.integer(d2$Bsmt.Qual)

# Bsmt.Cond
d2$Bsmt.Cond <- d$Bsmt.Cond
d2$Bsmt.Cond[is.na(d2$Bsmt.Cond)|d2$Bsmt.Cond==''] = '0'
d2$Bsmt.Cond[d2$Bsmt.Cond == 'Po'] = '1'
d2$Bsmt.Cond[d2$Bsmt.Cond == 'Fa'] = '1'
d2$Bsmt.Cond[d2$Bsmt.Cond == 'TA'] = '2'
d2$Bsmt.Cond[d2$Bsmt.Cond == 'Gd'|d2$Bsmt.Cond == 'Ex'] = '3'
d2$Bsmt.Cond <- as.numeric(d2$Bsmt.Cond)

# Bsmt.Exposure
d2$Bsmt.Exposure <- d$Bsmt.Exposure
d2$Bsmt.Exposure[is.na(d2$Bsmt.Exposure)|d2$Bsmt.Exposure==''] = '0'
d2$Bsmt.Exposure[d2$Bsmt.Exposure == 'No'] = '0'
d2$Bsmt.Exposure[d2$Bsmt.Exposure == 'Mn'] = '1'
d2$Bsmt.Exposure[d2$Bsmt.Exposure == 'Av'] = '2'
d2$Bsmt.Exposure[d2$Bsmt.Exposure == 'Gd'] = '3'
d2$Bsmt.Exposure <- as.numeric(d2$Bsmt.Exposure)

# BsmtFin.Type.1
d2$BsmtFin.Type.1 <- d$BsmtFin.Type.1
d2$BsmtFin.Type.1[is.na(d2$BsmtFin.Type.1)|d2$BsmtFin.Type.1==''] <- '0'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'Unf'] <- '1'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'LwQ'] <- '2'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'Rec'] <- '3'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'BLQ'] <- '4'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'ALQ'] <- '5'
d2$BsmtFin.Type.1[d2$BsmtFin.Type.1 == 'GLQ'] <- '6'
d2$BsmtFin.Type.1 <- as.numeric(d2$BsmtFin.Type.1)

# BsmtFin.Type.2
d2$BsmtFin.Type.2 <- d$BsmtFin.Type.2
d2$BsmtFin.Type.2[is.na(d2$BsmtFin.Type.2)|d2$BsmtFin.Type.2==''] <- '0'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'Unf'] <- '1'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'LwQ'] <- '2'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'Rec'] <- '3'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'BLQ'] <- '4'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'ALQ'] <- '5'
d2$BsmtFin.Type.2[d2$BsmtFin.Type.2 == 'GLQ'] <- '6'
d2$BsmtFin.Type.2 <- as.numeric(d2$BsmtFin.Type.2)

# Alley
d2$NOM.Alley <- d$Alley
d2$NOM.Alley[is.na(d2$NOM.Alley)] <- 'None'

# Fireplace.Qu
d2$Fireplace.Qu <- d$Fireplace.Qu
d2$Fireplace.Qu[is.na(d2$Fireplace.Qu)|d2$Fireplace.Qu==''] <- '0'
d2$Fireplace.Qu[d2$Fireplace.Qu=='Po'] <- '1'
d2$Fireplace.Qu[d2$Fireplace.Qu=='Fa'] <- '2'
d2$Fireplace.Qu[d2$Fireplace.Qu=='TA'] <- '3'
d2$Fireplace.Qu[d2$Fireplace.Qu=='Gd'] <- '4'
d2$Fireplace.Qu[d2$Fireplace.Qu=='Ex'] <- '5'
d2$Fireplace.Qu <- as.numeric(d2$Fireplace.Qu)

# Garage.Type
d2$NOM.Garage.Type <- d$Garage.Type
d2$NOM.Garage.Type[is.na(d2$NOM.Garage.Type)] <- 'None'

# Garage.Yr.Blt
x <- d[d$SalePrice!=-1,]
fit.tree=rpart(SalePrice~Garage.Yr.Blt,data=x)
# fancyRpartPlot(fit.tree)
d2$NOM.Garage.Year <- 'None'
d2$NOM.Garage.Year[d$Garage.Yr.Blt<1966] = 'Bef.1966'
d2$NOM.Garage.Year[d$Garage.Yr.Blt>=1966&d$Garage.Yr.Blt<1992] = 'Bet.1966.1992'
d2$NOM.Garage.Year[d$Garage.Yr.Blt>=1992&d$Garage.Yr.Blt<2005] = 'Bet.1992.2005'
d2$NOM.Garage.Year[d$Garage.Yr.Blt>=2005] = 'Aft.2005'

# Garage.Finish
d2$Garage.Finish <- d$Garage.Finish
d2$Garage.Finish[is.na(d2$Garage.Finish)|d2$Garage.Finish==''] <- '0'
d2$Garage.Finish[d2$Garage.Finish=='Unf'] <- '1'
d2$Garage.Finish[d2$Garage.Finish=='RFn'] <- '2'
d2$Garage.Finish[d2$Garage.Finish=='Fin'] <- '4'
d2$Garage.Finish <- as.numeric(d2$Garage.Finish)

# Garage.Cars
d2$Garage.Cars <- d$Garage.Cars
d2$Garage.Cars[is.na(d2$Garage.Cars)] <- 0

# Garage.Area
d2$Garage.Area <- d$Garage.Area
d2$Garage.Area[which(is.na(d2$Garage.Area))]=419

# Garage.Qual
d2$Garage.Qual <- d$Garage.Qual
d2$Garage.Qual[is.na(d2$Garage.Qual)|d2$Garage.Qual==''] <- '0'
d2$Garage.Qual[d2$Garage.Qual=='Po'|d2$Garage.Qual=='Fa'] <- '1'
d2$Garage.Qual[d2$Garage.Qual=='TA'] <- '2'
d2$Garage.Qual[d2$Garage.Qual=='Gd'|d2$Garage.Qual=='Ex'] <- '3'
d2$Garage.Qual <- as.numeric(d2$Garage.Qual)

# Garage.Cond
d2$Garage.Cond <- d$Garage.Cond
d2$Garage.Cond[is.na(d2$Garage.Cond)|d2$Garage.Cond==''] <- '0'
d2$Garage.Cond[d2$Garage.Cond=='Po'|d2$Garage.Cond=='Fa'] <- '1'
d2$Garage.Cond[d2$Garage.Cond=='TA'] <- '2'
d2$Garage.Cond[d2$Garage.Cond=='Gd'|d2$Garage.Cond=='Ex'] <- '3'
d2$Garage.Cond <- as.numeric(d2$Garage.Cond)

# Pool.QC
d2$NOM.Pool <- 'no'
d2$NOM.Pool[is.na(d$Pool.QC)] <- 'yes'

# Fence
d2$Fence <- '0'
d2$Fence[d$Fence=='MnWw'|d$Fence=='GdWo'] = '1'
d2$Fence[d$Fence=='MnPrv'] = '2'
d2$Fence[d$Fence=='GdPrv'] = '3'
d2$Fence <- as.numeric(d2$Fence)

# Lot.Frontage
sum(is.na(d$Lot.Frontage))
b0 = summary(lm(Lot.Frontage~Lot.Area, data=d))$coef[1]
b1 = summary(lm(Lot.Frontage~Lot.Area, data=d))$coef[2]
d2$Lot.Frontage = d$Lot.Frontage
d2$Lot.Area = d$Lot.Area
m <- is.na(d2$Lot.Frontage)
d2$Lot.Frontage[m] <- b0+b1*d2$Lot.Area[m]

d$Misc.Feature <- NA

# setdiff(colnames(d), colnames(d2))

# VARIABILI SENZA MISSING MA DA TRATTARE

# [1] "MS.SubClass"    OK   
# [2] "MS.Zoning"      OK  
# [3] "Street"         OK
# [4] "Lot.Shape"      OK
# [5] "Land.Contour"   OK
# [6] "Utilities"      removed
# [7] "Lot.Config"     OK
# [8] "Land.Slope"     OK
# [9] "Neighborhood"   OK
# [10] "Condition.1"   OK  
# [11] "Condition.2"   OK  
# [12] "Bldg.Type"     OK
# [13] "House.Style"   OK   
# [14] "Overall.Qual"  OK  
# [15] "Overall.Cond"  OK
# [16] "Year.Built"    OK   
# [17] "Year.Remod.Add" OK
# [18] "Roof.Style"     OK
# [19] "Roof.Matl"      OK
# [20] "Exterior.1st"   OK
# [21] "Exterior.2nd"   OK
# [22] "Mas.Vnr.Type"   OK
# [23] "Exter.Qual"     OK   
# [24] "Exter.Cond"     OK
# [25] "Foundation"     OK    
# [26] "Heating"        OK  
# [27] "Heating.QC"     OK
# [28] "Central.Air"    OK
# [29] "Electrical"     OK 
# [30] "X1st.Flr.SF"    OK
# [31] "X2nd.Flr.SF"    OK
# [32] "Low.Qual.Fin.SF" OK
# [33] "Gr.Liv.Area"     OK
# [34] "Full.Bath"       OK
# [35] "Half.Bath"       OK
# [36] "Bedroom.AbvGr"   OK
# [37] "Kitchen.AbvGr"   OK
# [38] "Kitchen.Qual"    OK
# [39] "TotRms.AbvGrd"   OK
# [40] "Functional"      OK
# [41] "Fireplaces"      OK
# [43] "Paved.Drive"     OK
# [44] "Wood.Deck.SF"    OK
# [45] "Open.Porch.SF"   OK
# [46] "Enclosed.Porch"  OK
# [47] "X3Ssn.Porch"     OK
# [48] "Screen.Porch"    OK
# [49] "Pool.Area"       removed
# [50] "Pool.QC"         removed
# [51] "Misc.Feature"    removed
# [52] "Misc.Val"        removed
# [53] "Mo.Sold"         OK
# [54] "Yr.Sold"         OK
# [55] "Sale.Type"       OK
# [56] "Sale.Condition"  OK
# [57] "SalePrice"       OK

d2$NOM.MS.SubClass <- d$MS.SubClass
d2$NOM.MS.Zoning <- d$MS.Zoning
d2$NOM.Street <- d$Street
d2$NOM.Lot.Shape <- d$Lot.Shape
d2$NOM.Land.Contour <- d$Land.Contour
d$Utilities <- NULL
d2$NOM.Lot.Config <- d$Lot.Config
d2$NOM.Land.Slope <- 'Gtl'
d2$NOM.Land.Slope[d$Land.Slope!='Gtl'] <- 'Mod'
d2$NOM.Neighborhood <- d$Neighborhood
d2$NOM.Condition.1 <- 'Norm'
d2$NOM.Condition.1[d$Condition.1!='Norm'] <- 'NotNorm'
d2$NOM.Bldg.Type <- d$Bldg.Type
d2$NOM.House.Style <- d$House.Style
d2$Overall.Qual <- d$Overall.Qual
d2$Overall.Cond <- d$Overall.Cond

x <- d[d$SalePrice!=-1,]
fit.tree=rpart(SalePrice~Year.Built,data=x)
# fancyRpartPlot(fit.tree)
d2$NOM.Year.Built <- 'None'
d2$NOM.Year.Built[d$Year.Built<1959] = 'Bef.1959'
d2$NOM.Year.Built[d$Year.Built>=1959&d$Year.Built<1987] = 'Bet.1959.1987'
d2$NOM.Year.Built[d$Year.Built>=1987&d$Year.Built<2005] = 'Bet.1987.2005'
d2$NOM.Year.Built[d$Year.Built>=2005] = 'Aft.2005'


x <- d[d$SalePrice!=-1,]
fit.tree=rpart(SalePrice~Year.Remod.Add,data=x)
d2$NOM.Year.Remod.Add <- 'None'
d2$NOM.Year.Remod.Add[d$Year.Remod.Add<1956] = 'Bef.1956'
d2$NOM.Year.Remod.Add[d$Year.Remod.Add>=1956&d$Year.Remod.Add<1986] = 'Bet.1956.1986'
d2$NOM.Year.Remod.Add[d$Year.Remod.Add>=1986&d$Year.Remod.Add<2006] = 'Bet.1986.2006'
d2$NOM.Year.Remod.Add[d$Year.Remod.Add>=2006] = 'Aft.2006'

d2$NOM.Roof.Style <- d$Roof.Style
d2$NOM.Roof.Matl <- d$Roof.Matl

d2$NOM.Exterior.1st <- d$Exterior.1st
d2$NOM.Exterior.2nd <- d$Exterior.2nd

d2$NOM.Mas.Vnr.Type <- d$Mas.Vnr.Type

d2$Exter.Qual <- 0
d2$Exter.Qual[d$Exter.Qual=='Fa'] <- 1
d2$Exter.Qual[d$Exter.Qual=='TA'] <- 2
d2$Exter.Qual[d$Exter.Qual=='Gd'] <- 3
d2$Exter.Qual[d$Exter.Qual=='Ex'] <- 4

d2$Exter.Cond <- 0
d2$Exter.Cond[d$Exter.Cond=='Fa'] <- 1
d2$Exter.Cond[d$Exter.Cond=='TA'] <- 2
d2$Exter.Cond[d$Exter.Cond=='Gd'] <- 3
d2$Exter.Cond[d$Exter.Cond=='Ex'] <- 4

d2$NOM.Foundation <- d$Foundation

d2$NOM.Heating <- d$Heating

d2$Heating.QC <- 0
d2$Heating.QC[d$Heating.QC=='Fa'] <- 1
d2$Heating.QC[d$Heating.QC=='TA'] <- 2
d2$Heating.QC[d$Heating.QC=='Gd'] <- 3
d2$Heating.QC[d$Heating.QC=='Ex'] <- 4

d2$NOM.Central.Air <- d$Central.Air

d2$NOM.Electrical <- 'Standard'
d2$NOM.Electrical[d$Electrical!='SBrkr'] <- 'NotStandard'

d2$X1st.Flr.SF <- d$X1st.Flr.SF
d2$X2nd.Flr.SF <- d$X2nd.Flr.SF
d2$Low.Qual.Fin.SF <- d$Low.Qual.Fin.SF
d2$Gr.Liv.Area <- d$Gr.Liv.Area
d2$Full.Bath <- d$Full.Bath
d2$Half.Bath <- d$Half.Bath
d2$Bedroom.AbvGr <- d$Bedroom.AbvGr
d2$Kitchen.AbvGr <- d$Kitchen.AbvGr

d2$Kitchen.Qual <- 0
d2$Kitchen.Qual[d$Kitchen.Qual=='Fa'] <- 1
d2$Kitchen.Qual[d$Kitchen.Qual=='TA'] <- 2
d2$Kitchen.Qual[d$Kitchen.Qual=='Gd'] <- 3
d2$Kitchen.Qual[d$Kitchen.Qual=='Ex'] <- 4

d2$TotRms.AbvGrd <- d$TotRms.AbvGrd

d2$NOM.Functional <- 'Typ'
d2$NOM.Functional[d$Functional!='Typ'] <- 'NotTyp'

d2$Fireplaces <- d$Fireplaces

d2$NOM.Paved.Drive <- 'Paved'
d2$NOM.Paved.Drive[d$Paved.Drive!='Y'] <- 'NotPaved'

d2$Wood.Deck.SF <- d$Wood.Deck.SF
d2$Open.Porch.SF <- d$Open.Porch.SF
d2$Enclosed.Porch <- d$Enclosed.Porch
d2$X3Ssn.Porch <- d$X3Ssn.Porch
d2$Screen.Porch <- d$Screen.Porch

d$Pool.Area <- NULL
d$Pool.QC <- NULL
d$Misc.Feature <- NULL
d$Misc.Val <- NULL

d2$Mo.Sold <- d$Mo.Sold
d2$Yr.Sold <- d$Yr.Sold

d2$NOM.Sale.Type <- d$Sale.Type
d2$NOM.Sale.Condition <- d$Sale.Condition

d2$SalePrice <- d$SalePrice

d2$PID <- NULL

train <- d2[d2$SalePrice!=-1,]
test <- d2[d2$SalePrice==-1,]

write.csv(train, './new_data/train.csv', row.names = F)
write.csv(test, './new_data/test.csv', row.names = F)