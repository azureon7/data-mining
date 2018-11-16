#=============================
# 3. EVALUATION 
#=============================

rm(list=ls())

# READ FUNCTIONS
source("source/OkCupid_Functions.R")
source("source/RandomForestBalanced.R")

# READ CSV
train <- read.csv('./new_datasets/data.csv')
test <- read.csv('./new_datasets/data_test.csv')

# sottomissione 76% beeviva
attrs <- c("male", "essay_link", "tech", "computer", "science", "fixing", 
           "matrix", "electronic", "nerdy", "artist", "SV", "body_type_A", "diet_A", "single",
           "student", "teaching", "loyal", "atheist", "smoke", "sign_imp", 
           "education_A", "income100000", "phdYN", "Class") 

fixed_attrs = c("UnionDummy", "educ_dummy1", "male", "phdYN", "teaching")

train = train[,attrs]; test = test[,attrs]

# PREPARO TRAIN E TEST
X_test <- test; X_test$Class=NULL
X_train <- train; X_train$Class=NULL
y_train <- train$Class

#-------------------------------------
# 3.A RANDOM FOREST MANUALE BILANCIATO
# proportion method
pred.tot <- c()
B <- 5000
h.mat <- c()
for(g in 1:100){
  set.seed(g)
  h <- sample(1:100000, B,  replace=F)
  h.mat <- cbind(h.mat,h)
}
for(i in 1:100){
  fit.tot <- rfb.fit(X_train,y_train,theta=1,alpha=0.15,B=B,minsplit=10, minbucket=10, seme=h.mat[, i])
  pred.tot <- cbind(pred.tot, rfb.predict(fit.tot, newdata = X_test, method="median"))
}
y_pred18 <- apply(X = pred.tot, MARGIN = 1 , FUN = mean, trim = .2)

# SUBMISSION
write.table(file="./predictions/myokcupid_18.txt", y_pred18, row.names = FALSE, col.names = FALSE)



#-------------------
# 3.B RANDOM FOREST CON CARET E DOWN
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
model_rf_rose <- caret::train(Class ~ .,
                              data = train,
                              method = "rf",
                              trControl = ctrl)
y_pred19 <- predict(model_rf_rose, newdata = test, type = "prob")[,"stem"]

write.table(file="./predictions/myokcupid_19.txt", y_pred19, row.names = FALSE, col.names = FALSE)




