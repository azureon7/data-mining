#---------------
#  FIT METHOD
#---------------

knnb.fit = function(X_train, y_train, theta, B, small_class=NA, reins=T){
  library(caret)
  
  if (is.na(small_class)){
    C1 = levels(y_train)[which.min(c(length(y_train[y_train==levels(y_train)[1]]),
                                     length(y_train[y_train==levels(y_train)[2]])))]
    Class_1 = which(y_train == C1)
    Class_0 = which(y_train != C1)  
  }  else {C1 = small_class}
  
  C0 <- levels(y_train)[levels(y_train)!=C1]
  Class_1 = which(y_train == C1)
  Class_0 = which(y_train == C0) 
  
  size = as.integer(length(Class_1)*theta)
  p = ncol(X_train)
  
  models <- list()
  cols <- list()
  
  for (b in 1:B){
    set.seed(b)
    
    # estrazione righe
    i_1 = sample(Class_1, size+10, replace = reins)
    i_0 = sample(Class_0, size-10, replace = reins)
    i = sort(c(i_0,i_1))
    
    # campione bootstrap
    help_df = data.frame(X_train[i, ], CLASS = y_train[i])
    
    # modello knn
    grid <- expand.grid(k = seq(120,180, by = 1))
    rcv <- trainControl(method = "repeatedcv", number = 10, repeats = 2, classProbs = T, summaryFunction = twoClassSummary)
    models[[b]] <- train(CLASS ~ ., help_df, method = "knn", trControl=rcv, tuneGrid = grid, metric = "ROC")
    
  }
  output <- list(Models=models, Class1=C1, Class0=C0)
  return(output)
}

#-----------------
# PREDICT METHOD
#-----------------

knnb.predict <- function(knnb_object, newdata, method){

  library(matrixStats)
  B = length(knnb_object$Models)

  # METHOD = MEAN
  if (method=='mean'){
    matriX=c()
    for (b in 1:B){
      fit = knnb_object$Models[[b]]
      p = predict(fit, newdata=newdata, type='prob')[,knnb_object$Class1]
      matriX = cbind(matriX,p)}
    fitted = rowMeans(matriX)
    return(fitted)
  }

  # METHOD = MEDIAN
  else if (method=='median'){
    matriX=c()
    for (b in 1:B){
      fit = knnb_object$Models[[b]]
      p = predict(fit, newdata=newdata,  type='prob')[,knnb_object$Class1]
      matriX = cbind(matriX,p)}
    fitted = rowMedians(matriX)
    return(fitted)
  }
}
