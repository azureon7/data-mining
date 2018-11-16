#---------------
#  FIT METHOD
#---------------

rfb.fit = function(X_train, y_train, fixed_attrs=0, theta=1, B=1000, alpha=1/3, max_features=0, small_class=NA, reins=T,maxdepth=30,minsplit=20,minbucket=7){
  library(rpart)
  library(tree)
  
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
  
  trees <- list()
  cols <- list()
  
  for (b in 1:B){
    set.seed(b)
    
    # estrazione righe
    i_1 = sample(Class_1, size+10, replace = reins)
    i_0 = sample(Class_0, size-10, replace = reins)
    i = sort(c(i_0,i_1))
    
    # estrazione colonne
    if (max_features<=1) {j = sort(sample(1:p, size = as.integer(p*alpha), replace = FALSE))
    } else {j = sort(sample(1:p, size = max_features, replace = FALSE))}
    
    if(length(fixed_attrs)>1){
      fixed = get_positions(X_train,fixed_attrs)
      j = union(j,fixed)
    }
    cols[[b]] <- colnames(X_train)[j]
    
    # campione bootstrap
    help_df = data.frame(X_train[i,j], CLASS = y_train[i])
    
    # modello albero (rpart)    

    ctrl = rpart.control(maxdepth=maxdepth,minbucket=minbucket, minsplit=minsplit,xval=1)
    trees[[b]] <- rpart(CLASS~.,data=help_df,control=ctrl)
    
  }
  output <- list(Trees=trees, Cols=cols, Class1=C1, Class0=C0)
  return(output)
}

#-----------------
# PREDICT METHOD
#-----------------

rfb.predict <- function(rfb_object, newdata, method='class'){
  
  library(matrixStats)
  B = length(rfb_object$Trees)
  
  # METHOD = CLASS
  if (method=='class'){
    matriX=c()
    for (b in 1:B){
      j = rfb_object$Cols[[b]]
      fit = rfb_object$Trees[[b]]
      p = as.character(predict(fit, newdata=newdata[,j], type='class'))
      matriX = cbind(matriX,p)}
    fitted = c()
    for (k in 1:nrow(matriX)){
      proP = sum(matriX[k,] == rfb_object$Class1)/ncol(matriX)
      if(proP>0.5) {fitted[k] = rfb_object$Class1}
      else if(proP<0.5) {fitted[k]=rfb_object$Class0}
      else if(proP==0.5) {fitted[k]=sample(c(rfb_object$Class0,rfb_object$Class1), size=1)}}
    return(as.factor(fitted))
  }
  
  # METHOD = PROPORTION
  else if (method=='proportion'){
    matriX=c()
    for (b in 1:B){
      j = rfb_object$Cols[[b]]
      fit = rfb_object$Trees[[b]]
      p = as.character(predict(fit, newdata=newdata[,j], type='class'))
      matriX = cbind(matriX,p)}
    fitted = c()
    for (k in 1:nrow(matriX)){
      fitted[k] = sum(matriX[k,] == rfb_object$Class1)/ncol(matriX)}
    return(fitted)
  }
  
  # METHOD = MEAN
  else if (method=='mean'){
    matriX=c()
    for (b in 1:B){
      j = rfb_object$Cols[[b]]
      fit = rfb_object$Trees[[b]]
      p = predict(fit, newdata=newdata[,j])[,rfb_object$Class1]
      matriX = cbind(matriX,p)}
    fitted = rowMeans(matriX)
    return(fitted)
  }
  
  # METHOD = MEDIAN
  else if (method=='median'){
    matriX=c()
    for (b in 1:B){
      j = rfb_object$Cols[[b]]
      fit = rfb_object$Trees[[b]]
      p = predict(fit, newdata=newdata[,j])[,rfb_object$Class1]
      matriX = cbind(matriX,p)}
    fitted = rowMedians(matriX)
    return(fitted)
  }
}
  

#-----------------
# KFOLDS CV METHOD
#-----------------
  
rfb.alpha.cv <- function(X_train,y_train,K=10,range.alpha,B=200){
  AUC.estimate = c()
  #----
  for (u in 1:length(range.alpha)){
    alpha = range.alpha[u]
    N <- nrow(X_train)
    righe_da_testare <- 1:N
    n <- as.integer(N/K)
    AUC=c()
    #----
    for (k in 1:K){
      set.seed(k)
      i = sample(righe_da_testare, size = n, replace = F)
      righe_da_testare = setdiff(righe_da_testare, i)
      #----
      x_subtest <- X_train[i,]; y_subtest <- y_train[i]
      x_subtrain <- X_train[-i,]; y_subtrain <- y_train[-i]
      #----
      fit <- rfb.fit(x_subtrain,y_subtrain, theta=.9, alpha=alpha, B=B)
      y_pred <- rfb.predict(fit, newdata=x_subtest, method="class")
      AUC[k]=Evaluate(y_subtest,y_pred,echo=F)$auc
    }
    AUC.estimate[u] = mean(AUC)
    print(u)
  }
  return(AUC.estimate)
}
