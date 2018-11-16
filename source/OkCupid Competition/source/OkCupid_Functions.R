#====================================================================================
# EVALUATE FUNCTION
#====================================================================================
Evaluate = function(real,pred,echo=T){
  library(ROSE)
  n = length(real)
  l = levels(real)
  levels(pred)=l
  t = table(real,pred)
  acc = (t[1]+t[4])/n
  auc = roc.curve(real,pred, plotit = F)$auc
  if(echo){
    print(t)
    print(paste('Accuracy:',acc))
    print(paste('AUC:',auc))
  }
  invisible(list(accuracy=acc, auc=auc,table=t))
}
#====================================================================================
# WRITE SUMMARY FUNCTION
#====================================================================================
write_summary = function(dataset,path){
  sink(path)
  cat('id;name;type\n')
  for (i in 1:length(colnames(dataset))){
    nome = colnames(dataset)[i]
    cat(paste(i,';',nome,';',class(dataset[,nome]),'\n', sep = ''))
  }
  sink()
}
#====================================================================================
# GET POSITION SUMMARY
#====================================================================================
get_positions = function(dataset,names){
  v = colnames(dataset)
  v2 = c()
  for (name in names){
    k = which(v==name)
    v2 = c(v2, k)
  }
  return(sort(v2))
}
#====================================================================================
#  COLUMN O/1 UNION  
#====================================================================================
col.union <- function(df){
  df <- as.data.frame(sapply(df, factor))
  for (i in 1:ncol(df)){levels(df[,i])<-c("0","1")}; rm(i)
  df <- as.data.frame(sapply(df, as.character), stringsAsFactors = F)
  df <- as.data.frame(sapply(df, as.numeric))
  S <- rowSums(df)
  return(ifelse(S==0, 'no', 'yes'))
}

#====================================================================================
# RANDOM FOREST BALANCED FUNCTION
#====================================================================================
# RandomForestBalanced = function(X_train,y_train,X_test,N=500,B=1000,method='class'){
#   library(rpart)
#   matrice = c()
#   p = ncol(X_train)
#   Other = which(y_train=='other')
#   Stem = which(y_train=='stem')
#   
#   # repliche bootstrap 
#   for(i in 1:B) {
#     set.seed(i)
#     
#     # estrazione righe
#     quali_other = sample(Other, size = N, replace = T)
#     quali_stem = sample(Stem, size = N, replace = T)
#     quali = sort(c(quali_other,quali_stem))
#     
#     # estrazione colonne
#     quali_var = sample(1:p, size = as.integer(p/3), replace = F)
#     
#     # sotto-dataset
#     x_subtrain = X_train[quali, quali_var]
#     y_subtrain = y_train[quali]
#     x_subtest = X_test[, quali_var]
#     mydata = data.frame(x_subtrain,Class=y_subtrain)
#     
#     # modello albero rpart
#     ctrl = rpart.control(maxdepth = 30, minsplit = 20)
#     fit = rpart(Class~., data=mydata, control = ctrl)
#     
#     # previsioni
#     if (method=='class'|method=='class_submit'){prev = as.character(predict(fit, newdata=x_subtest, type="class"))}
#     else if (method=='mean'|method=='median'){prev = predict(fit, newdata=x_subtest, type="prob")[,"stem",drop=F]}
#     matrice = cbind(matrice, prev)
#   }
#   previsioni = c()
#   for(j in 1:nrow(matrice)){
#     if (method=='mean'){previsioni[j]=mean(matrice[j,])}
#     else if (method=='median'){previsioni[j]=median(matrice[j,])}
#     else if (method=='class_submit'){previsioni[j]=sum(matrice[j,] == "stem")/B}
#     else if (method=='class'){
#       somma=sum(matrice[j,] == "stem")/B
#       if(somma > 0.5){previsioni[j] = "stem"}
#       else if(somma < 0.5){previsioni[j] = "other"}
#       else if(somma == 0.5){previsioni[j] = sample(c("stem", "other"), size=1)}
#     }
#     
#   }
#   if (method=='class'){return(as.factor(previsioni))}
#   else {return(previsioni)}
# }