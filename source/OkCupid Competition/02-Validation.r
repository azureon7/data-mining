#=============================
# 2. VALIDATION
#=============================

rm(list=ls())

# READ FUNCTIONS
source("source/OkCupid_Functions.R")
source("source/RandomForestBalanced.R")

# VARIABILI GLM
variablesGLM <- c("male", "essay_link", "tech", "computer", "science", "fixing", 
                  "matrix", "electronic", "nerdy", "artist", "SV", "body_type_A", "diet_A", "single",
                  "student", "teaching", "loyal", "atheist", "smoke", "sign_imp", 
                  "education_A", "income100000", "phdYN", "UnionDummy", "Class") 

# READ CSV
d <- read.csv('./new_datasets/data.csv')
d=d[,variablesGLM]

# CREO TEST SET PER VALUTARE MODELLI
set.seed(1)
N <- 200
p <- sample(1:nrow(d), size=N, replace = F)
y_train <- d[-p, 'Class']
X_train <- d[-p, which(colnames(d)!='Class')]
y_test <- d[p,'Class']
X_test <- d[p, which(colnames(d)!='Class')]

rm(N,p,write_summary,col.union,get_positions)

# K-FOLDS CROSS VALIDATION
CrossValidationAlpha <- function(X_train,y_train,K=10,range.alpha,B=200){
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
      fit <- rfb.fit(x_subtrain,y_subtrain, theta=.8, alpha=alpha, B=B)
      y_pred <- rfb.predict(fit, newdata=x_subtest, method="class")
      AUC[k]=Evaluate(y_subtest,y_pred,echo=F)$auc
    }
    AUC.estimate[u] = mean(AUC)
    print(u)
  }
  return(AUC.estimate)
}

AUC.alpha = CrossValidationAlpha(X_train,y_train, K=20, range.alpha = seq(from=0.1,to=1,0.1), B=200)

plot(seq(from=0.1,to=1,0.1), AUC.alpha, type='b')
