########################################################
## create missing values and format NA data
########################################################
NA_Data <- function(na.data, NA.perc)
{
  rand.row <- sample(1:nrow(na.data), nrow(na.data)*NA.perc)
  for (i in rand.row) 
    { rand.col <- sample(1:(ncol(na.data)-1), (ncol(na.data)-1)*NA.perc)
      na.data[i, rand.col] = NA
    }
  print(sum(is.na(na.data))/(nrow(na.data)*ncol(na.data)))
  return(na.data)
}

########################################################
## variable discretization_rating assignment funcion
########################################################
discretize_fun <- function(dats, level)
{ 
  for (var in c(1:4,6:9))
  {
    cutoff <- c()
    cutoff <- quantile(dats[,var], seq(from=0,to=1,by=1/level), na.rm=TRUE)
    no.na.index <- which(!is.na(dats[,var]))
    
    for (r in no.na.index)
    { for  (j in 1:(level+1)) 
      {
      if(dats[r,var]<=cutoff[j+1] & dats[r,var]>cutoff[j]) {dats[r,var] <- j}
      }
    if (dats[r,var] == cutoff[1]) {dats[r,var] <- 1}
    }
  }
  return(dats)
}

#################################
## knn Imputation
#################################
knnImpute_fun <- function(dats)
{
  preproc <- preProcess(dats[,-10], method="knnImpute")
  imputed.data <- cbind(predict(preproc, dats[,-10]), dats[,10])
  colnames(imputed.data)[10] <- "chd"
  return (imputed.data)
}

#################################
## knn Error
#################################
knnError_fun <- function(train, test)
{
  knn_error <- c()
  for (k in 1:20)
  {
    knn_pred <- knn(train[,-10], test[,-10], train[,10], k)
    knn_error[k] <- sum(knn_pred != test[,10])/length(knn_pred)
  }
  opt_k <- which.min(knn_error)  
  min_error <- knn_error[opt_k]
  return(c(opt_k, min_error))
}

#################################
## random forest error
#################################
rf_knn_err_fun <- function(train, test, repeats)
{
    error <- c()
    for (re in 1:repeats)
    {
      rf.fit <- randomForest(chd~., data=train, ntree=3001) 
      pred <- predict(rf.fit, newdata=test)
      error[re] <- sum(pred != test[,10])/length(pred)
    }
    rf_knn_error <- mean(error)
    rf_knn_sd <- sd(error)
    return(c(rf_knn_error, rf_knn_sd))
}

rf_rf_err_fun <- function(train, test, repeats)
{ 
    error <- c()
    for (re in 1:repeats)
    {
      imputed.train <- rfImpute(chd~., data = train)
      imputed.test <- rfImpute(chd~., data = test)
      rf.fit <- randomForest(chd~., data=imputed.train, ntree=3001) 
      pred <- predict(rf.fit, newdata=imputed.test)
      error [re] <- sum(pred != test[,10])/length(pred)  
    }
    rf_rf_error <- mean(error)
    rf_rf_sd <- sd(error)
    return(c(rf_rf_error, rf_rf_sd))
}

#################################
## Rec Sys error
#################################
RecSys_error_fun <- function(discLevel, train, test)
{
  train <- as.matrix(train)
  test <- as.matrix(test)
  true <- test[,10]
  test[,10] <- NA
  
  training <- as(train, "realRatingMatrix")
  testing <- as(test, "realRatingMatrix")
  
  recom <- Recommender(training, method ="UBCF",
                      param=list(normalize="Z-score", method="pearson", 
                                 nn=25, sample=F))
  pred <- predict(recom, newdata=testing, type="ratings")
  pred.matrix <- as(pred, "matrix")
 
  # aucPred <- prediction(pred.matrix[,10], true)
  # perf <- performance(aucPred, "auc")
  # Auc <- perf@y.values
  # perf <- performance(aucPred, "tpr", "fpr")
  # plot(perf, colorize=TRUE)
  
  pred.class <- ifelse(pred.matrix[,10]<= 1.5, 1, 2)
  RecSys_error <- (sum ((true!= pred.class), na.rm=TRUE))/length(true)
  return(RecSys_error)
}