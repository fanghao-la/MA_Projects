install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("caret")
library(caret)
install.packages("class")
library(class)
install.packages("recommenderlab")
library(recommenderlab)
install.packages("randomForest")
library(randomForest)
install.packages("RANN")
library(RANN)
data(SAheart)
sapply(SAheart, class)

setwd("C:/Users/pp/Dropbox/MA project/New folder (2)")
source("def_funs2.R")

working.data <- SAheart
working.data$chd <- as.factor(working.data$chd)   # value of 0 & 1
working.data$famhist <- as.numeric(working.data$famhist)  # value of 1 & 2

#################################
time <- proc.time()

k = 5     # 5-fold cross validation
set.seed(1857)
cv.indis <- sample(1:k, nrow(SAheart), replace=TRUE)

## define a function to get the error rate for the ith fold
GetError <- function(i)
{
  train <- working.data[cv.indis != i,]
  test <- working.data[cv.indis == i,]
  
  na_perc <- 0.7   # controlling NA percentage
  NaTrain <- NA_Data(train,na_perc)
  NaTest <- NA_Data(test,na_perc)
  
  DiscTrain <- list()
  DiscTest <- list()
  ImputedTrain <- list()
  ImputedTest <- list()
  Error <- list()
  
  ## j corresponds to the level of discretization
  for (j in 2:5)
  {
    DiscTrain[[j]] <- discretize_fun(NaTrain, j)
    DiscTest[[j]] <- discretize_fun(NaTest, j)
  
    ImputedTrain[[j]] <- knnImpute_fun(DiscTrain[[j]])
    ImputedTest[[j]] <- knnImpute_fun(DiscTest[[j]])
    
    Error[[j]] <- matrix(nrow=8, ncol=1)
    Error[[j]][1:2,] <- knnError_fun(ImputedTrain[[j]], ImputedTest[[j]])
    Error[[j]][3:4,]  <- rf_knn_err_fun(ImputedTrain[[j]], ImputedTest[[j]], 15)
    Error[[j]][5:6,] <- rf_rf_err_fun(DiscTrain[[j]], DiscTest[[j]], 15)
    DiscTrain[[j]]$chd <- as.numeric(DiscTrain[[j]]$chd)   # value of 1 or 2
    DiscTest[[j]]$chd <- as.numeric(DiscTest[[j]]$chd)     # value of 1 or 2
    Error[[j]][7,] <- RecSys_error_fun(j, DiscTrain[[j]], DiscTest[[j]])
    ImputedTrain[[j]]$chd <- as.numeric(ImputedTrain[[j]]$chd)
    ImputedTest[[j]]$chd <- as.numeric(ImputedTest[[j]]$chd)
    Error[[j]][8,] <- RecSys_error_fun(j, ImputedTrain[[j]], ImputedTest[[j]])
  } 
return(Error)
}

## obtain the error rate for the ith fold (temp[[i]])
temp <- list()
for (i in 1:k)
{
  temp[[i]] <- GetError(i)
}
proc.time() - time

## temp is a list, the components of which are also lists.
## temp[[i]][[j]] is a matrix, which contains the error rates 
## for the ith fold with j-level discretization. Each matrix has 
## dimension of 8 x 1. 
## Row 1 & 2: optimal k value and error for knn;
## Row 3 & 4: error and sd for knn imputation + rf;
## Row 5 & 6: error and sd for rf imputation + rf;
## Row 7: error for rec sys;
## Row 8: error for knn imputation + rec sys.


## tmp is a matrix, which row corresponds to different algorithms and 
##                        column corresponds to ith fold for a specific discretization level
j = 2     # taking 2-level discretization as an example
tmp <- matrix(nrow=8, ncol=5)
for (i in 1:k)
{
  tmp[,i] <- temp[[i]][[j]]
}

error_jLevel <- apply(tmp, 1, mean)



##################################################
## Please ignore the code below for now
## code for implementation for parallel computing
##################################################
library(parallel)
numWorkers<-4 
cl<-makeCluster(numWorkers,type="PSOCK")

clusterEvalQ(cl, library(randomForest))
clusterEvalQ(cl, library(class))
clusterEvalQ(cl, library(caret))
clusterEvalQ(cl, library(recommenderlab))
clusterEvalQ(cl, library(RANN))
clusterEvalQ(cl, source("def_funs2.R"))
clusterExport(cl, c("seed", "working.data", "cv.indis"))

# clusterCall(cl, function() {source("C:/Users/cxiong/Downloads/def_funs2.R")})

res<-  parallel::parLapply(cl, i <- (1:5), GetError)
stopCluster(cl)
proc.time() - time

ErrorMatrix <- vector("list", 5)
for (j in 2:5)
{
  for (i in 1:5)
  {
    ErrorMatrix [[j]] <- cbind(ErrorMatrix[[j]], res[[i]][[j]])
  }
}

saveRDS(ErrorMatrix, "Error_6perc_2")


#############################################

Na6perc <- readRDS("Error_6perc_2")[2:5]
Na20perc <- readRDS("Error_20perc_1")[2:5]
Na42perc <- readRDS("Error_42perc_1")[2:5]
Na56perc <- readRDS("Error_56perc_1")[2:5]
Na59perc <- readRDS("Error_59perc_1")[2:5]

rownames <- c("opt_k", "knn","knn_rf","knn_rf_sd", "rf_rf", 
             "rf_rf_sd", "rec_sys", "rec_sys_knn")
colnames <- c("level_2", "level_3", "level_4", "level_5")

summary6perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
summary20perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
summary42perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
summary56perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
summary59perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))

for (m in 1:4)
{
  summary6perc[,m] <- apply(Na6perc[[m]], 1, mean)  
  summary20perc[,m] <- apply(Na20perc[[m]], 1, mean)  
  summary42perc[,m] <- apply(Na42perc[[m]], 1, mean)  
  summary56perc[,m] <- apply(Na56perc[[m]], 1, mean)    
  summary59perc[,m] <- apply(Na59perc[[m]], 1, mean)    
}

summary6perc[-c(1,4,6),]
summary20perc[-c(1,4,6),]
summary42perc[-c(1,4,6),]
summary56perc[-c(1,4,6),]
summary59perc[-c(1,4,6),]
###############################
sd6perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
sd20perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
sd42perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))
sd56perc <- matrix(nrow=8, ncol=4, dimnames = list(rownames, colnames))

for (m in 1:4)
{
  sd6perc[,m] <- apply(Na6perc[[m]], 1, sd)  
  sd20perc[,m] <- apply(Na20perc[[m]], 1, sd)  
  sd42perc[,m] <- apply(Na42perc[[m]], 1, sd)  
  sd56perc[,m] <- apply(Na56perc[[m]], 1, sd)    
}

sd6perc_vec <- c(sd6perc[-c(1,4,6),])
limits6perc <- aes(ymax = value + sd6perc_vec, ymin = value - sd6perc_vec)
sd20perc_vec <- c(sd20perc[-c(1,4,6),])
limits20perc <- aes(ymax = value + sd20perc_vec, ymin = value - sd20perc_vec)
sd42perc_vec <- c(sd42perc[-c(1,4,6),])
limits42perc <- aes(ymax = value + sd42perc_vec, ymin = value - sd42perc_vec)
sd56perc_vec <- c(sd56perc[-c(1,4,6),])
limits56perc <- aes(ymax = value + sd56perc_vec, ymin = value - sd56perc_vec)
###############################
library(ggplot2)
library(reshape2)

error.df6 <- data.frame(summary6perc[-c(1,4,6),])
error.df6$alg <- c("knn","knn_rf","rf_rf","rec_sys","rec_sys_knn")
error.df6.long <- melt(error.df6, id.vars="alg")
x11()
jpeg("na_6_perc_1.jpeg")
ggplot(data=error.df6.long, 
       aes(x=alg, y=value, 
           group=variable, colour=variable))+
  geom_line() + geom_point(size=3, type=21)+
#  geom_errorbar(limits6perc, width=0.1)+
  ylab("Misclassification Error") + 
  xlab("Number of Rating Levels") + 
  ggtitle("na_6_perc")
dev.off()

error.df20 <- data.frame(summary20perc[-c(1,4,6),])
error.df20$alg <- c("knn","knn_rf","rf_rf","rec_sys","rec_sys_knn")
error.df20.long <- melt(error.df20, id.vars="alg")
x11()
jpeg("na_20_perc_1.jpeg")
ggplot(data=error.df20.long, 
       aes(x=alg, y=value, 
           group=variable, colour=variable))+
  geom_line() + geom_point(size=3, type=21)+
  #geom_errorbar(limits, width=0.1)+
  ylab("Misclassification Error") + 
  xlab("Number of Rating Levels") + 
  ggtitle("na_20_perc")
dev.off()

error.df42 <- data.frame(summary42perc[-c(1,4,6),])
error.df42$alg <- c("knn","knn_rf","rf_rf","rec_sys","rec_sys_knn")
error.df42.long <- melt(error.df42, id.vars="alg")
x11()
jpeg("na_42_perc_1.jpeg")
ggplot(data=error.df42.long, 
       aes(x=alg, y=value, 
           group=variable, colour=variable))+
  geom_line() + geom_point(size=3, type=21)+
  #geom_errorbar(limits, width=0.1)+
  ylab("Misclassification Error") + 
  xlab("Number of Rating Levels") + 
  ggtitle("na_42_perc")
dev.off()

error.df56 <- data.frame(summary56perc[-c(1,4,6),])
error.df56$alg <- c("knn","knn_rf","rf_rf","rec_sys","rec_sys_knn")
error.df56.long <- melt(error.df56, id.vars="alg")
x11()
jpeg("na_56_perc_1.jpeg")
ggplot(data=error.df56.long, 
       aes(x=alg, y=value, 
           group=variable, colour=variable))+
  geom_line() + geom_point(size=3, type=21)+
  #geom_errorbar(limits, width=0.1)+
  ylab("Misclassification Error") + 
  xlab("Number of Rating Levels") + 
  ggtitle("na_56_perc")
dev.off()

error.df59 <- data.frame(summary59perc[-c(1,4,6),])
error.df59$alg <- c("knn","knn_rf","rf_rf","rec_sys","rec_sys_knn")
error.df59.long <- melt(error.df59, id.vars="alg")
x11()
jpeg("na_59_perc_1.jpeg")
ggplot(data=error.df59.long, 
       aes(x=alg, y=value, 
           group=variable, colour=variable))+
  geom_line() + geom_point(size=3, type=21)+
  #geom_errorbar(limits, width=0.1)+
  ylab("Misclassification Error") + 
  xlab("Number of Rating Levels") + 
  ggtitle("na_59_perc")
dev.off()
#####################
## create NA's 
## and discretize
## and knn imputation
#####################
# perc=0.3 ~ 6%  Na's (1617s) (2086s)
# perc=0.5 ~ 20%  Na's
# perc=0.7 ~ 42%  Na's (1791.82)
# perc=0.8 ~ 56%  Na's  (1770)

# perc=0.85 ~ 59%  Na's --------- knn imputation starts to show warning message
#Warning message:
#  In preProcess.default(dats[, -10], method = "knnImpute") :
#  These variables have zero variances: famhist
# perc=0.9 ~ 72%  Na's
# perc=0.99 ~ 80%  Na's
#Error in cutoff[i] <- sort(dats[no.na.index, var])[(i - 1) * index] : 
# replacement has length zero

#perc <- c(0.3, 0.5, 0.7, 0.8)

########################################
###### random forest on raw data #######
working_data <- SAheart
working_data$chd <- as.factor(working_data$chd)

set.seed(1839)
seed <- runif(10, 0, 52486)

rf_error <- c()
knn_error <- c()
for (i in 1:10)
{
  set.seed(seed[i])
  train.indis <- createDataPartition(working_data$chd, p=0.7, list=FALSE, time=1)
  train <- working_data[train.indis,]
  test <- working_data[-train.indis,]
  
  rf_fit <- randomForest(chd~., ntree=3001, data=train)
  rf_pred <- predict(rf_fit, newdata=test, type="response")
  rf_error[i] <- sum(rf_pred != test[,10])/length(rf_pred)
}  

###### knn #######
working_data <- SAheart
working_data$famhist <- as.numeric(working_data$famhist)  # value of 1 & 2

set.seed(1839)
seed <- runif(10, 0, 52486)

rf_error <- c()
knn_error <- c()
for (i in 1:10)
{
  working_data$chd <- as.factor(working_data$chd)
  set.seed(seed[i])
  train.indis <- createDataPartition(working_data$chd, p=0.7, list=FALSE, time=1)
  working_data$chd <- as.numeric(working_data$chd)
  train <- working_data[train.indis,]
  test <- working_data[-train.indis,]
  
  error <- c()
  for (k in 1:20)
  {
    knn_pred <- knn(train[,-10], test[,-10], train[,10], k)
    error[k] <- sum(knn_pred != test[,10])/length(knn_pred)
  }
  knn_error[i] <- min(error)
}
#############################################################

