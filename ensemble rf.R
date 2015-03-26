setwd("C:/Users/pp/Desktop/kaggle/driver")

time <- proc.time()
set.seed(3921)
seed <- sample(c(0: 298592), size=5)

GetResult<-function(n)
{
  drivers <- dir("../driver/drivers/")
  set.seed(seed[n])
  randomDrivers = sample(drivers, size = 5)
  
  dats_1 <- NULL
  for (driver1 in randomDrivers)
  {
    filename <- paste("../driver/feat_file/", driver1, ".txt", sep='')
    temp <- read.table(file=filename,header=TRUE)
    
    indis <- which(is.na(temp), arr.ind=TRUE)
    for (r in 1:nrow(indis))
     { temp[indis[r,1], indis[r,2]]=0 }
    dats_1 <- rbind(dats_1, temp)
  }
  dats_1$response <- 0
  
  result <- matrix(nrow=0, ncol=2)
  for (driver in drivers)
  {
  filename <- paste("../driver/feat_file/", driver, ".txt", sep='')
  dats_2 <- read.table(file=filename,header=TRUE)
  indis <- which(is.na(dats_2), arr.ind=TRUE)
  for (r in 1:nrow(indis))
  { dats_2[indis[r,1], indis[r,2]]=0 }
  dats_2$response <- 1
    
  train <- rbind(dats_1, dats_2)
  rf <- randomForest(response~., data=train, ntree=2001)
  p <- predict(rf, dats_2, type="response")
  
  labels = sapply(1:200, function(x) paste0(driver,'_', x))
  oneResult = cbind(labels, p)
  result <- rbind(result, oneResult)
}
  return(result)
}

library(parallel)
## Number of workers (R processes) to use:
numWorkers<-3 
## Set up the 'cluster'
cl<-makeCluster(numWorkers,type="PSOCK")
clusterEvalQ(cl, {library(randomForest)})
clusterExport(cl, c("seed"))
## Parallel calculation (parLapply):
res<- parallel::parLapply(cl, n <- (1:5), GetResult)
## Shut down cluster
stopCluster(cl)
proc.time() - time

one <- res[[1]][,2]
one <- as.numeric(one)
two <- res[[2]][,2]
two <- as.numeric(two)
three <- res[[3]][,2]
three <- as.numeric(three)
four <- res[[4]][,2]
four <- as.numeric(four)
five <- res[[5]][,2]
five <- as.numeric(five)

aveResult <- matrix(nrow=547200, ncol=2, )
for (i in 1:547200)
{
  aveResult[i,1] <- res[[1]][i,1]
  aveResult[i,2] <- mean(c(one[i], two[i], three[i], four[i], five[i]))
  temp <- sd(c(one[i], two[i], three[i], four[i], five[i]))
}

ave <- aveResult[,2]
ave <- as.numeric(ave)
lab <- res[[1]][,1]
finalResult <- data.frame(cbind(lab, ave), stringsAsFactors=FALSE)

finalLab <-matrix(,nrow=0,ncol=1)
drivers <- dir("../driver/drivers/")
tempfiles <- dir("../driver/drivers/1")
for (driver_num in 1:2736)
{
  tempLab <-matrix(,nrow=200,ncol=1)
  for (trip_num in 1:200)
  {
    tempLab[trip_num,1]=paste(drivers[driver_num],"_",
                                substring(tempfiles[trip_num], 1,
                                          nchar(tempfiles[trip_num])-4), sep='')
  }
  finalLab <- rbind(finalLab,tempLab)
}

finalResult[,1] <- finalLab
colnames (finalResult) <- c("driver_trip", "prob")

filename <- paste("../driver/finalresult_ens_rf.csv", sep='')
write.csv(finalResult,file=filename,row.names=FALSE)




########################################################
finalresult <-matrix(,nrow=0,ncol=2)
colnames (finalresult) <- c("driver_trip", "prob")
tempfiles <- dir("../driver/drivers/1")
for (driver_num in 1:2736){
  tempmatrix <- as.matrix(res[[driver_num]])
  for (trip_num in 1:200)
  {tempmatrix[trip_num,1]=paste(drivers[driver_num],"_",
                                substring(tempfiles[trip_num], 1,
                                          nchar(tempfiles[trip_num])-4), sep='')
  }
  finalresult <- rbind(finalresult,tempmatrix)
}

  filename <- paste("../driver/finalresult_rf.csv", sep='')
  write.csv(finalresult,file=filename,row.names=FALSE)


