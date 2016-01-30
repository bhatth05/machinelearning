traindata <- read.csv(file="pml-training.csv",na.strings=c("NA",""),header=TRUE,sep=",")
testdata <- read.csv(file="pml-testing.csv",na.strings=c("NA",""),header=TRUE,sep=",")
ncol(traindata)
ncol(testdata)
all.equal(colnm1[1:length(colnm1)-1],colnm2[1:length(colnm2)-1])
colnm1[160]
colnm2[160]
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

NACnt <- function(x) {
    as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
    }	
nadata <- NACnt(traindata)
traindataupd <- traindataupd[, !names(traindataupd) %in% dropCol]
ncol(traindataupd)
traindataupd <- traindataupd[,8:length(traindataupd)]
ncol(traindataupd)

testdataupd <- testdataupd[, !names(testdataupd) %in% dropCol]
testdataupd <- testdataupd[,8:length(testdataupd)]

nzv <- nearZeroVar(traindataupd)

set.seed(53)
pTrain <- createDataPartition(y=traindataupd$classe, p=0.75, list=FALSE)
partitionedTrain <- traindataupd[pTrain,]
partitionedTest <- traindataupd[-pTrain,]

modelRF <- train(partitionedTrain$classe ~ .,method = "rf", trControl=trainControl(method = "cv", number = 4),data=partitionedTrain)

modelRF$finalModel
predictResForData <- predict(modelRF,testdataupd)
predResForData

modelRFFull <- train(traindataupd$classe ~ .,method = "rf", trControl=trainControl(method = "cv", number = 4),data=traindataupd)

predResForDataFull <- predict(modelRFFull,testdataupd)
predResFull <- predict(modelRFFull,newdata=partitionedTest)
confusionMatrix(partitionedTest$classe, predResFull)
predResForDataComplete <- predict(modelRFFull,traindataupd)
confusionMatrix(traindataupd$classe, predResForDataComplete)

predResForDataFull
modelRFFull$finalModel


