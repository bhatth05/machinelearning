# Practical Machine Learning Course Project
## Executive Summary:
This document describes methods to visualize arbitrary data sets. This uses the 'R Programming Language' - a language used predominantly in statistics.
In this project, we are using built in graphics packages with our custom R functions. 
We are able to analyze one dimensional data, two dimensional data and correlations between multi dimensional data.
This is very useful in doing exploratory data analysis and to understand the relationship between data better.
In plotting 2 dimensional data, we are able to deal with non numeric data as well.

## Cleaning up Data
traindata <- read.csv(file="pml-training.csv",na.strings=c("NA",""),header=TRUE,sep=",")
testdata <- read.csv(file="pml-testing.csv",na.strings=c("NA",""),header=TRUE,sep=",")
ncol(traindata)
ncol(testdata)
all.equal(colnm1[1:length(colnm1)-1],colnm2[1:length(colnm2)-1])

[1] TRUE

> colnm1[160]
[1] "classe"
> colnm2[160]
[1] "problem_id"

### Reducing unneeded columns 

Enable parallel processing:
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


First count NAs in each column and drop the ones that have NAs in them from both training and testing data set

NACnt <- function(x) {
    as.vector(apply(x, 2, function(x) length(which(is.na(x)))))
    }
	
nadata <- NACnt(traindata)

### Removing the NA columns from training set
traindataupd <- traindataupd[, !names(traindataupd) %in% dropCol]
ncol(traindataupd)
[1] 60
traindataupd <- traindataupd[,8:length(traindataupd)]
ncol(traindataupd)
[1] 53

### Remove the NAs from the testing set
testdataupd <- testdataupd[, !names(testdataupd) %in% dropCol]
testdataupd <- testdataupd[,8:length(testdataupd)]

### The next step is to remove variables with very little variance using the nearZeroVar function
nzv <- nearZeroVar(traindataupd)
This shows that there are no variables with zero variance

## Setting up training data for cross validation

We split the training set to two to do cross validation. First a seed is set to a prime number and the training data is split
at 75% and 25% each.
In addition, using trainControl method, we also do cross validation

set.seed(53)
pTrain <- createDataPartition(y=traindataupd$classe, p=0.75, list=FALSE)
partitionedTrain <- traindataupd[pTrain,]
partitionedTest <- traindataupd[-pTrain,]

## Using Random Forest Algorithm
### Use random forest algorithm to do the prediction
modelRF <- train(partitionedTrain$classe ~ .,method = "rf", trControl=trainControl(method = "cv", number = 4),data=partitionedTrain)


```r
modelRF$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.6%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 4183    1    0    0    1 0.0004778973
## B   15 2829    4    0    0 0.0066713483
## C    1   16 2549    1    0 0.0070120764
## D    0    0   39 2371    2 0.0169983416
## E    0    0    2    6 2698 0.0029563932
```

```r
predictResForData <- predict(modelRF,testdataupd)
predResForData
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

### Building a model using all the training data and doing cross validation using trainControl method

Now check if there is a difference by using the full training data to build a model

modelRFFull <- train(traindataupd$classe ~ .,method = "rf", trControl=trainControl(method = "cv", number = 4),data=traindataupd)


```r
predResForDataFull <- predict(modelRFFull,testdataupd)
```

#### Validating with full training data:



```r
predResFull <- predict(modelRFFull,newdata=partitionedTest)
confusionMatrix(partitionedTest$classe, predResFull)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    0  949    0    0    0
##          C    0    0  855    0    0
##          D    0    0    0  804    0
##          E    0    0    0    0  901
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9992, 1)
##     No Information Rate : 0.2845     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

### Testing it on the full training dataset shows the confusion matrix and accuracy of 100%.


```r
predResForDataComplete <- predict(modelRFFull,traindataupd)
confusionMatrix(traindataupd$classe, predResForDataComplete)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 5580    0    0    0    0
##          B    0 3797    0    0    0
##          C    0    0 3422    0    0
##          D    0    0    0 3216    0
##          E    0    0    0    0 3607
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9998, 1)
##     No Information Rate : 0.2844     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2844   0.1935   0.1744   0.1639   0.1838
## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

```r
predResForDataFull
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
modelRFFull$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.44%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 5577    2    0    0    1 0.0005376344
## B   13 3779    5    0    0 0.0047405847
## C    0   18 3403    1    0 0.0055523086
## D    0    0   40 3174    2 0.0130597015
## E    0    0    0    5 3602 0.0013861935
```

### Conclusion
We tried two models using random forest algorithm. The first one split the training set into two. 75% of the training set was used for
training and the remaining 25% was used for cross validation. From the confusion matrix on the cross validation set, the accuracy in this case is lower
at 99.35%.

Then we tried using the full training set in the random forest algorithm using cross validation in the trainControl method. On testing the model
on the full training set and the 25% training set used for cross validation earlier, the accuracy is 100%. The model's accuracy is also higher at 99.56%
In the second case, the model is likely overfitting the training data.

We used both the models on the testing set. The outputs from the two models were identical. On inputting the result into the quiz, all answers turned out to be correct.



