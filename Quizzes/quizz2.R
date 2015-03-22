### Q 2

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


library(Hmisc)
qplot(,training$CompressiveStrength, colour=training$BlastFurnaceSlag)
training$Age2 <-  cut2(training$Age, g = 5)
qplot(,training$CompressiveStrength, colour=training$Age2)

training$FlyAsh2 <-  cut2(training$FlyAsh, g = 5)
qplot(,training$CompressiveStrength, colour=training$FlyAsh2)

_________
### Q 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
training$Superplasticizer2 <-  log(training$Superplasticizer)
hist(training$Superplasticizer2)

training$Superplasticizer3 <-  log(training$Superplasticizer + 1)
hist(training$Superplasticizer3)

_________
### Q 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[, grep("IL", names(training))]
preProcesstraining <- preProcess(trainingIL[,], method = c("center", "scale"))
preProcess(trainingIL[,-1], method = c("pca"), thresh=0.9 )


_________
### Q 5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
trainingIL <- training[, grep("IL", names(training))]
trainingIL <- trainingIL[,-13]
trainingIL['diagnosis'] <- training[,1]
preProcess(trainingIL[,-13], method = c("pca"), thresh=0.8)
# PCA needed 7 components to capture 80 percent of the variance

preProcIL <- preProcess(trainingIL[,-13], method="pca", pcaComp = 7)
trainPCIL <- predict(preProcIL, trainingIL[,-13])
modelFitIL <- train(trainingIL$diagnosis ~ ., method="glm", data=trainPCIL)

testingIL <- testing[, grep("IL", names(testing))]
testingIL <- testingIL[,-13]
testingIL['diagnosis'] <- testing[,1]

testIL <- predict(preProcIL, testingIL[,-13])
confusionMatrix(testingIL$diagnosis, predict(modelFitIL, testIL))

###   Accuracy : 0.7195 

trainfit <- train(diagnosis ~ ., data = trainingIL, method = "glm")
confusionMatrix(testingIL$diagnosis, predict(trainfit, testingIL))

###   Accuracy : 0.6463
