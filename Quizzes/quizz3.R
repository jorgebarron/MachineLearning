### Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# Subset the data to a training set and testing set based on the Case variable in the data set
dim(segmentationOriginal)
training <- segmentationOriginal[segmentationOriginal$Case == 'Train',]
testing <- segmentationOriginal[segmentationOriginal$Case == 'Test',]

set.seed(125)

# fit a CART model with the rpart method using all predictor variables and default caret settings
modFit <- train(Class ~ ., method="rpart", data=training)



### Q3
library(pgmm)
data(olive)
olive = olive[,-1]

#  Fit a classification tree where Area is the outcome variable
modFit <- train(Area ~ ., method="rpart", data=olive)

# Then predict the value of area for the following data frame using the tree command with all defaults
modFit <- tree(Area ~ ., data=olive)
predict(modFit, newdata=newdata) 


### Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

# age at onset, current alcohol, obesity, tabacco, type-A, and low density lipoprotein cholesterol as predictors
# age, alcohol, tobacco, typea, ldl
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

modFit <- train(chd ~ (age + alcohol+ obesity + tobacco + typea + ldl), method="glm", data=trainSA, family="binomial")

missClass(trainSA$chd, predict(modFit, trainSA))

### Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

modFit <- train(y ~ ., data=vowel.train, methods='rf', prox=TRUE)





