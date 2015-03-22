library(caret)
#getwd()
data <- read.csv("pml-training.csv")
dim(data)
str(data)
# exploring the data
dim(data)
str(data[,15:20])
data <- read.csv("pml-training.csv", na.strings = c("NA","", "#DIV/0!"))
colSums(is.na(data))
table(colSums(is.na(data)))
#keep cols where no NAs
data <- data[,colSums(!is.na(data))==nrow(data)]

#lets drop time and non releted var such as names, window
data <- data[,c(-1:-7)]

#test file data
datatestfinal <- read.csv("pml-testing.csv", na.strings = c("NA","", "#DIV/0!"))
datatestfinal <- datatestfinal[,c(-1:-7)]
datatestfinal <- datatestfinal[,colSums(!is.na(datatestfinal))==nrow(datatestfinal)]

#removing zero covariates, i.e. variables with no variability and therefore useless
TrainData <- createDataPartition(y =data$classe , p=0.7, list = FALSE)
training <- data[TrainData,]
testing <- data[-TrainData,]
#nsv <- nearZeroVar(training, saveMetrics = TRUE)
# studying correlation between factors 
#corrtraining <- abs(cor(training [-53]))
#diag(corrtraining) <- 0
#which (corrtraining > 0.8, arr.ind=T)
#dim(which (corrtraining > 0.8, arr.ind=T))

# random forest:
# modFit <- train(classe ~ ., data=training, methods='rf', number = 3, repeats = 3, ntree=5)
modFit
modfitImp <- varImp(modFit)
modfitImp
plot(modfitImp, top = 10)

variables <- c("roll_belt", "pitch_forearm", "yaw_belt", "pitch_belt", "magnet_dumbbell_z", "magnet_dumbbell_y", "roll_forearm", "accel_dumbbell_y", "magnet_dumbbell_x", "roll_dumbbell", "classe")

training <- (training[, variables])
modFit2 <- train(classe ~ ., data=training, methods='rf', number = 3, repeats = 3, ntree=500)


testing <- (testing[, variables])
pred <- predict(modFit2, testing)
confusionMatrix(testing$classe,pred)

# test file for submission    
datatestfinal  <- (datatestfinal[, variables])
variablestest <- c("roll_belt", "pitch_forearm", "yaw_belt", "pitch_belt", "magnet_dumbbell_z", "magnet_dumbbell_y", "roll_forearm", "accel_dumbbell_y", "magnet_dumbbell_x", "roll_dumbbell", "classe")
datatestfinal  <- (datatestfinal[, variables])
testpred <- predict(modFit2, datatestfinal)
#
pml_write_files = function(x){
+     n = length(x)
+     for(i in 1:n){
      +         filename = paste0("problem_id_",i,".txt")
      +         write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      +     }
+ }

pml_write_files(testpred)

