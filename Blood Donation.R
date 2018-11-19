library(MASS, quietly=TRUE)
library(caret)
library(ROSE)
library(ROCR)
library(BBmisc)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ROSE)
library(lattice)
library(ggplot2)
library(caret)

setwd('C:/Users/akshit/Desktop/Data Analytics Challenge/Blood Donation Challenge')
getwd()

#loading training and testing dataset
blood_train_or <- read.csv('blood-train.csv')
blood_test_or <- read.csv('blood-test.csv')

View(blood_test_or)
blood_train <- blood_train_or
blood_test <- blood_test_or

blood_train$X <- NULL
blood_test$X <- NULL


str(blood_train)

names <- c('MonthsLast','Number','Volume','MonthsFirst','Donated')
colnames(blood_train) <- names
colnames(blood_test) <- names[1:4]

head(blood_test)
head(blood_train)

blood_test$Donated <- NA
str(blood_test)
#check that no datapoint is missing, otherwise we need to fix the dataset
apply(blood_train,2,function(x) sum(is.na(x)))
apply(blood_test,2,function(x) sum(is.na(x)))



blood_train$isTrainData <- TRUE
blood_test$isTrainData <- FALSE




blood_full <- rbind(blood_train, blood_test)
dim(blood_train)
dim(blood_test)
dim(blood_full)

range01 <- function(x)
{
  (x-min(x))/(max(x)- min(x))
}

df <- data.frame(c(1,2,3,4,5,6,7,8,9,10))
range01(df)

head(blood_full)

# Normalizing the Predictor Variables 

blood_full$Volume <- range01(blood_full$Volume)
blood_full$Number <- range01(blood_full$Number)
blood_full$MonthsLast <- range01(blood_full$MonthsLast)
blood_full$MonthsFirst<- range01(blood_full$MonthsFirst)

head(blood_full)

# Correlations of each Variable Towards 'Donated' Variable'

# cor(as.numeric(blood_train$MonthsFirst),as.numeric(blood_train$Donated))
# cor(as.numeric(blood_train$MonthsLast),as.numeric(blood_train$Donated))
# cor(as.numeric(blood_train$Volume),as.numeric(blood_train$Donated))
# cor(as.numeric(blood_train$Number),as.numeric(blood_train$Donated))



blood_full$Donated <- as.factor(blood_full$Donated)

#now extract back Train and Test dataset from full dataset
#to identify train and test data we use "isTrainData" variable value
blood_train <- blood_full[blood_full$isTrainData==TRUE, ]
blood_test <- blood_full[blood_full$isTrainData==FALSE, ]


blood_train$isTrainData <- NULL
blood_test$isTrainData <- NULL

#fitting a random forest model

str(blood_train)
library(randomForest)
#now first defined formula for predictiion of "Survived"
Donated_equation <- "Donated ~ MonthsLast + Number + Volume + MonthsLast"
Donated_formula <- as.formula(Donated_equation)

str(Donated_formula)

blood_model <- randomForest(formula= Donated_formula, 
                            data= blood_train, 
                            ntree = 2000, mtry = 1, 
                            nodesize = 12,
                            do.trace=TRUE)
plot(blood_model)


#prediction on test dataset
View(blood_test)

Donated_prediction <- predict(blood_model, newdata = blood_test,type='prob')
head(Donated_prediction)


f <- data.frame(Donated_prediction)
class(f)
head(f)
View(f)


output <- cbind(blood_test_or$X,f$X1)


df <- as.data.frame(output)
head(df)
colnames(df)[1] <- ''
head(output)
View(output)
write.csv(output, file = "Predictions.csv", row.names = FALSE)


######################## RANDOM PARAMETER SEARCH FOR TUNING THE RANDOM FOREST ################################

#Best OOB Error: 20.31%

control <- trainControl(method="repeatedcv", number=10, repeats=3,search = 'random')
seed <- 123
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(blood_test))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Donated~., data=blood_train, method="rf", metric=metric, tuneLength=12, trControl=control,ntree=1000,do.trace=TRUE,nodesize=4,importance=TRUE, proximity= TRUE)
print(rf_default)
attributes(rf_default)



