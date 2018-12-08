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
library(MLmetrics)
blood_train <- read.csv('blood-train.csv')
blood_test <- read.csv('blood-test.csv')


names <- c('X','MonthsLast','Number','Volume','MonthsFirst','Donated')
colnames(blood_train) <- names
colnames(blood_test) <- names[1:5]

head(blood_test)
head(blood_train)


#blood_train$Donated <- as.factor(blood_train$Donated)

blood_train$Volume <- NULL
dim(blood_train)

blood_train$Frequency <- log(blood_train$MonthsFirst / blood_train$Number)
blood_train$MonthLFRatio <- log((blood_train$MonthsLast/(blood_train$MonthsFirst))+1)
blood_train$NumbByFirst <- log(blood_train$Number/(blood_train$MonthsFirst)+1)
blood_train$NumbByLast <- log((blood_train$Number/(blood_train$MonthsLast+1))+1)


head(blood_test)
dim(blood_test)

blood_test$Volume <- NULL
dim(blood_test)
names(blood_test)

blood_test$Frequency <- log(blood_test$MonthsFirst / blood_test$Number)
blood_test$MonthLFRatio <- log((blood_test$MonthsLast/(blood_test$MonthsFirst))+1)
blood_test$NumbByFirst <- log(blood_test$Number/(blood_test$MonthsFirst)+1)
blood_test$NumbByLast <- log((blood_test$Number/(blood_test$MonthsLast+1))+1)



head(blood_test)
blood_train <- blood_train[c(1:4,6:9,5)]    
head(blood_train)

#log -> numbyfirst,numbylast,  MonthLFRatio, frequenccy
cor_mat <- cor(blood_train)
cor_mat <- cor_mat[9,]
View(cor_mat)

# 
# blood_train$Number <- log(blood_train$Number)
# blood_train$MonthsLast <- log(2+blood_train$MonthsLast)
# blood_train$MonthsFirst<- log(2+blood_train$MonthsFirst)
# blood_train$MonthLFRatio <- log((blood_train$MonthsLast/(blood_train$MonthsFirst))+1)
# blood_train$NumbByLast <- log(blood_train$Number/(blood_train$MonthsLast+1))+1

View(blood_train)

set.seed(123)
ind <- sample(2,nrow(blood_train),replace=TRUE, prob = c(0.8,0.2))
train_data <- blood_train[ind==1,]
dev_data <- blood_train[ind==2,]
head(train_data)
head(dev_data)
dim(train_data)
dim(dev_data)
names(dev_data)
str(blood_train)
library(randomForest)
#now first defined formula for predictiion of "Survived"
Donated_equation <- 'Donated ~ NumbByLast + NumbByFirst + Number + Frequency + MonthLFRatio + MonthsFirst'
Donated_formula <- as.formula(Donated_equation)

str(Donated_formula)
train_data$Donated <- as.factor(train_data$Donated)
dev_data$Donated <- as.factor(dev_data$Donated) 
blood_model <- randomForest(formula= Donated_formula,
                            data= train_data,
                            ntree = 1200, mtry = 4,
                            nodesize = 21,
                            do.trace=TRUE)




table(dev_data$Donated)



Donated_prediction <- predict(blood_model, newdata = dev_data,type='class')

# class(Donated_prediction)
# head(Donated_prediction)
df <- as.data.frame(Donated_prediction[,2])
head(df)
df <- cbind(blood_test$X,df$`Donated_prediction[, 2]`)
df <- as.data.frame(df)
head(df)
names(df) <- c('','Made Donation in March 2007')
write.csv(df,'Prediction.csv',row.names = FALSE)


class(Donated_prediction)
cm <- confusionMatrix(Donated_prediction, as.factor(dev_data$Donated))
cm
table(dev_data$Donated)
cm$byClass
######################## RANDOM PARAMETER SEARCH FOR TUNING THE RANDOM FOREST ################################

#Best OOB Error: 20.31%

control <- trainControl(method="repeatedcv", number=5, repeats=3,search = 'random')
seed <- 123
metric <- "Accuracy"
set.seed(seed)
dev_data$Donated <- as.factor(dev_data$Donated)
mtry <- sqrt(ncol(dev_data))
tunegrid <- expand.grid(.mtry=mtry)
dev_data$X<- NULL
rf_default <- train(Donated~., data=dev_data, method="rf", metric = 'Kappa', tuneLength=12, trControl=control,ntree=1000,do.trace=TRUE,nodesize=4,importance=TRUE, proximity= TRUE)
print(rf_default)
attributes(rf_default)
rf_default


