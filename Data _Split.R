#clear lists
rm(list=ls())

#load the libraries
library(caret)
library(klaR)

#load the iris dataset
data(iris)

#summary of data loaded
summary(iris)

#80%/20% and 70%/30% are common ratios of training and testing data for models
#we will run it both ways to compare results

#set seed to replicate results
set.seed(12345)

#split dataset into 80%/20% (the bigger number is the training data and the smaller will be the test data)
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]

#model in NaiveBayes, the ~ after the dependent variable denotes all variables will be used to train the model
model <- NaiveBayes(Species~., data=data_train)

#display the model information
model

#make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
predictions

#summarize results
confusionMatrix(predictions$class, y_test)

#The Reference shows a 96.67% accuracy where 1 prediction was wrong 


########################################### New Data Split ##########################################
#set seed to replicate results
set.seed(1234)

#split dataset to a 70%/30%
split1=0.70
trainIndex <- createDataPartition(iris$Species, p=split1, list=FALSE)
data_train1 <- iris[ trainIndex,]
data_test1 <- iris[-trainIndex,]

#predict model based on data_train1 with 70% of data used to train
model1 <- NaiveBayes(Species~., data = data_train1)

#display the model information
model1

#make predictions
x_test1 <- data_test1[,1:4]
y_test1 <- data_test1[,5]
predictions1 <- predict(model, x_test1)
predictions1

#summarize results
confusionMatrix(predictions1$class, y_test1)

#The Reference shows a 93.33% accuracy where 3 predictions were wrong compared to the first results of only 1 wrong

#There isn't a best way to handle a dataset split. It typically goes by a standard set in place by the department you work for but
# it isn't to say that better results can't be found from different splits.

