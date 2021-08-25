library(tidyverse)
library(caTools)
library(rpart)
library(caret)
library(ROCR)
library(randomForest)

data_diab = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/diabetes.csv", 
                     col_types = cols())

set.seed(3233)
intrain = createDataPartition(y = data_diab$Outcome, p = 0.7, list = FALSE)
training  = data_diab[intrain,]
testing  = data_diab[-intrain,]

training$Outcome = as.factor(training$Outcome)
testing$Outcome = as.factor(testing$Outcome)

trcntrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

svmlinear = train(Outcome~., data = training, method = "svmLinear",
                  trControl = trcntrl,
                  preProcess = c("center", "scale"), tuneLength = 10
                  )

test_pred = predict(svmlinear, newdata = testing)



