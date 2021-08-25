library(tidyverse)
library(caTools)
library(rpart)
library(caret)
library(ROCR)
library(randomForest)

data_diab = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/diabetes.csv", 
                     col_types = cols())

set.seed(3)

id = sample(2, nrow(data_diab), prob = c(0.7,0.3), replace = TRUE)
diab_train = data_diab[id==1,]
diab_test = data_diab[id==2,]

data_diab$Outcome = as.factor(data_diab$Outcome)
diab_train$Outcome = as.factor(diab_train$Outcome)

bestmtry = tuneRF(diab_train, diab_train$Outcome, stepFactor = 1.2,
                  improve = 0.01, trace = T, plot = T)

diab_forest = randomForest(Outcome~., data = diab_train)

importance(diab_forest)

pred_diab = predict(diab_forest, diab_test, type= "class")

confusionMatrix(table(pred_diab, diab_test$Outcome))



