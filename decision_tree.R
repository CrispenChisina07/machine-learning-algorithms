library(tidyverse)
library(caTools)
library(rpart)
library(caret)
library(ROCR)

data_diab = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/diabetes.csv", 
                     col_types = cols())

set.seed(3)

id = sample(2, nrow(data_diab), prob = c(0.7,0.3), replace = TRUE)
diab_train = data_diab[id==1,]
diab_test = data_diab[id==2,]

data_diab$Outcome = as.factor(data_diab$Outcome)
diab_train$Outcome = as.factor(diab_train$Outcome)
diab_test$Outcome = as.factor(diab_test$Outcome)


diab_model = rpart(Outcome~., data = diab_train)

# plot(diab_model, margin = 0.1)
# 
# test(diab_model, use.n = TRUE, pretty = TRUE, cex = 0.8)

pred_diab = predict(diab_model, diab_test, type= "class")

confusionMatrix(table(pred_diab, diab_test$Outcome))

