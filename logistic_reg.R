library(tidyverse)
library(caTools)
library(rpart)
library(caret)
library(ROCR)

data_diab = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/diabetes.csv", 
                         col_types = cols())

#split_vals = sample.split(data_diab$anycolumn, SplitRatio = 0.8)

split_vals = sample.split(data_diab$Glucose, SplitRatio = 0.8)

training = subset(data_diab, split_vals == TRUE)
testing = subset(data_diab, split_vals == FALSE)

model = glm(Outcome~., training, family = "binomial")
### to check if the independent variables are significant we call the glm functi
### function with other columns and if the residual deviance should not increase
### and your AIC should decrease and if both statements are then the variable removal is the correct thing to do
summary(model)

res = predict(model, testing, type="response")
table(ActualvALUE=testing$Outcome, PredValue=res>0.5)

## calculating the accuracy of a model
## formula = right diagonal / every record in the matrix
(93+29)/(93+29+18+11)

res2 = predict(model, training, type="response")
ROCRPred = prediction(res2, training$Outcome)
ROCRPref = performance(ROCRPred, "tpr", "fpr")

plot(ROCRPref, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

res3 = predict(model, testing, type="response")
table(ActualvALUE=testing$Outcome, PredValue=res3>0.3)

