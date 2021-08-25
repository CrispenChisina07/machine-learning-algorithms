library(tidyverse)
library(caTools)
library(rpart)
library(caret)
library(lubridate)
library(e1071)

emp_data = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/HRDataset_v14.csv", 
                         col_types = cols())

years_diff <- function(dob, age.day = today(), units = "years", floor = TRUE) {
            calc.age = interval(dob, age.day) / duration(num = 1, units = units)
            if (floor) return(as.integer(floor(calc.age)))
            return(calc.age)
          }

emp_data$DOB = mdy(sub("/(..$)", "/19\\1",emp_data$DOB))
emp_data$DateofHire =  mdy(emp_data$DateofHire)

emp_data = emp_data %>%
            mutate(emp_age = years_diff(DOB))%>%
            mutate(years.exp = years_diff(DateofHire))

sal = ifelse(emp_data$Salary >= 50000, "High", "Low")
emp_data = tibble(emp_data, sal)
emp_data = emp_data[,-10]

set.seed(2)

id = sample(2, nrow(emp_data), prob = c(0.7,0.3), replace = TRUE)
emp_train = emp_data[id==1,]
emp_test = emp_data[id==2,]

emp_data$sal = as.factor(emp_data$sal)
emp_train$sal = as.factor(emp_train$sal)
emp_test$sal = as.factor(emp_test$sal)

### Naive Bayes

emp_nb = naiveBayes(sal~., emp_train)

emp_nb = naiveBayes(sal ~ Position + PerformanceScore + Absences + emp_age + years.exp + SpecialProjectsCount + Department, emp_train)
emp_pred = predict(emp_nb, emp_test)

confusionMatrix(table(emp_pred, emp_test$sal))


