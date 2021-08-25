library(tidyverse)
library(caTools)
library(rpart)
library(caret)
car_purchases = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/suv_data.csv", 
                                col_types = cols())

car_puchas = 
  car_purchases %>% select(-1)

car_puchas$Purchased[which(car_puchas$Purchased == 1)] = "YES"
car_puchas$Purchased[which(car_puchas$Purchased == 0)] = "NO"


split_values = sample.split(car_puchas$Purchased, SplitRatio = 0.65)
train_set = subset(car_puchas, split_values == T)
test_set = subset(car_puchas, split_values == F)

rpart(Purchased~.,data = train_set) -> mod_class

predict(mod_class, test_set, type= "class") -> result_class

table(test_set$Purchased, result_class)

### Regression

View(diamonds)

split_values = sample.split(diamonds$price, SplitRatio = 0.65)
train_reg = subset(diamonds, split_values == T)
test_reg = subset(diamonds, split_values == F)

mod_reg = lm(price~., data = train_reg)
result_reg = predict(mod_reg, test_reg)
finalData = as_tibble(cbind(Actual = diamonds$price, Predicted = result_reg))

## finding the error

error = (finalData$Actual - finalData$Predicted)
finalData = cbind(finalData, error)
rmse = sqrt(mean(finalData$error^2))
rmse

##clustering

iris_k = iris[1:4]
iris_k = as.matrix(iris_k)
iris_clust = kmeans(iris_k, 3)
cluster_data = cbind(iris, iris_clust$cluster)


## Pokemon case study

pokemon = read_csv("/Users/crispenchisina/Desktop/R practise/Machine Learning/datasets/pokemon.csv", 
                         col_types = cols())

pokemon = pokemon %>% select(-1)
colnames(pokemon)[which(colnames(pokemon) == "type1")] = "Primary_Type"
colnames(pokemon)[which(colnames(pokemon) == "type2")] = "Secondary_Type"
colnames(pokemon)[which(colnames(pokemon) == "hp")] = "Health_Points"
colnames(pokemon)[which(colnames(pokemon) == "sp_attack")] = "Special_Attack"
colnames(pokemon)[which(colnames(pokemon) == "sp_defense")] = "Special_Defence"

str(pokemon)

pokemon$is_legendary[which(pokemon$is_legendary == 1)] = "TRUE"
pokemon$is_legendary[which(pokemon$is_legendary == 0)] = "FALSE"

pokemon$is_legendary = as.factor(pokemon$is_legendary)
table(pokemon$Primary_Type)

# Selecting Grass Pokemon

Grass_pokemon = pokemon %>% filter(Primary_Type == "grass")
Grass_Poison_pokemon = Grass_pokemon %>% filter(Secondary_Type == "poison")
range(Grass_Poison_pokemon$speed)
My_Grass_Pokemon = Grass_Poison_pokemon %>% filter(speed==90)

# Selecting Water Pokemon

Water_pokemon = pokemon %>% filter(Primary_Type == "water")
Water_Psychic_pokemon = Grass_pokemon %>% filter(Secondary_Type == "psychic")
range(Water_Psychic_pokemon$defense)
My_water_Pokemon = Water_Psychic_pokemon %>% filter(defense==85)

# Selecting Fire Pokemon

Fire_pokemon = pokemon %>% filter(Primary_Type == "fire")
Fire_Fighting_pokemon = Fire_pokemon %>% filter(Secondary_Type == "fighting")
range(Fire_Fighting_pokemon$attack)
My_fire_Pokemon = Fire_Fighting_pokemon %>% filter(attack==160)

My_Pokemons = rbind(My_Grass_Pokemon, My_water_Pokemon, My_fire_Pokemon)

# splitting data

# understanding what influences a pokemon's attack
split_index = sample.split(pokemon$attack, SplitRatio = 0.65)
train1 = subset(pokemon, split_index == T)
test1 = subset(pokemon, split_index == F)


mod_progress = lm(attack~defense,data = train1)
result_reg = predict(mod_progress, test1)
final_data = cbind(Actual = test1$attack, Predicted = result_reg)
final_data = as_tibble(final_data)

# Error

error = (final_data$Actual - final_data$Predicted)
final_data = cbind(final_data, error)
rmse1 = sqrt(mean(final_data$error^2))
rmse1

## model 2 (attack relation to defence + speed and health points)

mod_progress2 = lm(attack~defense+speed+Health_Points,data = train1)
result_reg2 = predict(mod_progress2, test1)
final_data2 = cbind(Actual = test1$attack, Predicted = result_reg2)
final_data2 = as_tibble(final_data2)

# Error

error2 = (final_data2$Actual - final_data2$Predicted)
final_data2 = cbind(final_data2, error2)
rmse2 = sqrt(mean(final_data2$error^2))
rmse2

# classification to see if it is legendary or not

split_values = sample.split(pokemon$is_legendary, SplitRatio = 0.65)
train_data = subset(pokemon, split_values == T)
test_data = subset(pokemon, split_values == F)

train_data$is_legendary = as.factor(train_data$is_legendary)
test_data$is_legendary = as.factor(test_data$is_legendary)


nrow(train_data)
nrow(test_data)

mod1 = rpart(is_legendary~.,data = train_data)
result1  = predict(mod1, test_data, type= "class")
table(test_data$is_legendary, result1)


# Build model 2

mod2 = rpart(is_legendary~defense+speed+Health_Points,data = train_data)
result2  = predict(mod2, test_data, type= "class")
table(test_data$is_legendary, result2)

confusionMatrix(table(test_data$is_legendary, result2))


