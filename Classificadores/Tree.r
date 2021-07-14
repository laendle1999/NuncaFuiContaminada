# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)
library(readxl)
library(class)
library(rpart)
library(rattle)
library(RColorBrewer)

# Loading Files
source("Classificadores/CalculaIndicadores.R")

#Loading Train and Test
train.set <- treino_evolucao1_datas
test.set  <- teste_evolucao1_datas

#Formatting
train.set$X <- NULL
test.set$X <- NULL

test.set$EVOLUCAO <- ifelse(test.set$EVOLUCAO < 2, 'Cura', 'Obito')
train.set$EVOLUCAO <- ifelse(train.set$EVOLUCAO < 2, 'Cura', 'Obito')

#Training
rpart.tree <- rpart(EVOLUCAO ~  ., data=train.set)

#Predictiong
predictions <- predict(rpart.tree, test.set, type="class")

tb = table(test.set$EVOLUCAO, predictions)

fancyRpartPlot(rpart.tree, caption = NULL)

acccFromTable(tb)
