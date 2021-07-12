library(ggplot2)
library(e1071)
library(caret)
library(caTools)
#transpose
library(data.table)

#limpar ambiente
rm(list=ls())

print("Naive Bayes - Evolução")

###### Leitura da base direta no CSV

train_cl1 = read.csv("~/Área de Trabalho/covid/Novo/treino_evolucao_datas.csv", 
                     sep ="," )
test_cl1 = read.csv("~/Área de Trabalho/covid/Novo/teste_evolucao_datas.csv", 
                    sep ="," )

#Idade
train_cl1$NU_IDADE_Ncat = rep("A", nrow(train_cl1))
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 20] = "B"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 40] = "C"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 60] = "D"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 80] = "E"
train_cl1$NU_IDADE_Ncat = as.factor(train_cl1$NU_IDADE_Ncat)
summary(train_cl1[,c('NU_IDADE_Ncat')])

test_cl1$NU_IDADE_Ncat = rep("A", nrow(test_cl1))
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 20] = "B"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 40] = "C"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 60] = "D"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 80] = "E"
test_cl1$NU_IDADE_Ncat = as.factor(test_cl1$NU_IDADE_Ncat)
summary(test_cl1[,c('NU_IDADE_Ncat')])

#hist(train_cl1$DIAS_INTERNACAO)
#DIAS_INTERNACAOcat
train_cl1$DIAS_INTERNACAOcat = rep("A", nrow(train_cl1))
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 3] = "B"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 6] = "C"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 9] = "D"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 12] = "E"
train_cl1$DIAS_INTERNACAOcat = as.factor(train_cl1$DIAS_INTERNACAOcat)
summary(train_cl1[,c('DIAS_INTERNACAOcat')])

test_cl1$DIAS_INTERNACAOcat = rep("A", nrow(test_cl1))
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 3] = "B"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 6] = "C"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 9] = "D"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 12] = "E"
test_cl1$DIAS_INTERNACAOcat = as.factor(test_cl1$DIAS_INTERNACAOcat)
#test_cl1$DIAS_INTERNACAO = as.factor(test_cl1$DIAS_INTERNACAO)
summary(test_cl1[,c('DIAS_INTERNACAOcat')])
#summary(test_cl1[,c('DIAS_INTERNACAO')])

summary(train_cl1$DIAS_SINTOMAS_A_INTERNAR)
#DIAS_SINTOMAS_A_INTERNARcat
train_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("A", nrow(train_cl1))
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 3] = "B"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 6] = "C"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 9] = "D"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 12] = "E"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat =  
  as.factor(train_cl1$DIAS_SINTOMAS_A_INTERNARcat)
summary(train_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])

test_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("A", nrow(test_cl1))
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 3] = "B"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 6]= "C"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 9]= "D"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 12] = "E"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat = 
  as.factor(test_cl1$DIAS_SINTOMAS_A_INTERNARcat)
summary(test_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])

summary(train_cl1$DIAS_SINTOMAS)
#DIAS_SINTOMAScat
train_cl1$DIAS_SINTOMAScat = rep("A", nrow(train_cl1))
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 6] = "B"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 12] = "C"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 18] = "D"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 24] = "E"
train_cl1$DIAS_SINTOMAScat = as.factor(train_cl1$DIAS_SINTOMAScat)
#train_cl1$DIAS_SINTOMAS = as.factor(train_cl1$DIAS_SINTOMAS)
summary(train_cl1[,c('DIAS_SINTOMAScat')])
#summary(train_cl1[,c('DIAS_SINTOMAS')])

test_cl1$DIAS_SINTOMAScat = rep("A", nrow(test_cl1))
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 6] = "B"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 12] = "C"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 18] = "D"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 24] = "E"
test_cl1$DIAS_SINTOMAScat = as.factor(test_cl1$DIAS_SINTOMAScat)
#test_cl1$DIAS_SINTOMAS = as.factor(test_cl1$DIAS_SINTOMAS)
summary(test_cl1[,c('DIAS_SINTOMAScat')])
#summary(test_cl1[,c('DIAS_SINTOMAS')])

train_cl1$SG_UF_NOT = as.factor(train_cl1$SG_UF_NOT)
train_cl1$CS_SEXO = as.factor(train_cl1$CS_SEXO)
train_cl1$CS_GESTANT = as.factor(train_cl1$CS_GESTANT)
train_cl1$CS_RACA = as.factor(train_cl1$CS_RACA)
train_cl1$CS_ESCOL_N = as.factor(train_cl1$CS_ESCOL_N)
train_cl1$NOSOCOMIAL = as.factor(train_cl1$NOSOCOMIAL)
train_cl1$FEBRE = as.factor(train_cl1$FEBRE)
train_cl1$TOSSE = as.factor(train_cl1$TOSSE)
train_cl1$GARGANTA = as.factor(train_cl1$GARGANTA)
train_cl1$DISPNEIA = as.factor(train_cl1$DISPNEIA)
train_cl1$DESC_RESP = as.factor(train_cl1$DESC_RESP)
train_cl1$SATURACAO = as.factor(train_cl1$SATURACAO)
train_cl1$DIARREIA = as.factor(train_cl1$DIARREIA)
train_cl1$VOMITO = as.factor(train_cl1$VOMITO)
train_cl1$DOR_ABD = as.factor(train_cl1$DOR_ABD)
train_cl1$FADIGA = as.factor(train_cl1$FADIGA)
train_cl1$PERD_OLFT = as.factor(train_cl1$PERD_OLFT)
train_cl1$PERD_PALA = as.factor(train_cl1$PERD_PALA)
train_cl1$PUERPERA = as.factor(train_cl1$PUERPERA)
train_cl1$CARDIOPATI = as.factor(train_cl1$CARDIOPATI)
train_cl1$HEMATOLOGI = as.factor(train_cl1$HEMATOLOGI)
train_cl1$SIND_DOWN = as.factor(train_cl1$SIND_DOWN)
train_cl1$HEPATICA = as.factor(train_cl1$HEPATICA)
train_cl1$ASMA = as.factor(train_cl1$ASMA)
train_cl1$DIABETES = as.factor(train_cl1$DIABETES)
train_cl1$NEUROLOGIC = as.factor(train_cl1$NEUROLOGIC)
train_cl1$PNEUMOPATI = as.factor(train_cl1$PNEUMOPATI)
train_cl1$IMUNODEPRE = as.factor(train_cl1$IMUNODEPRE)
train_cl1$RENAL = as.factor(train_cl1$RENAL)
train_cl1$OBESIDADE = as.factor(train_cl1$OBESIDADE)
train_cl1$VACINA = as.factor(train_cl1$VACINA)
train_cl1$ANTIVIRAL = as.factor(train_cl1$ANTIVIRAL)
train_cl1$UTI = as.factor(train_cl1$UTI)
train_cl1$SUPORT_VEN = as.factor(train_cl1$SUPORT_VEN)
train_cl1$RES_AN = as.factor(train_cl1$RES_AN)
train_cl1$PCR_RESUL = as.factor(train_cl1$PCR_RESUL)
train_cl1$CLASSI_FIN = as.factor(train_cl1$CLASSI_FIN)
train_cl1$EVOLUCAO = as.factor(train_cl1$EVOLUCAO)


test_cl1$SG_UF_NOT = as.factor(test_cl1$SG_UF_NOT)
test_cl1$CS_SEXO = as.factor(test_cl1$CS_SEXO)
test_cl1$CS_GESTANT = as.factor(test_cl1$CS_GESTANT)
test_cl1$CS_RACA = as.factor(test_cl1$CS_RACA)
test_cl1$CS_ESCOL_N = as.factor(test_cl1$CS_ESCOL_N)
test_cl1$NOSOCOMIAL = as.factor(test_cl1$NOSOCOMIAL)
test_cl1$FEBRE = as.factor(test_cl1$FEBRE)
test_cl1$TOSSE = as.factor(test_cl1$TOSSE)
test_cl1$GARGANTA = as.factor(test_cl1$GARGANTA)
test_cl1$DISPNEIA = as.factor(test_cl1$DISPNEIA)
test_cl1$DESC_RESP = as.factor(test_cl1$DESC_RESP)
test_cl1$SATURACAO = as.factor(test_cl1$SATURACAO)
test_cl1$DIARREIA = as.factor(test_cl1$DIARREIA)
test_cl1$VOMITO = as.factor(test_cl1$VOMITO)
test_cl1$DOR_ABD = as.factor(test_cl1$DOR_ABD)
test_cl1$FADIGA = as.factor(test_cl1$FADIGA)
test_cl1$PERD_OLFT = as.factor(test_cl1$PERD_OLFT)
test_cl1$PERD_PALA = as.factor(test_cl1$PERD_PALA)
test_cl1$PUERPERA = as.factor(test_cl1$PUERPERA)
test_cl1$CARDIOPATI = as.factor(test_cl1$CARDIOPATI)
test_cl1$HEMATOLOGI = as.factor(test_cl1$HEMATOLOGI)
test_cl1$SIND_DOWN = as.factor(test_cl1$SIND_DOWN)
test_cl1$HEPATICA = as.factor(test_cl1$HEPATICA)
test_cl1$ASMA = as.factor(test_cl1$ASMA)
test_cl1$DIABETES = as.factor(test_cl1$DIABETES)
test_cl1$NEUROLOGIC = as.factor(test_cl1$NEUROLOGIC)
test_cl1$PNEUMOPATI = as.factor(test_cl1$PNEUMOPATI)
test_cl1$IMUNODEPRE = as.factor(test_cl1$IMUNODEPRE)
test_cl1$RENAL = as.factor(test_cl1$RENAL)
test_cl1$OBESIDADE = as.factor(test_cl1$OBESIDADE)
test_cl1$VACINA = as.factor(test_cl1$VACINA)
test_cl1$ANTIVIRAL = as.factor(test_cl1$ANTIVIRAL)
test_cl1$UTI = as.factor(test_cl1$UTI)
test_cl1$SUPORT_VEN = as.factor(test_cl1$SUPORT_VEN)
test_cl1$RES_AN = as.factor(test_cl1$RES_AN)
test_cl1$PCR_RESUL = as.factor(test_cl1$PCR_RESUL)
test_cl1$CLASSI_FIN = as.factor(test_cl1$CLASSI_FIN)
test_cl1$EVOLUCAO = as.factor(test_cl1$EVOLUCAO)

#Evol - Class - Evol_new
#1    - Cura  - 1
#2    - Óbito - 0


train_cl1$EVOL_new = as.factor(ifelse(train_cl1$EVOLUCAO == "1", 1 , 0 ))
test_cl1$EVOL_new = as.factor(ifelse(test_cl1$EVOLUCAO == "1", 1 , 0 ))


train_cl1 = subset(train_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS, 
                                          -DIAS_SINTOMAS_A_INTERNAR, 
                                        -DIAS_INTERNACAO, -HOSPITAL, -EVOLUCAO,
                                          -X))
test_cl1 = subset(test_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS, 
                                        -DIAS_SINTOMAS_A_INTERNAR, 
                                        -DIAS_INTERNACAO, -HOSPITAL, -EVOLUCAO 
                                        , -X))

# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl1 <- naiveBayes(EVOL_new ~ ., data = train_cl1, laplace = 1)

library(ROCR)

#classifier_cl1 = train(EVOL_new ~ ., data = train_cl1, method = "nb")

classifier_cl1

# Predicting on test data'
y_pred1 <- predict(classifier_cl1, newdata = test_cl1, positive = "1")

y_pred = prediction(as.numeric(y_pred1), as.numeric(test_cl1$EVOL_new))

# Confusion Matrix
cm1 <- table(test_cl1$EVOL_new, y_pred1)
#cm1

# Model Evauation
caret::confusionMatrix(cm1, positive = "1")

#Precisão para Evolução
caret::precision(test_cl1$EVOL_new, y_pred1)

library(ROCR)

# calculo da auc (area under the curve)
auc = performance(y_pred,"auc")
unlist(auc@y.values)

performance = performance(y_pred, "tpr", "fpr")
plot(performance, col = "blue", lwd = 5)
abline(a=0, b=1, lwd =2, lty = 2)

#MODEL DIAGNOSTICS
library(InformationValue)

misClassError(as.numeric(test_cl1$EVOL_new), as.numeric(y_pred1))



