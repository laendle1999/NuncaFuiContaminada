library(ggplot2)
library(e1071)
library(caret)
library(caTools)
#transpose
library(data.table)
library(RSNNS)

library(clusterGeneration)
library(nnet)

#limpar ambiente
rm(list=ls())

print("MLP - Evolução")

###### Leitura da base direta no CSV

train_cl1 = read.csv("~/Área de Trabalho/covid/Novo/MLP/out_evolucao_datas.csv", 
                     sep ="," )
#test_cl1 = read.csv("~/Área de Trabalho/covid/Novo/teste_evolucao_datas.csv", 
#                    sep ="," )


#Idade
train_cl1$NU_IDADE_Ncat = rep("1", nrow(train_cl1))
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 20] = "2"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 40] = "3"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 60] = "4"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 80] = "5"
train_cl1$NU_IDADE_Ncat = as.factor(train_cl1$NU_IDADE_Ncat)
summary(train_cl1[,c('NU_IDADE_Ncat')])

# """
# 
# test_cl1$NU_IDADE_Ncat = rep("1", nrow(test_cl1))
# test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 20] = "2"
# test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 40] = "3"
# test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 60] = "4"
# test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 80] = "5"
# test_cl1$NU_IDADE_Ncat = as.factor(test_cl1$NU_IDADE_Ncat)
# summary(test_cl1[,c('NU_IDADE_Ncat')])
# """

hist(train_cl1$DIAS_INTERNACAO)
#DIAS_INTERNACAOcat
train_cl1$DIAS_INTERNACAOcat = rep("1", nrow(train_cl1))
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 3] = "2"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 6] = "3"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 9] = "3"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 12] = "5"
train_cl1$DIAS_INTERNACAOcat = as.factor(train_cl1$DIAS_INTERNACAOcat)-DT_NOTIFIC
summary(train_cl1[,c('DIAS_INTERNACAOcat')])

# """
# test_cl1$DIAS_INTERNACAOcat = rep("1", nrow(test_cl1))
# test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 3] = "2"
# test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 6] = "3"
# test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 9] = "4"
# test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 12] = "5"
# test_cl1$DIAS_INTERNACAOcat = as.factor(test_cl1$DIAS_INTERNACAOcat)
# #test_cl1$DIAS_INTERNACAO = as.factor(test_cl1$DIAS_INTERNACAO)
# summary(test_cl1[,c('DIAS_INTERNACAOcat')])
# #summary(test_cl1[,c('DIAS_INTERNACAO')])
# """

summary(train_cl1$DIAS_SINTOMAS_A_INTERNAR)
#DIAS_SINTOMAS_A_INTERNARcat
train_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("1", nrow(train_cl1))
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 3] = "2"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 6] = "3"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 9] = "4"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 12] = "5"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat =  
  as.factor(train_cl1$DIAS_SINTOMAS_A_INTERNARcat)
summary(train_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])

# """
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("1", nrow(test_cl1))
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
# #                                     > 3] = "2"
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
# #                                     > 6]= "3"
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
# #                                     > 9]= "4"
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
# #                                     > 12] = "5"
# test_cl1$DIAS_SINTOMAS_A_INTERNARcat = 
#   as.factor(test_cl1$DIAS_SINTOMAS_A_INTERNARcat)
# summary(test_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])
# 
# """

summary(train_cl1$DIAS_SINTOMAS)
#DIAS_SINTOMAScat
train_cl1$DIAS_SINTOMAScat = rep("1", nrow(train_cl1))
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 6] = "2"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 12] = "3"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 18] = "4"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 24] = "5"
train_cl1$DIAS_SINTOMAScat = as.factor(train_cl1$DIAS_SINTOMAScat)
#train_cl1$DIAS_SINTOMAS = as.factor(train_cl1$DIAS_SINTOMAS)
summary(train_cl1[,c('DIAS_SINTOMAScat')])
#summary(train_cl1[,c('DIAS_SINTOMAS')])

# """
# test_cl1$DIAS_SINTOMAScat = rep("1", nrow(test_cl1))
# test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 6] = "2"
# test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 12] = "3"
# test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 18] = "4"
# test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 24] = "5"
# test_cl1$DIAS_SINTOMAScat = as.factor(test_cl1$DIAS_SINTOMAScat)
# #test_cl1$DIAS_SINTOMAS = as.factor(test_cl1$DIAS_SINTOMAS)
# summary(test_cl1[,c('DIAS_SINTOMAScat')])
# #summary(test_cl1[,c('DIAS_SINTOMAS')])
# """


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

# """
# test_cl1$SG_UF_NOT = as.factor(test_cl1$SG_UF_NOT)
# test_cl1$CS_SEXO = as.factor(test_cl1$CS_SEXO)
# test_cl1$CS_GESTANT = as.factor(test_cl1$CS_GESTANT)
# test_cl1$CS_RACA = as.factor(test_cl1$CS_RACA)
# test_cl1$CS_ESCOL_N = as.factor(test_cl1$CS_ESCOL_N)
# test_cl1$NOSOCOMIAL = as.factor(test_cl1$NOSOCOMIAL)
# test_cl1$FEBRE = as.factor(test_cl1$FEBRE)
# test_cl1$TOSSE = as.factor(test_cl1$TOSSE)
# test_cl1$GARGANTA = as.factor(test_cl1$GARGANTA)
# test_cl1$DISPNEIA = as.factor(test_cl1$DISPNEIA)
# test_cl1$DESC_RESP = as.factor(test_cl1$DESC_RESP)
# test_cl1$SATURACAO = as.factor(test_cl1$SATURACAO)
# test_cl1$DIARREIA = as.factor(test_cl1$DIARREIA)
# test_cl1$VOMITO = as.factor(test_cl1$VOMITO)
# test_cl1$DOR_ABD = as.factor(test_cl1$DOR_ABD)
# test_cl1$FADIGA = as.factor(test_cl1$FADIGA)
# test_cl1$PERD_OLFT = as.factor(test_cl1$PERD_OLFT)
# test_cl1$PERD_PALA = as.factor(test_cl1$PERD_PALA)
# test_cl1$PUERPERA = as.factor(test_cl1$PUERPERA)
# test_cl1$CARDIOPATI = as.factor(test_cl1$CARDIOPATI)
# test_cl1$HEMATOLOGI = as.factor(test_cl1$HEMATOLOGI)
# test_cl1$SIND_DOWN = as.factor(test_cl1$SIND_DOWN)
# test_cl1$HEPATICA = as.factor(test_cl1$HEPATICA)
# test_cl1$ASMA = as.factor(test_cl1$ASMA)
# test_cl1$DIABETES = as.factor(test_cl1$DIABETES)
# test_cl1$NEUROLOGIC = as.factor(test_cl1$NEUROLOGIC)
# test_cl1$PNEUMOPATI = as.factor(test_cl1$PNEUMOPATI)
# test_cl1$IMUNODEPRE = as.factor(test_cl1$IMUNODEPRE)
# test_cl1$RENAL = as.factor(test_cl1$RENAL)
# test_cl1$OBESIDADE = as.factor(test_cl1$OBESIDADE)
# test_cl1$VACINA = as.factor(test_cl1$VACINA)
# test_cl1$ANTIVIRAL = as.factor(test_cl1$ANTIVIRAL)
# test_cl1$UTI = as.factor(test_cl1$UTI)
# test_cl1$SUPORT_VEN = as.factor(test_cl1$SUPORT_VEN)
# test_cl1$RES_AN = as.factor(test_cl1$RES_AN)
# test_cl1$PCR_RESUL = as.factor(test_cl1$PCR_RESUL)
# test_cl1$CLASSI_FIN = as.factor(test_cl1$CLASSI_FIN)
# test_cl1$EVOLUCAO = as.factor(test_cl1$EVOLUCAO)
# """

train_cl1$EVOLUCAO = ifelse(train_cl1$EVOLUCAO == "1", 1 , 0 )
#test_cl1$EVOLUCAO = ifelse(test_cl1$EVOLUCAO == "1", 1 , 0 )


#removi sexo, estado e data de notificação pq são valores não numéricos
#necessario converter sexo e estado.

train_cl1 = subset(train_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS, 
                                          -DIAS_SINTOMAS_A_INTERNAR, 
                                          -DIAS_INTERNACAO, -HOSPITAL, -X, 
                                          -SG_UF_NOT, -CS_SEXO, -DT_NOTIFIC))



# """
# test_cl1 = subset(test_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS, 
#                                         -DIAS_SINTOMAS_A_INTERNAR, 
#                                         -DIAS_INTERNACAO, -HOSPITAL, -X, -SG_UF_NOT, -CS_SEXO, -DT_NOTIFIC))
# """

train_cl1 = data.matrix(train_cl1)

summary(train_cl1)

train_cl1Values = subset(train_cl1, select = c(-EVOLUCAO))
#train_cl1Values = data.matrix(train_cl1Values)
#train_cl1target = decodeClassLabels(train_cl1[,36], valTrue=1, valFalse=0)
train_cl1target = decodeClassLabels(train_cl1[,36])
#train_cl1target = data.matrix(train_cl1target)

#train_cl1Values = normalizeData(train_cl1Values, type = "0_1")
#train_cl1target = normalizeData(train_cl1target, type = "0_1")

train_cl1 = splitForTrainingAndTest(train_cl1Values, train_cl1target, ratio = 0.3)

train_cl1 = normTrainingAndTestSet(train_cl1)


#test_cl1Values = subset(test_cl1, select = c(-EVOLUCAO))
#test_cl1Values = data.matrix((test_cl1Values))
#test_cl1target = decodeClassLabels((test_cl1[,36]))
#test_cl1target = data.matrix(test_cl1target)

#test_cl1Values = normalizeData(train_cl1Values, type = "0_1")
#test_cl1target = normalizeData(train_cl1target, type = "0_1")


model = mlp(train_cl1$inputsTrain, train_cl1$targetsTrain, size = 20, 
            learnFuncParams = c(0.1), maxit = 20, inputsTest = train_cl1$inputsTest, 
            targetsTest = train_cl1$targetsTest)

summary(model)
model
weightMatrix(model)

par(mfrow=c(1,1))
plotIterativeError(model)

prediction = predict(model, train_cl1$inputsTest)

par(mfrow=c(1,1))
plotRegressionError(prediction[,2], train_cl1$targetsTest[,2])

summary(train_cl1target[,2])
summary(fitted.values(model)[,2])

summary(prediction)

cm1 = table(train_cl1$targetsTrain, prediction)

confusionMatrix(cm1)

cm1 = confusionMatrix(train_cl1$targetsTrain, fitted.values(model)  )

cm2 = confusionMatrix(train_cl1$targetsTest, prediction )

accc = function(TP,TN,FN, FP){
  print("acuracia - indica exatidão do valor correto")
  print(((TP + TN)/(TP + TN + FN + FP)) * 100)
  print("Precisão - indica os Falso-positivo ")
  print((TP)/(TP+FP) * 100)
  print("Recal - Sensibilidade: Falso negativo ")
  print((TP)/(TP+FN) * 100)
  print("F1 Score - média harmonica da precisão e da sensibilidade")
  print((2*TP)/(2*TP+FP+FN) * 100)
  print("Especificidade - Capacidade de acertar o negativo entre os negativos")
  print((TN)/(FP+TN) * 100)
  
  
}

cm1[2,1]

print("TBase de treinamento: 70%")
accc(cm1[1,1], cm1[2,2], cm1[1,2], cm1[2,1]  )

print("Base de teste: 30%")
accc(cm2[1,1], cm2[2,2], cm2[1,2], cm2[2,1]  )

#demora muuito para rodar.. não consegui rodar no meu pc
plotROC(fitted.values(model)[,2], train_cl1$targetsTrain[,2])
plotROC(prediction[,2], train_cl1$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(train_cl1$targetsTrain, encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))
# }

print(cm1)





