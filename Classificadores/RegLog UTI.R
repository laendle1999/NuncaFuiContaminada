library(ggplot2)
library(e1071)
library(caret)
library(caTools)
#transpose
library(data.table)

print("Regressão logistica - UTI")

#limpar ambiente
rm(list=ls())

###### Leitura da base direta no CSV

train_cl1 = 
  read.csv("~/Área de Trabalho/covid/Novo/treino_UTI_datas.csv", sep ="," )
test_cl1 = 
  read.csv("~/Área de Trabalho/covid/Novo/teste_UTI_datas.csv", sep ="," )


#Idade
train_cl1$NU_IDADE_Ncat = rep("A", nrow(train_cl1))
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 10] = "B"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 20] = "C"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 30] = "D"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 40] = "E"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 50] = "F"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 60] = "G"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 70] = "H"
train_cl1$NU_IDADE_Ncat[train_cl1$NU_IDADE_N > 80] = "I"
train_cl1$NU_IDADE_Ncat = as.factor(train_cl1$NU_IDADE_Ncat)
#train_cl1$NU_IDADE_N = as.factor(train_cl1$NU_IDADE_N)
summary(train_cl1[,c('NU_IDADE_Ncat')])
#summary(train_cl1[,c('NU_IDADE_N')])

test_cl1$NU_IDADE_Ncat = rep("A", nrow(test_cl1))
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 10] = "B"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 20] = "C"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 30] = "D"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 40] = "E"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 50] = "F"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 60] = "G"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 70] = "H"
test_cl1$NU_IDADE_Ncat[test_cl1$NU_IDADE_N > 80] = "I"
test_cl1$NU_IDADE_Ncat = as.factor(test_cl1$NU_IDADE_Ncat)
#test_cl1$NU_IDADE_N = as.factor(test_cl1$NU_IDADE_N)
summary(test_cl1[,c('NU_IDADE_Ncat')])
#summary(test_cl1[,c('NU_IDADE_N')])


#DIAS_INTERNACAOcat
train_cl1$DIAS_INTERNACAOcat = rep("A", nrow(train_cl1))
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 2] = "B"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 4] = "C"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 6] = "D"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 8] = "E"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 10] = "F"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 12] = "G"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 14] = "H"
train_cl1$DIAS_INTERNACAOcat[train_cl1$DIAS_INTERNACAO > 16] = "I"
train_cl1$DIAS_INTERNACAOcat = as.factor(train_cl1$DIAS_INTERNACAOcat)
#train_cl1$DIAS_INTERNACAO = as.factor(train_cl1$DIAS_INTERNACAO)
summary(train_cl1[,c('DIAS_INTERNACAOcat')])
#summary(train_cl1[,c('DIAS_INTERNACAO')])

test_cl1$DIAS_INTERNACAOcat = rep("A", nrow(test_cl1))
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 2] = "B"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 4] = "C"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 6] = "D"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 8] = "E"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 10] = "F"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 12] = "G"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 14] = "H"
test_cl1$DIAS_INTERNACAOcat[test_cl1$DIAS_INTERNACAO > 16] = "I"

test_cl1$DIAS_INTERNACAOcat = as.factor(test_cl1$DIAS_INTERNACAOcat)
#test_cl1$DIAS_INTERNACAO = as.factor(test_cl1$DIAS_INTERNACAO)
summary(test_cl1[,c('DIAS_INTERNACAOcat')])
#summary(test_cl1[,c('DIAS_INTERNACAO')])

summary(train_cl1$DIAS_SINTOMAS_A_INTERNAR)
#DIAS_SINTOMAS_A_INTERNARcat
train_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("A", nrow(train_cl1))
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 2] = "B"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 4] = "C"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 6] = "D"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 8] = "E"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 10] = "F"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 12] = "G"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 14] = "H"
train_cl1$DIAS_SINTOMAS_A_INTERNARcat[train_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                      > 16] = "I"

train_cl1$DIAS_SINTOMAS_A_INTERNARcat = 
  as.factor(train_cl1$DIAS_SINTOMAS_A_INTERNARcat)

summary(train_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])
#summary(train_cl1[,c('DIAS_SINTOMAS_A_INTERNAR')])

test_cl1$DIAS_SINTOMAS_A_INTERNARcat = rep("A", nrow(test_cl1))
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 2] = "B"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 4] = "C"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 6] = "D"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 8] = "E"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 10] = "F"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 12] = "G"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 14] = "H"
test_cl1$DIAS_SINTOMAS_A_INTERNARcat[test_cl1$DIAS_SINTOMAS_A_INTERNAR 
                                     > 16] = "I"

test_cl1$DIAS_SINTOMAS_A_INTERNARcat = 
  as.factor(test_cl1$DIAS_SINTOMAS_A_INTERNARcat)

summary(test_cl1[,c('DIAS_SINTOMAS_A_INTERNARcat')])

summary(train_cl1$DIAS_SINTOMAS)
#DIAS_SINTOMAScat
train_cl1$DIAS_SINTOMAScat = rep("A", nrow(train_cl1))
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 4] = "B"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 8] = "C"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 12] = "D"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 16] = "E"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 20] = "F"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 24] = "G"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 28] = "H"
train_cl1$DIAS_SINTOMAScat[train_cl1$DIAS_SINTOMAS > 32] = "I"
train_cl1$DIAS_SINTOMAScat = as.factor(train_cl1$DIAS_SINTOMAScat)
summary(train_cl1[,c('DIAS_SINTOMAScat')])

test_cl1$DIAS_SINTOMAScat = rep("A", nrow(test_cl1))
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 4] = "B"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 8] = "C"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 12] = "D"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 16] = "E"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 20] = "F"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 24] = "G"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 28] = "H"
test_cl1$DIAS_SINTOMAScat[test_cl1$DIAS_SINTOMAS > 32] = "I"
test_cl1$DIAS_SINTOMAScat = as.factor(test_cl1$DIAS_SINTOMAScat)
summary(test_cl1[,c('DIAS_SINTOMAScat')])

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

#UTI - Class - Evol_new
#1    - Sim  - 1
#2    - Não - 0

train_cl1$EVOL_new = ifelse(train_cl1$UTI == "1", 1 , 0 )
test_cl1$EVOL_new = ifelse(test_cl1$UTI == "1", 1 , 0 )


train_cl1 = subset(train_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS,
                                         -DIAS_SINTOMAS_A_INTERNAR, 
                                         -DIAS_INTERNACAO, -HOSPITAL, -X, -UTI))
test_cl1 = subset(test_cl1, select = c( -NU_IDADE_N, -DIAS_SINTOMAS, 
                                       -DIAS_SINTOMAS_A_INTERNAR, 
                                       -DIAS_INTERNACAO, -HOSPITAL, -X, -UTI ))


summary(train_cl1)

########################################################
#Regressão Logistica

library(kernlab)
library(ROCR)

sapply(train_cl1, function(x) sum(is.na(x)))

#tentatica de melhorar o modelo
#remover items conforme execução de stepwise backward (VOMITO - AIC: 
#444685,1, PUERPERA - AIC: 444686, 
#CARDIOPATI - AIC: 44688,3) E com 
#alta multicolineariedade (DIAS_INTERNACAOcat,  DIAS_SINTOMAScat  )

# train_cl1 = subset(train_cl1, select = c(-CS_GESTANT, -VOMITO, -PUERPERA,
#                                          -CARDIOPATI ,
#                                         -DIAS_INTERNACAOcat, -DIAS_SINTOMAScat
#                                          ))
# test_cl1 = subset(test_cl1, select = c(-CS_GESTANT, -VOMITO, -PUERPERA,
#                                        -CARDIOPATI ,
#                                        -DIAS_INTERNACAOcat, -DIAS_SINTOMAScat
# ))

train_cl1 = subset(train_cl1, select = c(-DIAS_INTERNACAOcat, -DIAS_SINTOMAScat))
test_cl1 = subset(test_cl1, select = c(-DIAS_INTERNACAOcat, -DIAS_SINTOMAScat))

#####Evolução

#https://smolski.github.io/livroavancado/reglog.html
#https://estatsite.com.br/2018/08/26/regressao-logistica-no-r/
#https://www.edureka.co/blog/logistic-regression-in-r/

modelo = glm(EVOL_new ~., data = train_cl1, family = binomial)

summary(modelo)

odd.ratio = exp(coef(modelo))
print(odd.ratio)

print("Percentual de importancia comparativa para cura entre fatores")
(odd.ratio - 1) * 100

coef(modelo)

step = step(modelo, direction = "backward")

#caluclo da razão das chances
#library(mfx)

#odds = logitor(EVOL_new ~., data = train_cl1)

#odd1 = as.data.frame(odds$oddsratio)

#odd1$prop = (odd1$OddsRatio-1)*100

#exp(cbind(OR=coef(modelo), confint(modelo)))

#a = c(rownames(odd1))


#anova(modelo)


#Calculo do McFadden R2 (valores acima de 0.4 
#indicam que o modelo fit bem)

pscl::pR2(modelo)["McFadden"]

#Importancia das variaveis 
X = caret::varImp(modelo)
print(X)

#valores de colineariedade - acima de 5 grande colinearidade 
car::vif(modelo)

pred.Teste = predict(modelo, test_cl1, type = "response")

Teste_v2 = cbind(test_cl1, pred.Teste)

pred.val = prediction(pred.Teste, Teste_v2$EVOL_new)

# calculo da auc (area under the curve)
auc = performance(pred.val,"auc")
unlist(auc@y.values)

# Plota curva ROC
performance = performance(pred.val, "tpr", "fpr")
plot(performance, col = "blue", lwd = 5)
abline(a=0, b=1, lwd =2, lty = 2)

#Calculo Estatística KS
ks <- max(attr(performance, "y.values")[[1]] - 
            (attr(performance, "x.values")[[1]]))
ks

test_cl1$predito = ifelse(pred.Teste > 0.5, 1 , 0)

test_cl1$predito = as.factor(test_cl1$predito)
test_cl1$EVOL_new = as.factor(test_cl1$EVOL_new)

table(test_cl1$EVOL_new, test_cl1$predito)


#pred.Teste
#test_cl1$EVOL_new

require(caret)

caret::confusionMatrix(test_cl1$EVOL_new, test_cl1$predito, positive = "1")


#calculos do cutoff
cutoffs <- seq(0.1, 0.9, 0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(modelo$fitted.values >= cutoffs[i], 1, 0) 
  accuracy <- c(accuracy,length(which(train_cl1$EVOL_new == 
                                        prediction))/length(prediction)*100) }

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

# https://www.statology.org/logistic-regression-in-r/

#valor numérico
library(InformationValue)
optimal <- optimalCutoff(test_cl1$EVOL_new, pred.Teste)
optimal

#calculate sensitivity - True positive rate
sensitivity(test_cl1$EVOL_new, pred.Teste)


#calculate specificity - true negative rate
specificity(test_cl1$EVOL_new, pred.Teste)


#calculate total misclassification error rate - Incorrect classification
misClassError(test_cl1$EVOL_new, pred.Teste, threshold=optimal)

