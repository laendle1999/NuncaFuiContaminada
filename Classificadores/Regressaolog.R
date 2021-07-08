library(ggplot2)
library(e1071)
library(caret)
library(caTools)
#transpose
library(data.table)

#limpar ambiente
rm(list=ls())

EVOLUCAO = read.csv("~/Área de Trabalho/covid/Novo/out_evolucao_datas.csv", sep ="," )

summary(EVOLUCAO)

sapply(EVOLUCAO, function(x) sum(is.na(x)))

#

out_evolucao$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y") - as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))





EVOLUCAO$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(EVOLUCAO$DT_EVOLUCA), format="%d/%m/%Y") - as.Date(as.character(EVOLUCAO$DT_INTERNA), format="%d/%m/%Y"))
EVOLUCAO$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(EVOLUCAO$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(EVOLUCAO$DT_SIN_PRI), format="%d/%m/%Y"))
EVOLUCAO$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(EVOLUCAO$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(EVOLUCAO$DT_SIN_PRI), format="%d/%m/%Y"))

EVOLUCAO$DT_TOTAL = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_SIN_PRI , units = "days"))
EVOLUCAO$DT_INTERNADO = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_INTERNA , units = "days"))

###### Leitura da base direta no CSV

train_cl1 = read.csv("~/Área de Trabalho/covid/Novo/treino_evolucao_datas.csv", sep ="," )
test_cl1 = read.csv("~/Área de Trabalho/covid/Novo/teste_evolucao_datas.csv", sep ="," )

summary(EVOLUCAO$DIAS_INTERNACAO)

EVOLUCAO$NU_IDADE_Ncat = rep("A", nrow(EVOLUCAO))
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 20] = "B"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 40] = "C"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 60] = "D"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 80] = "E"
EVOLUCAO$NU_IDADE_Ncat = as.factor(EVOLUCAO$NU_IDADE_Ncat)
#EVOLUCAO$NU_IDADE_N = as.factor(EVOLUCAO$NU_IDADE_N)
summary(EVOLUCAO[,c('NU_IDADE_Ncat')])
#summary(EVOLUCAO[,c('NU_IDADE_N')])


EVOLUCAO$DT_NOTIFIC = as.Date(EVOLUCAO$DT_NOTIFIC, format("%d/%m/%Y"))
EVOLUCAO$DT_INTERNA = as.Date(EVOLUCAO$DT_INTERNA, format("%d/%m/%Y"))
#EVOLUCAO$DT_ENTUTI = as.Date(EVOLUCAO$DT_ENTUTI, format("%d/%m/%Y"))
#EVOLUCAO$DT_SAIDUTI = as.Date(EVOLUCAO$DT_SAIDUTI, format("%d/%m/%Y"))
EVOLUCAO$DT_EVOLUCA = as.Date(EVOLUCAO$DT_EVOLUCA, format("%d/%m/%Y"))
EVOLUCAO$DT_SIN_PRI = as.Date(EVOLUCAO$DT_SIN_PRI, format("%d/%m/%Y"))

#Calculos das diferenças do tempo

#EVOLUCAO$DT_TOTAL = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_SIN_PRI , units = "days"))
#EVOLUCAO$DT_INTERNADO = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_INTERNA , units = "days"))
#EVOLUCAO$DT_UTI = as.numeric(difftime(EVOLUCAO$DT_SAIDUTI, EVOLUCAO$DT_ENTUTI, units = "days"))


#Discretização tempo internado (delta de 3 dias)

#EVOLUCAO$DT_INTERNADO_Ncat = rep("A", nrow(EVOLUCAO))
#EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 3] = "B"
#EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 6] = "C"
#EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 9] = "D"
#EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 12] = "E"
#EVOLUCAO$DT_INTERNADO_Ncat = as.factor(EVOLUCAO$DT_INTERNADO_Ncat)
#summary(EVOLUCAO[,c('DT_INTERNADO_Ncat')])

#ggplot(data.frame(EVOLUCAO$DT_INTERNADO_Ncat), aes(EVOLUCAO$DT_INTERNADO_Ncat)) + geom_bar()



#Discretização tempo total (delta de 6 dias)

#EVOLUCAO$DT_TOTAL_Ncat = rep("A", nrow(EVOLUCAO))
#EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 6] = "B"
#EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 12] = "C"
#EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 18] = "D"
#EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$)DT_TOTAL > 24] = "E"
#EVOLUCAO$DT_TOTAL_Ncat = as.factor(EVOLUCAO$DT_TOTAL_Ncat)
#summary(EVOLUCAO[,c('DT_TOTAL_Ncat')])

#ggplot(data.frame(EVOLUCAO$DT_TOTAL_Ncat), aes(EVOLUCAO$DT_TOTAL_Ncat)) + geom_bar()


#ggplot(data.frame(UTI$DT_TOTAL_Ncat), aes(UTI$DT_TOTAL_Ncat)) +  geom_bar()

#Discretização tempo UTI (delta de 3 dias)

#EVOLUCAO$DT_UTI_Ncat = rep("A", nrow(EVOLUCAO))
#EVOLUCAO$DT_UTI_Ncat[EVOLUCAO$DT_UTI >= 0] = "B"
#EVOLUCAO$DT_UTI_Ncat[EVOLUCAO$DT_UTI > 1] = "C"
#EVOLUCAO$DT_UTI_Ncat[EVOLUCAO$DT_UTI > 2] = "D"
#EVOLUCAO$DT_UTI_Ncat[EVOLUCAO$DT_UTI > 3] = "E"
#EVOLUCAO$DT_UTI_Ncat = as.factor(EVOLUCAO$DT_UTI_Ncat)
#summary(EVOLUCAO[,c('DT_UTI_Ncat')])

#remover colunas de tempo apois tratametno

EVOLUCAO1 = subset(EVOLUCAO, select = c(-DT_NOTIFIC, - NU_IDADE_N  ,  -DT_INTERNA, -DT_EVOLUCA, -DT_SIN_PRI ))

# Converter para fator

EVOLUCAO1$SG_UF_NOT = as.factor(EVOLUCAO1$SG_UF_NOT)
EVOLUCAO1$CS_SEXO = as.factor(EVOLUCAO1$CS_SEXO)
EVOLUCAO1$CS_GESTANT = as.factor(EVOLUCAO1$CS_GESTANT)
EVOLUCAO1$CS_RACA = as.factor(EVOLUCAO1$CS_RACA)
EVOLUCAO1$CS_ESCOL_N = as.factor(EVOLUCAO1$CS_ESCOL_N)
#EVOLUCAO1$SURTO_SG = as.factor(EVOLUCAO1$SURTO_SG)
EVOLUCAO1$NOSOCOMIAL = as.factor(EVOLUCAO1$NOSOCOMIAL)
EVOLUCAO1$FEBRE = as.factor(EVOLUCAO1$FEBRE)
EVOLUCAO1$TOSSE = as.factor(EVOLUCAO1$TOSSE)
EVOLUCAO1$GARGANTA = as.factor(EVOLUCAO1$GARGANTA)
EVOLUCAO1$DISPNEIA = as.factor(EVOLUCAO1$DISPNEIA)
EVOLUCAO1$DESC_RESP = as.factor(EVOLUCAO1$DESC_RESP)
EVOLUCAO1$SATURACAO = as.factor(EVOLUCAO1$SATURACAO)
EVOLUCAO1$DIARREIA = as.factor(EVOLUCAO1$DISPNEIA)
EVOLUCAO1$VOMITO = as.factor(EVOLUCAO1$VOMITO)
EVOLUCAO1$DOR_ABD = as.factor(EVOLUCAO1$DOR_ABD)
EVOLUCAO1$FADIGA = as.factor(EVOLUCAO1$FADIGA)
EVOLUCAO1$PERD_OLFT = as.factor(EVOLUCAO1$PERD_OLFT)
EVOLUCAO1$PERD_PALA = as.factor(EVOLUCAO1$PERD_PALA)
EVOLUCAO1$PUERPERA = as.factor(EVOLUCAO1$PUERPERA)
EVOLUCAO1$CARDIOPATI = as.factor(EVOLUCAO1$CARDIOPATI)
EVOLUCAO1$HEMATOLOGI = as.factor(EVOLUCAO1$HEMATOLOGI)
EVOLUCAO1$SIND_DOWN = as.factor(EVOLUCAO1$SIND_DOWN)
EVOLUCAO1$HEPATICA = as.factor(EVOLUCAO1$HEPATICA)
EVOLUCAO1$ASMA = as.factor(EVOLUCAO1$ASMA)
EVOLUCAO1$DIABETES = as.factor(EVOLUCAO1$DIABETES)
EVOLUCAO1$NEUROLOGIC = as.factor(EVOLUCAO1$NEUROLOGIC)
EVOLUCAO1$PNEUMOPATI = as.factor(EVOLUCAO1$PNEUMOPATI)
EVOLUCAO1$IMUNODEPRE = as.factor(EVOLUCAO1$IMUNODEPRE)
EVOLUCAO1$RENAL = as.factor(EVOLUCAO1$RENAL)
EVOLUCAO1$OBESIDADE = as.factor(EVOLUCAO1$OBESIDADE)
EVOLUCAO1$VACINA = as.factor(EVOLUCAO1$VACINA)
EVOLUCAO1$ANTIVIRAL = as.factor(EVOLUCAO1$ANTIVIRAL)
EVOLUCAO1$HOSPITAL = as.factor(EVOLUCAO1$HOSPITAL)
EVOLUCAO1$UTI = as.factor(EVOLUCAO1$UTI)
EVOLUCAO1$SUPORT_VEN = as.factor(EVOLUCAO1$SUPORT_VEN)
EVOLUCAO1$RES_AN = as.factor(EVOLUCAO1$RES_AN)
#EVOLUCAO1$RES_IGG = as.factor(EVOLUCAO1$RES_IGG)
#EVOLUCAO1$RES_IGM = as.factor(EVOLUCAO1$RES_IGM)
EVOLUCAO1$PCR_RESUL = as.factor(EVOLUCAO1$PCR_RESUL)
EVOLUCAO1$CLASSI_FIN = as.factor(EVOLUCAO1$CLASSI_FIN)
EVOLUCAO1$EVOLUCAO = as.factor(EVOLUCAO1$EVOLUCAO)
EVOLUCAO1$DIAS_INTERNACAO = as.factor(EVOLUCAO1$DIAS_INTERNACAO)
EVOLUCAO1$DIAS_INTERNACAO = as.factor(EVOLUCAO1$DIAS_SINTOMAS)
EVOLUCAO1$DIAS_SINTOMAS_A_INTERNAR = as.factor(EVOLUCAO1$DIAS_SINTOMAS_A_INTERNAR)
EVOLUCAO1$EVOLUCAO = as.factor(EVOLUCAO1$EVOLUCAO)

EVOLUCAO1$DIAS_INTERNACAO = as.factor(EVOLUCAO1$DIAS_INTERNACAO)





#limpando memória para não travar

rm(EVOLUCAO)

####################### Treinamento do Naives Bayes - Evolução

# Splitting data into train
# and test data
EVOLUCAO1$spl <- sample.split(transpose(EVOLUCAO1), SplitRatio=0.7)
train_cl1 <- subset(EVOLUCAO1, EVOLUCAO1$spl == "TRUE")
test_cl1 <- subset(EVOLUCAO1, EVOLUCAO1$spl == "FALSE")

train_cl1 <- subset(train_cl1, select =c(-spl, -EVOLUCAO))
test_cl1 <- subset(test_cl1, select =c(-spl, -EVOLUCAO))



#limpando memória para não travar

rm(EVOLUCAO1)

########################################################
#Regressão Logistica

library(kernlab)
library(ROCR)

sapply(train_cl1, function(x) sum(is.na(x)))



#####Evolução

#modelo = glm(EVOL_new ~ SG_UF_NOT + CS_SEXO + CS_GESTANT + CS_RACA + FEBRE + TOSSE + GARGANTA + 
#               DISPNEIA + DESC_RESP + SATURACAO + DIARREIA  + VOMITO + DOR_ABD + FADIGA + 
#               PERD_PALA  + PERD_OLFT +
#               NU_IDADE_Ncat + DT_INTERNADO_Ncat + DT_TOTAL_Ncat + PUERPERA + 
#               CARDIOPATI + HEMATOLOGI + SIND_DOWN + HEPATICA + ASMA + DIABETES + 
#               NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL + OBESIDADE, 
#             data = train_cl1, family = binomial)


modelo = glm(EVOL_new ~ SG_UF_NOT + CS_SEXO + CS_GESTANT + CS_RACA + FEBRE + TOSSE + GARGANTA + 
               DISPNEIA + DESC_RESP + SATURACAO + DIARREIA  + VOMITO + DOR_ABD + FADIGA + 
               PERD_PALA  + PERD_OLFT +
               NU_IDADE_Ncat + DIAS_INTERNACAO + DIAS_SINTOMAS_A_INTERNAR + DIAS_SINTOMAS + PUERPERA + 
               CARDIOPATI + HEMATOLOGI + SIND_DOWN + HEPATICA + ASMA + DIABETES + 
               NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL + OBESIDADE, 
             data = train_cl1, family = binomial)


summary(modelo)$coefficients

odd.ratio = exp(coef(modelo))

pred.Teste = predict(modelo, test_cl1, type = "response")
Teste_v2 = cbind(test_cl1, pred.Teste)


Teste_v2$EVOL_new

pred.val = prediction(pred.Teste, Teste_v2$EVOL_new)

# calculo da auc (area under the curve)
auc = performance(pred.val,"auc")

# Plota curva ROC
performance = performance(pred.val, "tpr", "fpr")
plot(performance, col = "blue", lwd = 5)
abline(a=0, b=1, lwd =2, lty = 2)

#Calculo Estatística KS
ks <- max(attr(performance, "y.values")[[1]] - (attr(performance, "x.values")[[1]]))
ks

table(test_cl1$EVOL_new, pred.Teste >0.5)

unlist(auc@y.values)

accuracy

summary(train_cl1)

cutoffs <- seq(0.1, 0.9, 0.05)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(modelo$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(train_cl1$EVOL_new ==prediction))/length(prediction)*100) }

plot(cutoffs, accuracy, pch =19, type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")



