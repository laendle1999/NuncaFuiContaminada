library(ggplot2)
library(e1071)
library(caret)
library(caTools)

#limpar ambiente
rm(list=ls())

UTI = read.csv("~/Área de Trabalho/covid/Novo/out_uti_datas.csv", sep ="," )


#discretização do campo idade.
UTI$NU_IDADE_Ncat = rep("A", nrow(UTI))
UTI$NU_IDADE_Ncat[UTI$NU_IDADE_N > 20] = "B"
UTI$NU_IDADE_Ncat[UTI$NU_IDADE_N > 40] = "C"
UTI$NU_IDADE_Ncat[UTI$NU_IDADE_N > 60] = "D"
UTI$NU_IDADE_Ncat[UTI$NU_IDADE_N > 80] = "E"
UTI$NU_IDADE_Ncat = as.factor(UTI$NU_IDADE_Ncat)
summary(UTI[,c('NU_IDADE_Ncat')])


#tratamento Idade
UTI$DT_NOTIFIC = as.Date(UTI$DT_NOTIFIC, format("%d/%m/%Y"))
UTI$DT_INTERNA = as.Date(UTI$DT_INTERNA, format("%d/%m/%Y"))
#UTI$DT_ENTUTI = as.Date(UTI$DT_ENTUTI, format("%d/%m/%Y"))
#UTI$DT_SAIDUTI = as.Date(UTI$DT_SAIDUTI, format("%d/%m/%Y"))
UTI$DT_EVOLUCA = as.Date(UTI$DT_EVOLUCA, format("%d/%m/%Y"))
UTI$DT_SIN_PRI = as.Date(UTI$DT_SIN_PRI, format("%d/%m/%Y"))


#Calculos das diferenças do tempo

UTI$DT_TOTAL = as.numeric(difftime(UTI$DT_EVOLUCA, UTI$DT_SIN_PRI , units = "days"))
UTI$DT_INTERNADO = as.numeric(difftime(UTI$DT_EVOLUCA, UTI$DT_INTERNA , units = "days"))
#UTI$DT_UTI = as.numeric(difftime(UTI$DT_SAIDUTI, UTI$DT_ENTUTI, units = "days"))




#Discretização tempo internado (delta de 3 dias)

UTI$DT_INTERNADO_Ncat = rep("A", nrow(UTI))
UTI$DT_INTERNADO_Ncat[UTI$DT_INTERNADO > 3] = "B"
UTI$DT_INTERNADO_Ncat[UTI$DT_INTERNADO > 6] = "C"
UTI$DT_INTERNADO_Ncat[UTI$DT_INTERNADO > 9] = "D"
UTI$DT_INTERNADO_Ncat[UTI$DT_INTERNADO > 12] = "E"
UTI$DT_INTERNADO_Ncat = as.factor(UTI$DT_INTERNADO_Ncat)
summary(UTI[,c('DT_INTERNADO_Ncat')])

#ggplot(data.frame(UTI$DT_INTERNADO_Ncat), aes(UTI$DT_INTERNADO_Ncat)) + geom_bar()

#Discretização tempo total (delta de 6 dias)

UTI$DT_TOTAL_Ncat = rep("A", nrow(UTI))
UTI$DT_TOTAL_Ncat[UTI$DT_TOTAL > 6] = "B"
UTI$DT_TOTAL_Ncat[UTI$DT_TOTAL > 12] = "C"
UTI$DT_TOTAL_Ncat[UTI$DT_TOTAL > 18] = "D"
UTI$DT_TOTAL_Ncat[UTI$DT_TOTAL > 24] = "E"
UTI$DT_TOTAL_Ncat = as.factor(UTI$DT_TOTAL_Ncat)
summary(UTI[,c('DT_TOTAL_Ncat')])

#ggplot(data.frame(UTI$DT_TOTAL_Ncat), aes(UTI$DT_TOTAL_Ncat)) +  geom_bar()

#ggplot(data.frame(UTI$DT_UTI_Ncat), aes(UTI$DT_UTI_Ncat)) +  geom_bar()

#remover colunas de tempo apois tratametno

UTI1 = subset(UTI, select = c(-DT_NOTIFIC, -DT_INTERNA,  -DT_EVOLUCA, -DT_SIN_PRI, -NU_IDADE_N, -DT_INTERNADO, -DT_TOTAL))

# Converter para fator

UTI1$SG_UF_NOT = as.factor(UTI1$SG_UF_NOT)
UTI1$CS_SEXO = as.factor(UTI1$CS_SEXO)
UTI1$CS_GESTANT = as.factor(UTI1$CS_GESTANT)
UTI1$CS_RACA = as.factor(UTI1$CS_RACA)
UTI1$CS_ESCOL_N = as.factor(UTI1$CS_ESCOL_N)
#UTI1$SURTO_SG = as.factor(UTI1$SURTO_SG)
UTI1$NOSOCOMIAL = as.factor(UTI1$NOSOCOMIAL)
UTI1$FEBRE = as.factor(UTI1$FEBRE)
UTI1$TOSSE = as.factor(UTI1$TOSSE)
UTI1$GARGANTA = as.factor(UTI1$GARGANTA)
UTI1$DISPNEIA = as.factor(UTI1$DISPNEIA)
UTI1$DESC_RESP = as.factor(UTI1$DESC_RESP)
UTI1$SATURACAO = as.factor(UTI1$SATURACAO)
UTI1$DIARREIA = as.factor(UTI1$DISPNEIA)
UTI1$VOMITO = as.factor(UTI1$VOMITO)
UTI1$DOR_ABD = as.factor(UTI1$DOR_ABD)
UTI1$FADIGA = as.factor(UTI1$FADIGA)
UTI1$PERD_OLFT = as.factor(UTI1$PERD_OLFT)
UTI1$PERD_PALA = as.factor(UTI1$PERD_PALA)
UTI1$PUERPERA = as.factor(UTI1$PUERPERA)
UTI1$CARDIOPATI = as.factor(UTI1$CARDIOPATI)
UTI1$HEMATOLOGI = as.factor(UTI1$HEMATOLOGI)
UTI1$SIND_DOWN = as.factor(UTI1$SIND_DOWN)
UTI1$HEPATICA = as.factor(UTI1$HEPATICA)
UTI1$ASMA = as.factor(UTI1$ASMA)
UTI1$DIABETES = as.factor(UTI1$DIABETES)
UTI1$NEUROLOGIC = as.factor(UTI1$NEUROLOGIC)
UTI1$PNEUMOPATI = as.factor(UTI1$PNEUMOPATI)
UTI1$IMUNODEPRE = as.factor(UTI1$IMUNODEPRE)
UTI1$RENAL = as.factor(UTI1$RENAL)
UTI1$OBESIDADE = as.factor(UTI1$OBESIDADE)
UTI1$VACINA = as.factor(UTI1$VACINA)
UTI1$ANTIVIRAL = as.factor(UTI1$ANTIVIRAL)
UTI1$HOSPITAL = as.factor(UTI1$HOSPITAL)
UTI1$UTI = as.factor(UTI1$UTI)
UTI1$SUPORT_VEN = as.factor(UTI1$SUPORT_VEN)
UTI1$RES_AN = as.factor(UTI1$RES_AN)
#UTI1$RES_IGG = as.factor(UTI1$RES_IGG)
#UTI1$RES_IGM = as.factor(UTI1$RES_IGM)
UTI1$PCR_RESUL = as.factor(UTI1$PCR_RESUL)
UTI1$CLASSI_FIN = as.factor(UTI1$CLASSI_FIN)
UTI1$EVOLUCAO = as.factor(UTI1$EVOLUCAO)




#limpando memória para não travar
rm(UTI)


####################### Treinamento do Naives Bayes - Evolução
#transpose
library(data.table)

####################### Treinamento do Naives Bayes - UTI

#UTI

# Splitting data into train
# and test data
UTI1$spl <- sample.split(transpose(UTI1), SplitRatio = 0.7)
train_cl2 <- subset(UTI1, UTI1$spl == "TRUE")
test_cl2 <- subset(UTI1, UTI1$spl == "FALSE")

train_cl2 <- subset(train_cl2, select =c(-spl))
test_cl2 <- subset(test_cl2, select =c(-spl))

# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl2 <- naiveBayes(UTI ~ ., data = train_cl2, laplace = 1)
classifier_cl2

# Predicting on test data'
y_pred2 <- predict(classifier_cl2, newdata = test_cl2)

test_cl2$pred = as.factor(y_pred2)


# Confusion Matrix
cm2 <- table(test_cl2$UTI, y_pred2)
#cm2

# Model Evauation
confusionMatrix(cm2)

#calculo precisão
library(MLmetrics)

#Precisão para UTI
precision(test_cl2$UTI, test_cl2$pred)

