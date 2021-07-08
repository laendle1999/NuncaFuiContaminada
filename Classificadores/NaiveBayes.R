library(ggplot2)
library(e1071)
library(caret)
library(caTools)

#limpar ambiente
rm(list=ls())

EVOLUCAO = read.csv("~/Área de Trabalho/covid/Novo/out_evolucao_datas.csv", sep ="," )

###### Leitura da base direta no CSV

train_cl1 = read.csv("~/Área de Trabalho/covid/Novo/treino_evolucao_datas.csv", sep ="," )
test_cl1 = read.csv("~/Área de Trabalho/covid/Novo/teste_evolucao_datas.csv", sep ="," )


#discretização do campo idade.

EVOLUCAO$NU_IDADE_Ncat = rep("A", nrow(EVOLUCAO))
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 20] = "B"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 40] = "C"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 60] = "D"
EVOLUCAO$NU_IDADE_Ncat[EVOLUCAO$NU_IDADE_N > 80] = "E"
EVOLUCAO$NU_IDADE_Ncat = as.factor(EVOLUCAO$NU_IDADE_Ncat)
summary(EVOLUCAO[,c('NU_IDADE_Ncat')])


EVOLUCAO$DT_NOTIFIC = as.Date(EVOLUCAO$DT_NOTIFIC, format("%d/%m/%Y"))
EVOLUCAO$DT_INTERNA = as.Date(EVOLUCAO$DT_INTERNA, format("%d/%m/%Y"))
EVOLUCAO$DT_EVOLUCA = as.Date(EVOLUCAO$DT_EVOLUCA, format("%d/%m/%Y"))
EVOLUCAO$DT_SIN_PRI = as.Date(EVOLUCAO$DT_SIN_PRI, format("%d/%m/%Y"))

#Calculos das diferenças do tempo

EVOLUCAO$DT_TOTAL = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_SIN_PRI , units = "days"))
EVOLUCAO$DT_INTERNADO = as.numeric(difftime(EVOLUCAO$DT_EVOLUCA, EVOLUCAO$DT_INTERNA , units = "days"))
#EVOLUCAO$DT_UTI = as.numeric(difftime(EVOLUCAO$DT_SAIDUTI, EVOLUCAO$DT_ENTUTI, units = "days"))


#Discretização tempo internado (delta de 3 dias)

EVOLUCAO$DT_INTERNADO_Ncat = rep("A", nrow(EVOLUCAO))
EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 3] = "B"
EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 6] = "C"
EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 9] = "D"
EVOLUCAO$DT_INTERNADO_Ncat[EVOLUCAO$DT_INTERNADO > 12] = "E"
EVOLUCAO$DT_INTERNADO_Ncat = as.factor(EVOLUCAO$DT_INTERNADO_Ncat)
summary(EVOLUCAO[,c('DT_INTERNADO_Ncat')])

#ggplot(data.frame(EVOLUCAO$DT_INTERNADO_Ncat), aes(EVOLUCAO$DT_INTERNADO_Ncat)) + geom_bar()


#Discretização tempo total (delta de 6 dias)

EVOLUCAO$DT_TOTAL_Ncat = rep("A", nrow(EVOLUCAO))
EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 6] = "B"
EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 12] = "C"
EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 18] = "D"
EVOLUCAO$DT_TOTAL_Ncat[EVOLUCAO$DT_TOTAL > 24] = "E"
EVOLUCAO$DT_TOTAL_Ncat = as.factor(EVOLUCAO$DT_TOTAL_Ncat)
summary(EVOLUCAO[,c('DT_TOTAL_Ncat')])

#ggplot(data.frame(EVOLUCAO$DT_TOTAL_Ncat), aes(EVOLUCAO$DT_TOTAL_Ncat)) + geom_bar()

#remover colunas de tempo apois tratametno

EVOLUCAO1 = subset(EVOLUCAO, select = c(-DT_NOTIFIC,  -DT_INTERNA, -DT_EVOLUCA, -DT_SIN_PRI, -NU_IDADE_N,  -DT_INTERNADO, -DT_TOTAL))

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


#limpando memória para não travar
rm(EVOLUCAO)

####################### Treinamento do Naives Bayes - Evolução
#transpose
library(data.table)

# Splitting data into train
# and test data
EVOLUCAO1$spl <- sample.split(transpose(EVOLUCAO1), SplitRatio=0.7)
train_cl1 <- subset(EVOLUCAO1, EVOLUCAO1$spl == "TRUE")
test_cl1 <- subset(EVOLUCAO1, EVOLUCAO1$spl == "FALSE")

train_cl1 <- subset(train_cl1, select =c(-spl))
test_cl1 <- subset(test_cl1, select =c(-spl))

# Fitting Naive Bayes Model 
# to training dataset
set.seed(120)  # Setting Seed
classifier_cl1 <- naiveBayes(EVOLUCAO ~ ., data = train_cl1, laplace = 1)
classifier_cl1

# Predicting on test data'
y_pred1 <- predict(classifier_cl1, newdata = test_cl1)

# Confusion Matrix
cm1 <- table(test_cl1$EVOLUCAO, y_pred1)
#cm1

# Model Evauation
confusionMatrix(cm1)

#Precisão para Evolução
precision(test_cl1$EVOLUCAO, y_pred1)


#limpando memória para não travar

rm(EVOLUCAO1)

#tentativa de otimização utilizando pacote mlr
library(mlr)

task = makeClassifTask(data = test_cl1, target = "EVOLUCAO")

select_model = makeLearner("classif.naiveBayes")

nb_mlr = train(select_model, task)

nb_mlr$learner.model

y_pred1 <-  as.data.frame(predict(nb_mlr, newdata = test_cl1))

# Confusion Matrix
cm2 <- table(test_cl1$EVOLUCAO, y_pred1[,1])
#cm1

# Model Evauation
confusionMatrix(cm2)

#Precisão para Evolução
precision(test_cl1$EVOLUCAO, y_pred1)
