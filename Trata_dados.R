library(caTools)
library(data.table)

#Limpa lista
rm(list=ls())

#entrada dos dados
out_evolucao = read.csv("~/Área de Trabalho/covid/Novo/out_evolucao.csv", sep ="," )
out_uti = read.csv("~/Área de Trabalho/covid/Novo/out_uti.csv", sep ="," )

#Calcular o número de dias entre as datas
out_evolucao$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y") - as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))

out_uti$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))

#Remove as datas impossiveis
out_evolucao <- out_evolucao[out_evolucao$DIAS_INTERNACAO >= 0,]
out_evolucao <- out_evolucao[out_evolucao$DIAS_SINTOMAS_A_INTERNAR >= 0,]
out_evolucao <- out_evolucao[out_evolucao$DIAS_SINTOMAS >= 0,]

#Considera apenas caso onde ocorreu internação
out_evolucao <- out_evolucao[out_evolucao$HOSPITAL == 1,]


out_uti <- out_uti[out_uti$DIAS_INTERNACAO >= 0,]
out_uti <- out_uti[out_uti$DIAS_SINTOMAS_A_INTERNAR >= 0,]
out_uti <- out_uti[out_uti$DIAS_SINTOMAS >= 0,]

#Considera apenas caso onde ocorreu internação
out_uti <- out_uti[out_uti$HOSPITAL ==1,]

#Remove a coluna das datas
out_evolucao$DT_NOTIFIC <- NULL
out_evolucao$DT_EVOLUCA <- NULL
out_evolucao$DT_SIN_PRI <- NULL
out_evolucao$DT_INTERNA <- NULL

out_uti$DT_NOTIFIC <- NULL
out_uti$DT_EVOLUCA <- NULL
out_uti$DT_SIN_PRI <- NULL
out_uti$DT_INTERNA <- NULL

#Exporta os dados tratados
write.csv(out_evolucao, 'out_evolucao_datas.csv')
write.csv(out_uti, 'out_uti_datas.csv')

#verificar estatistica basicas

summary(out_evolucao)

summary(out_uti)

sapply(out_uti, function(x) sum(is.na(x)))

# Splitting data into train (70%) and test (30%) data
set.seed(120)
out_evolucao$spl <- sample.split(transpose(out_evolucao), SplitRatio=0.7)
train_cl1 <- subset(out_evolucao, out_evolucao$spl == "TRUE")
test_cl1 <- subset(out_evolucao, out_evolucao$spl == "FALSE")

train_cl1 <- subset(train_cl1, select =c(-spl))
test_cl1 <- subset(test_cl1, select =c(-spl))

out_uti$spl <- sample.split(transpose(out_uti), SplitRatio=0.7)
train_cl2 <- subset(out_uti, out_uti$spl == "TRUE")
test_cl2 <- subset(out_uti, out_uti$spl == "FALSE")

train_cl2 <- subset(train_cl2, select =c(-spl))
test_cl2 <- subset(test_cl2, select =c(-spl))

#Exporta os dados tratados (treino e teste)
write.csv(train_cl1, '~/Área de Trabalho/covid/Novo/treino_evolucao_datas.csv')
write.csv(test_cl1, '~/Área de Trabalho/covid/Novo/teste_evolucao_datas.csv')
write.csv(train_cl2, '~/Área de Trabalho/covid/Novo/treino_UTI_datas.csv')
write.csv(test_cl2, '~/Área de Trabalho/covid/Novo/teste_UTI_datas.csv')
