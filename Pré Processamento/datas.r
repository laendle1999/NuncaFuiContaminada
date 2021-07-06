#Calcular o número de dias entre as datas
out_evolucao$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y") - as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))

out_uti$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))

#Remove as datas impossiveis

out_evolucao <- out_evolucao[out_evolucao$DIAS_INTERNACAO > 0,]

out_uti <- out_uti[out_uti$DIAS_INTERNACAO > 0,]

#Remove a coluna das datas

out_evolucao$DT_EVOLUCA <- NULL
out_evolucao$DT_SIN_PRI <- NULL
out_evolucao$DT_INTERNA <- NULL

out_uti$DT_EVOLUCA <- NULL
out_uti$DT_SIN_PRI <- NULL
out_uti$DT_INTERNA <- NULL

#Exporta os dados tratados

write.csv(out_evolucao, 'out_evolucao_datas.csv')

write.csv(out_uti, 'out_uti_datas.csv')
