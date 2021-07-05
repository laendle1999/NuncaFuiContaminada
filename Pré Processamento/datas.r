out_evolucao$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y") - as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_evolucao$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))
out_evolucao$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_evolucao$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_evolucao$DT_SIN_PRI), format="%d/%m/%Y"))
write.csv(out_evolucao, 'out_evolucao_datas.csv')

out_uti$DIAS_INTERNACAO <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS <- as.numeric(as.Date(as.character(out_uti$DT_EVOLUCA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))
out_uti$DIAS_SINTOMAS_A_INTERNAR <- as.numeric(as.Date(as.character(out_uti$DT_INTERNA), format="%d/%m/%Y")- as.Date(as.character(out_uti$DT_SIN_PRI), format="%d/%m/%Y"))
write.csv(out_uti, 'out_uti_datas.csv')