#                Real
#          |Verd | Fal |
#Pred |Posi| VP  | FP  |
#     |Neg | FN  | VN  |

#Sigla - Real - predito
#VP = Verdadeiro positivo
#VN - Verdadeiro negativo
#FP - Falso      positivo
#FN - Falso      Negativo



accc = function(VP,VN,FN,FP){
  acc = ((VP + VN)/(VP + VN + FN + FP)) * 100
  P = (VP)/(VP+FP) * 100
  R = (VP)/(VP+FN) * 100
  F1 = 2 * ((P * R) / (P + R))
  ESP = (VN)/(VN+FP) * 100
  TFP = (FP)/(VN+FP) * 100
  print("Real Positivo e Predito Positivo - Verdadeiro Positivo:")
  print(VP / (VP + VN + FN + FP))
  print("Real Positivo e Predito Negativo - Falso negativo:")
  print(FN / (VP + VN + FN + FP) )
  print("Real Negativo e Predito Positivo - Falso Positivo:")
  print(FP / (VP + VN + FN + FP) )
  print("Real Negativo e Predito Negativo - Verdadeiro negativo:")
  print(VN / (VP + VN + FN + FP) )
  print("acuracia - indica exatidão do valor correto")
  print(acc)
  print("Precisão - indica os Falso-positivo ")
  print(P)
  print("Recal / Sensitividade - Taxa de verdadeiro positivo (TVP): Taxa de amostras positivas que são corretamentes classificadas")
  print(R)
  print("F1 Score - média harmonica da precisão e da sensibilidade")
  print(F1)
  print("Especificidade -Taxa de verdadeiro negativo (TVN)- Taxa de amostras negativas que são corretamentes  classificadas")
  print(ESP)
  print("Taxa de falso positivo (TFP) - É a taxa de amostra positivas que são erroneamente classificadas (TFP = 1 - TVN) ")
  print(TFP)
}

test = function() {

  cm1 = table(train_cl1$targetsTrain, prediction)

  accc(1,2,3,4)

  cm1[2,1]

  print("TBase de treinamento: 70%")
  accc(cm1[1,1], cm1[2,2], cm1[1,2], cm1[2,1]  )

  print("Base de teste: 30%")
  accc(cm2[1,1], cm2[2,2], cm2[1,2], cm2[2,1]  )


  ####Calculo curva ROC

  # Predicting on test data'
  y_pred1 <- predict(classifier_cl1, newdata = test_cl1, positive = "1")
  #transformar dados da predição e do teste em numerico para a curva ROC
  y_pred = prediction(as.numeric(y_pred1), as.numeric(test_cl1$EVOL_new))


  library(ROCR)

  # calculo da auc (area under the curve)
  auc = performance(y_pred,"auc")
  unlist(auc@y.values)

  performance = performance(y_pred, "tpr", "fpr")
  plot(performance, col = "blue", lwd = 5)
  abline(a=0, b=1, lwd =2, lty = 2)


}

acccFromTable = function(table) {
  accc(table[1,1], table[2,2], table[1,2], table[2,1]  )
}
