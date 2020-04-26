
#MTG NN


MTG_nn = h2o.deeplearning(y = 'TARGET', training_frame = MTG_Train_h2o, nfolds = 5, weights_column = 'weight',
                          epochs=1)



Test_h2o <- function(In_Model , In_Data){

  CM <- h2o.confusionMatrix(In_Model, newdata=In_Data)
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  Output <- NULL
  Output$Confusion_Matrix <- CM
  Output$Accuracy <- (TP + TN)/(TP + TN + FP + FN)
  Output$Precision <- TP/(TP + FP)
  Output$Recall <- TP/(TP + FN)
  Output$False_Positive_Rate <- FP/(TN + FP)
  Output$F1 <- 2*(Output$Precision*Output$Recall)/(Output$Precision+Output$Recall)
  Output$AUC <- h2o.auc(h2o.performance(In_Model, In_Data))
  return(Output)
}
Test_h2o(MTG_nn, MTG_Train_h2o)
Test_h2o(MTG_nn, MTG_CV_h2o) #Overfit!! assigns everything as default





#The Retail Loans Data

RL_nn = h2o.deeplearning(y = 'TARGET', training_frame = RL_Train_h2o, nfolds = 5, weights_column = 'weight',
                          epochs=1)

Test_h2o(RL_nn, RL_Train_h2o)
Test_h2o(RL_nn, RL_CV_h2o)





#The Irish Loans Data

IRE_nn = h2o.deeplearning(y = 'TARGET', training_frame = IRE_Train_h2o, nfolds = 5, weights_column = 'weight',
                         epochs=1)

Test_h2o(IRE_nn, IRE_Train_h2o)
Test_h2o(IRE_nn, IRE_CV_h2o) #Overfit!! assigns everything as default






