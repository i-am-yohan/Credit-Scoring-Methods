library(caret)
library(randomForest)
library(doParallel)
library(ranger)
library(tuneRanger)


#7.2 LEts give Random Forests a go!
#7.2.1 Mortgages random forrest


#Very computationally expensive reduce the variables

MTG_RF = h2o.randomForest(y = 'TARGET', training_frame = MTG_Train_h2o, nfolds = 5, weights_column = 'weight',
                          stopping_metric='AUC'
                          ,ntrees = 250,max_depth = 7)



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
  Output$F1 <- 2*(Output$Precision*Output$Recall)/(Output$Precision+Output$Recall)
  Output$False_Positive_Rate <- FP/(TN + FP) 
  Output$AUC <- h2o.auc(h2o.performance(In_Model, In_Data))
  return(Output)
}

Test_h2o(MTG_RF , MTG_Train_h2o) #Overfit number of trees
Test_h2o(MTG_RF , MTG_CV_h2o)









#7.2.2 Retail Loans random forrest

#Very computationally expensive reduce the variables

RL_RF = h2o.randomForest(y = 'TARGET', training_frame = RL_Train_h2o, nfolds = 5, weights_column = 'weight',
                          stopping_metric='AUC'
                          ,ntrees = 250,max_depth = 8)


Test_h2o(RL_RF , RL_Train_h2o)
Test_h2o(RL_RF , RL_CV_h2o)
#






#7.2.3 IRe random forrest

#Very computationally expensive reduce the variables

#
IRE_RF = h2o.randomForest(y = 'TARGET', training_frame = IRE_Train_h2o, nfolds = 5, weights_column = 'weight',
                         stopping_metric='AUC'
                         ,ntrees = 250,max_depth = 8)


Test_h2o(IRE_RF , IRE_Train_h2o)
Test_h2o(IRE_RF , IRE_CV_h2o)


#Recall is relatively low for this one!
