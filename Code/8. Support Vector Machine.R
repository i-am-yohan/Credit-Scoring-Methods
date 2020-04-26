library(e1071)
library(unbalanced)

# 
#Mortgages SVM - Under sampling - Used to balance the sample and cut down rows
#Use Linear Kernel with Woes - That way we can ensure that a linearly sepearable problem exists

#Lets Subset the samples
MTG_Col_List_SVM <- c('TARGET')
for (col in names(MTG)[(!names(MTG) %in% 'TARGET')]){
  if (MTG_Woe[[col]][1]$total_iv >= 0.05){
    MTG_Col_List_SVM <- append(MTG_Col_List_SVM , paste(col,'_woe',sep=''))
  }
}

#Use Linear Scorecard Datasets
MTG_Train_SVM_Woe <- MTG_Woe_Deploy[,MTG_Col_List_SVM]
MTG_CV_SVM_Woe <- MTG_SC_CV[,MTG_Col_List_SVM]
MTG_Test_SVM_Woe <- MTG_SC_Test[,MTG_Col_List_SVM]

MTG_Train_SVM_Woe <- stratified(MTG_Train_SVM_Woe, 'TARGET', 10000)
MTG_Param_SVM_Woe <- stratified(MTG_Train_SVM_Woe, 'TARGET', 1000)
MTG_CV_SVM_Woe <- stratified(as.data.frame(MTG_CV_SVM_Woe), 'TARGET', 2000)

Test_SVM <- function(In_Model , In_Data){
  
  p <- predict(In_Model, newdata=In_Data)
  
  CM <- table(In_Data$TARGET , as.numeric(as.vector(p)))
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  
  pred <- prediction(as.numeric(as.vector(p)) , In_Data$TARGET)
  
  prf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
  
  auc <- ROCR::performance(pred, measure = "auc")
  
  Output <- NULL
  Output$Confusion_Matrix <- CM
  Output$Accuracy <- (TP + TN)/(TP + TN + FP + FN)
  Output$Precision <- TP/(TP + FP)
  Output$Recall <- TP/(TP + FN)
  Output$False_Positive_Rate <- FP/(TN + FP)
  Output$F1 <- 2*(Output$Precision*Output$Recall)/(Output$Precision+Output$Recall)
  Output$AUC <- auc@y.values[[1]]
  print(plot(ROCR::performance(pred,"tpr","fpr"),main = 'ROC Curve'))
  return(Output)
}

#SVM complexity grows with sample size. May cut this down!
#Lets tune the SVM with a smaller subsample, the CV set!
tune_svm <- function(in_data){
  epsilon = seq(0.05,1,0.05)
  cost = 2^(-2:9)
  for (i in epsilon){
    for (j in cost){
      SVM_ <- svm(TARGET ~ .
                  , data=in_data
                  , type= 'C-classification'
                  , kernel = 'linear'
                  , probability = TRUE
                  , tolerance = i
                  , cost = j
      )
      out <- Test_SVM(SVM_ , in_data)
      print(paste('Epsilon:' , i, 'Cost:' , j , 'AUC:', out$AUC , 'Accuracy:',out$Accuracy))
    }
  }
}

#tune_svm(MTG_Param_SVM_Woe) # Changing paramters has no effect on overall model

MTG_svm <- svm(TARGET ~ .
               , data=MTG_Train_SVM_Woe
               , type= 'C-classification'
               , kernel = 'linear'
               , probability = TRUE
               , cross = 5 #5-fold CV
               )


Test_SVM(MTG_svm , MTG_Train_SVM_Woe)
Test_SVM(MTG_svm , MTG_CV_SVM_Woe)





# tHe vehicle loans dataset
RL_Col_List_SVM <- c('TARGET')
for (col in names(RL)[(!names(RL) %in% c('TARGET','issue_d'))]){
  if (RL_Woe[[col]][1]$total_iv >= 0.03){ # Change to 0.3 because 0.05 leaves nothing behind
    RL_Col_List_SVM <- append(RL_Col_List_SVM , paste(col,'_woe',sep=''))
  }
}

#Use Linear Scorecard Datasets
RL_Train_SVM_Woe <- RL_Woe_Deploy[,RL_Col_List_SVM]
RL_CV_SVM_Woe <- RL_SC_CV[,RL_Col_List_SVM]
RL_Test_SVM_Woe <- RL_SC_Test[,RL_Col_List_SVM]


RL_Train_SVM_Woe <- stratified(RL_Train_SVM_Woe, 'TARGET', 10000)
RL_Param_SVM_Woe <- stratified(RL_Train_SVM_Woe, 'TARGET', 1000)
RL_CV_SVM_Woe <- stratified(as.data.frame(RL_CV_SVM_Woe), 'TARGET', 2000)

#tune_svm(RL_Param_SVM_Woe) #No variance


#SVM complexity grows with sample size. May cut this down!
RL_svm <- svm(TARGET ~ .
               , data=RL_Train_SVM_Woe
               , type= 'C-classification'
               , kernel = 'linear'
               , probability = TRUE
              , cross = 5
)



Test_SVM(RL_svm , RL_Train_SVM_Woe)
Test_SVM(RL_svm , RL_CV_SVM_Woe)





#The Irish loans dataset
IRE_Col_List_SVM <- c('TARGET')
for (col in names(IRE)[(!names(IRE) %in% c('TARGET','issue_d','final_d'))]){
  if (IRE_Woe[[col]][1]$total_iv >= 0.05){
    IRE_Col_List_SVM <- append(IRE_Col_List_SVM , paste(col,'_woe',sep=''))
    print(col)
  }
}

#Use Linear Scorecard Datasets
IRE_Train_SVM_Woe <- IRE_Woe_Deploy[,IRE_Col_List_SVM]
IRE_CV_SVM_Woe <- IRE_SC_CV[,IRE_Col_List_SVM]
IRE_Test_SVM_Woe <- IRE_SC_Test[,IRE_Col_List_SVM]

IRE_Train_SVM_Woe <- stratified(as.data.frame(IRE_Train_SVM_Woe), 'TARGET', 10000)
IRE_CV_SVM_Woe <- stratified(as.data.frame(IRE_CV_SVM_Woe), 'TARGET', 5000)

#SVM complexity grows with sample size. May cut this down!
IRE_svm <- svm(TARGET ~ .
               , data=IRE_Train_SVM_Woe
               , type= 'C-classification'
               , kernel = 'linear'
               , probability = TRUE
               , cross = 5
)

#Grid Search SVM

Test_SVM(IRE_svm , IRE_Train_SVM_Woe)
Test_SVM(IRE_svm , IRE_CV_SVM_Woe)

