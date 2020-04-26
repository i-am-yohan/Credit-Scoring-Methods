library(tree)
library(rpart)
library(rpart.plot)
library(partykit)

#A function for Testing decision trees, like before!
Test_DT <- function(In_Model , In_Data){
  
  p <- predict(In_Model, type="class", newdata=In_Data)
  
  CM <- table(In_Data$TARGET , p)
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
  print(plot(ROCR::performance(pred, "tpr", "fpr"),main = 'ROC Curve'))
  return(Output)
}


#Lets fit decision trees

#Mortgages
MTG_Tree <- rpart(TARGET ~ . -weight, weights = weight, data = MTG_Train , method="class",
                  cp = 0.01)
rpart.plot(MTG_Tree) #Not very useful!

Test_DT(MTG_Tree, MTG_Train) #Not a lot of precision!
Test_DT(MTG_Tree, MTG_CV) #Not overfit!


#Vehicle Loans
RL_Tree <- rpart(TARGET ~ . -weight, weights = weight, data = RL_Train, method="class",
                 cp = 0.01)
rpart.plot(RL_Tree) #Branch ID?

Test_DT(RL_Tree , RL_Train) #very low recall
Test_DT(RL_Tree , RL_CV) #No sign of overfitting



#Ireland
IRE_Tree <- rpart(TARGET ~ . -weight, weights = weight, data = IRE_Train, method="class"
                  ,cp = 0.01)
rpart.plot(IRE_Tree) #A Good Tree there!, lets investigate

#predict(IRE_Tree, type='class', data=IRE_CV)
table(predict(IRE_Tree, newdata=IRE_CV, type = 'class') , IRE_CV$TARGET)



Test_DT(IRE_Tree , IRE_Train) #Massively
Test_DT(IRE_Tree , IRE_CV)

IRE_Train_CV <- rbind(IRE_Train, IRE_CV)
printcp(IRE_Tree) #Error is lowest no matter how far the the tree is built



#Lets test the other two:


#Decision tree works for IRE dataset and creates a dam good model!!