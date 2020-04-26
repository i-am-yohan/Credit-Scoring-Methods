library(scorecard)
library(regclass)
library(glmnet)
library(ROCR)
#5. Typical Scorecard Example
#5.1 The Home_Credit Dataset
#5.1.1 The binning by WoE - Automatic

#Bin each variable by WoE
#The Mortgages dataset
MTG_Woe = woebin(MTG_Train, y = 'TARGET' , count_distr_limit = 0.05)



#Include onl IVs > 0.01 Used for dimensional reduction
MTG_Col_List <- c('TARGET')
IV_List <- c(NA)
for (col in names(MTG)[(!names(MTG) %in% 'TARGET')]){
  if (MTG_Woe[[col]][1]$total_iv >= 0.01){
    MTG_Col_List <- append(MTG_Col_List , col)
    IV_List <- append(IV_List , MTG_Woe[[col]][1]$total_iv)
  }
}


IV_Table <- as.data.frame(cbind(paste(MTG_Col_List , '_woe' , sep = '') , IV_List))

MTG_Col_List #Not a lot of variables left!

MTG_Woe_Deploy <- woebin_ply(MTG_Train[MTG_Col_List] , MTG_Woe , to = 'woe')
MTG_Woe_Deploy <- as.data.frame(MTG_Woe_Deploy)

#Find Higly correlated variables
Corr_Chck <- function(x){
  corr_table <- melt(cor(x))

  corr_table_IV <- sqldf(
    'select 
    bse.X1 as Var
  , bse.X2 as Comp_Var
  , value as Correlation
  , IV.IV_List as Var_IV
  , IV2.IV_List as Comp_IV
  from corr_table as bse
  left join IV_Table as IV on bse.X1 = IV.V1
  left join IV_Table as IV2 on bse.X2 = IV2.V1
  order by Correlation
  ')
  
  return(corr_table_IV[abs(corr_table_IV['Correlation']) > .7 & corr_table_IV['Var'] != corr_table_IV['Comp_Var'],])
}

Corr_Chck(MTG_Woe_Deploy)

#Variable Exclusions - Excluded because of i
excl_List <- c(#Excluded because of multi-collinearity
                'BASEMENTAREA_AVG_woe','COMMONAREA_AVG_woe','DEF_60_CNT_SOCIAL_CIRCLE_woe'
               ,'YEARS_BUILD_AVG_woe','APARTMENTS_AVG_woe','HOUSETYPE_MODE_woe','WALLSMATERIAL_MODE_woe'
               ,'LIVE_CITY_NOT_WORK_CITY_woe','REGION_RATING_CLIENT_woe','NONLIVINGAPARTMENTS_AVG_woe'
               ,'FLAG_EMP_PHONE_woe','WALLSMATERIAL_MODE_wo','HOUSETYPE_MODE_woe','LIVINGAPARTMENTS_AVG_woe'
               ,'ENTRANCES_AVG_woe','ELEVATORS_AVG_woe','LIVINGAREA_AVG_woe','EMERGENCYSTATE_MODE_woe'
               ,'YEARS_BEGINEXPLUATATION_AVG_woe','LANDAREA_AVG_woe','NONLIVINGAREA_AVG_woe','FONDKAPREMONT_MODE_woe'
               
               #Excluded because of insgnificance 
               ,'REGION_POPULATION_RELATIVE_woe','NAME_HOUSING_TYPE_woe','FLOORSMIN_AVG_woe'
               ,'HOUR_APPR_PROCESS_START_woe','NAME_INCOME_TYPE_woe'
               
               #Excluded because of unituitve relationship
               ,'AGE_woe','REG_CITY_NOT_WORK_CITY_woe','FLAG_DOCUMENT_6_woe'
               )

Corr_Chck(MTG_Woe_Deploy[ ,!(names(MTG_Woe_Deploy) %in% excl_List)])

MTG_SC_ABT <- MTG_Woe_Deploy[ ,!(names(MTG_Woe_Deploy) %in% excl_List)]

#Adding in the weight vector - For Unbalanced Data
weight <- NA
MTG_SC_ABT[MTG_SC_ABT['TARGET'] == 1,'weight'] <- nrow(MTG_SC_ABT)/sum(MTG_SC_ABT['TARGET'])
MTG_SC_ABT[MTG_SC_ABT['TARGET'] == 0,'weight'] <- nrow(MTG_SC_ABT)/(nrow(MTG_SC_ABT) - sum(MTG_SC_ABT['TARGET']))


MTG_SC_Model <- glm(TARGET~. -weight , weights = weight , data=MTG_SC_ABT , family=binomial())
summary(MTG_SC_Model)

#Checking the VIF
vif(MTG_SC_Model) #VIFs are Ok



#A function for testing scorecard
Test_SC <- function(In_Model , In_Data , Prob = 0.5){
  
  #The confusion matrix Prob is the decision boundary
  CM <- table(In_Data$TARGET , predict(In_Model, newdata = In_Data, type = "response") > Prob)
  TP <- CM[2,2]
  TN <- CM[1,1]
  FP <- CM[1,2]
  FN <- CM[2,1]
  
  p <- predict(In_Model, newdata = In_Data, type = "response")
  pr <- prediction(p, In_Data$TARGET)
  
  prf <- ROCR::performance(pr, measure = "tpr", x.measure = "fpr")
  
  auc <- ROCR::performance(pr, measure = "auc")
  
  Output <- NULL
  Output$Confusion_Matrix <- CM
  Output$Accuracy <- (TP + TN)/(TP + TN + FP + FN)
  Output$Precision <- TP/(TP + FP)
  Output$Recall <- TP/(TP + FN)
  Output$False_Positive_Rate <- FP/(TN + FP) 
  Output$AUC <- auc@y.values[[1]]
  Output$F1 <- 2*(Output$Precision*Output$Recall)/(Output$Precision+Output$Recall)
  print(plot(prf , main = "ROC Curve"))
  return(Output)
}
Test_SC(MTG_SC_Model , MTG_SC_ABT, 0.5)



#Test on Cross-Validation Set
MTG_SC_CV <- as.data.frame(woebin_ply(MTG_CV[MTG_Col_List] , MTG_Woe , to = 'woe'))
MTG_SC_CV[MTG_SC_CV['TARGET'] == 1,'weight'] <- nrow(MTG_SC_ABT)/sum(MTG_SC_ABT['TARGET'])
MTG_SC_CV[MTG_SC_CV['TARGET'] == 0,'weight'] <- nrow(MTG_SC_ABT)/(nrow(MTG_SC_ABT) - sum(MTG_SC_ABT['TARGET']))
Test_SC(MTG_SC_Model , MTG_SC_CV, 0.5) #not bad Precision very low - a lot of missed lending oppurtunities?

#Regularization parameter
MTG_SC_TRAIN_CV <- rbind(MTG_SC_CV[,names(MTG_SC_ABT)] , MTG_SC_ABT)
MTG_Lambda_CV <- cv.glmnet(as.matrix(MTG_SC_TRAIN_CV[,!names(MTG_SC_TRAIN_CV) %in% c('TARGET','weight')])
                    , as.matrix(MTG_SC_TRAIN_CV[,'TARGET'])
                    , weights = as.matrix(MTG_SC_TRAIN_CV[,'weight']) , family = 'binomial')

plot(MTG_Lambda_CV)
MTG_CV$lambda.min #close to 0, will not regularize

#On to testing
MTG_SC_Test <- as.data.frame(woebin_ply(MTG_Test[MTG_Col_List] , MTG_Woe , to = 'woe'))
MTG_SC_Test[MTG_SC_Test['TARGET'] == 1,'weight'] <- nrow(MTG_SC_ABT)/sum(MTG_SC_ABT['TARGET'])
MTG_SC_Test[MTG_SC_Test['TARGET'] == 0,'weight'] <- nrow(MTG_SC_ABT)/(nrow(MTG_SC_ABT) - sum(MTG_SC_ABT['TARGET']))

 #Results are as expected!
#73% AUC



#The retail loans dataset
#5.1.2 The binning by WoE - Automatic
RL_Woe = woebin(RL_Train, y = 'TARGET' , count_distr_limit = 0.05)

#Include only IVs > 0.01
RL_Col_List <- c('TARGET')
IV_List <- c(NA)
for (col in names(RL)[(!names(RL) %in% 'TARGET')]){
  if (RL_Woe[[col]][1]$total_iv >= 0.01){
    RL_Col_List <- append(RL_Col_List , col)
    IV_List <- append(IV_List , RL_Woe[[col]][1]$total_iv)
  }
}
IV_Table <- as.data.frame(cbind(paste(RL_Col_List , '_woe' , sep = '') , IV_List))


RL_Woe_Deploy <- woebin_ply(RL_Train[RL_Col_List] , RL_Woe , to = 'woe')
RL_Woe_Deploy <- as.data.frame(RL_Woe_Deploy)

Corr_Chck(RL_Woe_Deploy)

excl_List <- c(#Excluded because of multi-collinearity
  'PRI_DISBURSED_AMOUNT_log_woe','PRI_SANCTIONED_AMOUNT_log_woe','PERFORM_CNS_SCORE_DESCRIPTION_woe'
  ,'PRI_CURRENT_BALANCE_log_woe','AADHAR_FLAG_woe'
  #Excluded due to Insignificance
  ,'PRI_ACTIVE_ACCTS_log_woe'
)

Corr_Chck(RL_Woe_Deploy[ ,!(names(RL_Woe_Deploy) %in% excl_List)])

RL_SC_ABT <- RL_Woe_Deploy[ ,!(names(RL_Woe_Deploy) %in% excl_List)]

#Adding in the weight vector - For Unbalanced Data
weight <- NA
RL_SC_ABT[RL_SC_ABT['TARGET'] == 1,'weight'] <- nrow(RL_SC_ABT)/sum(RL_SC_ABT['TARGET'])
RL_SC_ABT[RL_SC_ABT['TARGET'] == 0,'weight'] <- nrow(RL_SC_ABT)/(nrow(RL_SC_ABT) - sum(RL_SC_ABT['TARGET']))

RL_SC_Model <- glm(TARGET~. -weight , weights = weight , data=RL_SC_ABT , family=binomial())
summary(RL_SC_Model)

vif(RL_SC_Model) #VIFs are Ok

Test_SC(RL_SC_Model , RL_SC_ABT, 0.5)

RL_SC_CV <- as.data.frame(woebin_ply(RL_CV[RL_Col_List] , RL_Woe , to = 'woe'))
RL_SC_CV[RL_SC_CV['TARGET'] == 1,'weight'] <- nrow(RL_SC_ABT)/sum(RL_SC_ABT['TARGET'])
RL_SC_CV[RL_SC_CV['TARGET'] == 0,'weight'] <- nrow(RL_SC_ABT)/(nrow(RL_SC_ABT) - sum(RL_SC_ABT['TARGET']))
Test_SC(RL_SC_Model , RL_SC_CV) #not bad Precision very low - a lot of missed lending oppurtunities?


#Regularization parameter
RL_SC_TRAIN_CV <- rbind(RL_SC_CV[,names(RL_SC_ABT)] , RL_SC_ABT) #Combine Train and CV for K fold
RL_Lambda_CV <- cv.glmnet(as.matrix(RL_SC_TRAIN_CV[,!names(RL_SC_TRAIN_CV) %in% c('TARGET','weight')])
                    , as.matrix(RL_SC_TRAIN_CV[,'TARGET'])
                    , weights = as.matrix(RL_SC_TRAIN_CV[,'weight']) , family = 'binomial')

plot(RL_Lambda_CV)
RL_Lambda_CV$lambda.min #close to 0, will not regularize


#On to testing
RL_SC_Test <- as.data.frame(woebin_ply(RL_Test[RL_Col_List] , RL_Woe , to = 'woe'))
RL_SC_Test[RL_SC_Test['TARGET'] == 1,'weight'] <- nrow(RL_SC_ABT)/sum(RL_SC_ABT['TARGET'])
RL_SC_Test[RL_SC_Test['TARGET'] == 0,'weight'] <- nrow(RL_SC_ABT)/(nrow(RL_SC_ABT) - sum(RL_SC_ABT['TARGET']))

 #Results are as expected!
#63% AUC - Not a good model



#The IRE Dataset
#5.1.2 The binning by WoE - Automatic
IRE_Woe = woebin(IRE_Train[,!names(IRE) %in% c('issue_d','final_d')], y = 'TARGET' , count_distr_limit = 0.05) #Some very High IVs there

#Include only IVs > 0.01
IRE_Col_List <- c('TARGET')
IV_List <- c(NA)
for (col in names(IRE)[(!names(IRE) %in% c('TARGET','issue_d','final_d'))]){
  if (IRE_Woe[[col]][1]$total_iv >= 0.01){
    IRE_Col_List <- append(IRE_Col_List , col)
    IV_List <- append(IV_List , IRE_Woe[[col]][1]$total_iv)
  }
}
IV_Table <- as.data.frame(cbind(paste(IRE_Col_List , '_woe' , sep = '') , IV_List))


IRE_Woe_Deploy <- woebin_ply(IRE_Train[IRE_Col_List] , IRE_Woe , to = 'woe')
IRE_Woe_Deploy <- as.data.frame(IRE_Woe_Deploy)

Corr_Chck(IRE_Woe_Deploy)

excl_List <- c(#Excluded because of multi-collinearity
  'grade_woe','interest_payments_woe',
  #Excluded due to Insignificance
  'income_category_woe',
  #Excluded due to unituitve Correlation
  'term_woe','dti_LOG_woe'
)

Corr_Chck(IRE_Woe_Deploy[ ,!(names(IRE_Woe_Deploy) %in% excl_List)])

IRE_SC_ABT <- IRE_Woe_Deploy[ ,!(names(IRE_Woe_Deploy) %in% excl_List)]

#Adding in the weight vector - For Unbalanced Data
weight <- NA
IRE_SC_ABT[IRE_SC_ABT['TARGET'] == 1,'weight'] <- nrow(IRE_SC_ABT)/sum(IRE_SC_ABT['TARGET'])
IRE_SC_ABT[IRE_SC_ABT['TARGET'] == 0,'weight'] <- nrow(IRE_SC_ABT)/(nrow(IRE_SC_ABT) - sum(IRE_SC_ABT['TARGET']))

IRE_SC_Model <- glm(TARGET~. -weight , weights = weight , data=IRE_SC_ABT , family=binomial()) 
summary(IRE_SC_Model)

vif(IRE_SC_Model) #VIFs are Ok

Test_SC(IRE_SC_Model , IRE_SC_ABT)

IRE_SC_CV <- as.data.frame(woebin_ply(IRE_CV[IRE_Col_List] , IRE_Woe , to = 'woe'))
IRE_SC_CV[IRE_SC_CV['TARGET'] == 1,'weight'] <- nrow(IRE_SC_ABT)/sum(IRE_SC_ABT['TARGET'])
IRE_SC_CV[IRE_SC_CV['TARGET'] == 0,'weight'] <- nrow(IRE_SC_ABT)/(nrow(IRE_SC_ABT) - sum(IRE_SC_ABT['TARGET']))
Test_SC(IRE_SC_Model , IRE_SC_CV) #not bad Precision very low - a lot of missed lending oppurtunities?


#Regularization parameter
IRE_SC_TRAIN_CV <- rbind(IRE_SC_CV[,names(IRE_SC_ABT)] , IRE_SC_ABT) #Combine Train and CV for K fold
IRE_Lambda_CV <- cv.glmnet(as.matrix(IRE_SC_TRAIN_CV[,!names(IRE_SC_TRAIN_CV) %in% c('TARGET','weight')])
                   , as.matrix(IRE_SC_TRAIN_CV[,'TARGET'])
                   , weights = as.matrix(IRE_SC_TRAIN_CV[,'weight']) , family = 'binomial')

plot(IRE_Lambda_CV)
IRE_Lambda_CV$lambda.min #close to 0, will not regularize


#On to testing
IRE_SC_Test <- as.data.frame(woebin_ply(IRE_Test[IRE_Col_List] , IRE_Woe , to = 'woe'))
IRE_SC_Test[IRE_SC_Test['TARGET'] == 1,'weight'] <- nrow(IRE_SC_ABT)/sum(IRE_SC_ABT['TARGET'])
IRE_SC_Test[IRE_SC_Test['TARGET'] == 0,'weight'] <- nrow(IRE_SC_ABT)/(nrow(IRE_SC_ABT) - sum(IRE_SC_ABT['TARGET']))

#Results are as expected!
#73% AUC - acceptable model

