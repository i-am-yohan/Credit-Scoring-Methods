library(splitstackshape)
library(dplyr)
library(Hmisc)
#4 Stratified sampling
set.seed(198666)

MTG_Disp <- MTG
RL_Disp <- RL
IRE_Disp <- IRE

MTG <- MTG[,!(names(MTG) %in% 'TARGET_CHAR')]
RL <- RL[,!(names(RL) %in% 'TARGET_CHAR')]
IRE <- IRE[,!(names(IRE) %in% 'TARGET_CHAR')]

#4.1 MTG - split between train, test and CV
MTG_Train_idx <- createDataPartition(MTG$TARGET, p = .7, list = FALSE)
MTG_Train <- MTG[MTG_Train_idx,]
MTG_Test_CV <- MTG[-MTG_Train_idx,]

MTG_CV_idx <- createDataPartition(MTG_Test_CV$TARGET, p = .33, list = FALSE)
MTG_CV <- MTG_Test_CV[MTG_CV_idx,]
MTG_Test <- MTG_Test_CV[-MTG_CV_idx,]



#4.2 RL - split between train, test and CV
RL_Train_idx <- createDataPartition(RL$TARGET, p = .7, list = FALSE)
RL_Train <- RL[RL_Train_idx,]
RL_Test_CV <- RL[-RL_Train_idx,]

RL_CV_idx <- createDataPartition(RL_Test_CV$TARGET, p = .33, list = FALSE)
RL_CV <- RL_Test_CV[RL_CV_idx,]
RL_Test <- RL_Test_CV[-RL_CV_idx,]



#4.3 IRE - split between train, test and CV
IRE_Train <- as.data.frame(stratified(IRE, c('TARGET','issue_d'), 0.7, keep.rownames =TRUE))
rownames(IRE_Train) <- IRE_Train[,1]
IRE_Train[,1] <- NULL
IRE_Test_CV <- IRE[which(rownames(IRE) %nin% rownames(IRE_Train)),]

IRE_CV <- as.data.frame(stratified(IRE_Test_CV, c('TARGET','issue_d'), .33, keep.rownames =TRUE))
rownames(IRE_CV) <- IRE_CV[,1]
IRE_CV[,1] <- NULL
IRE_Test <- IRE_Test_CV[which(rownames(IRE_Test_CV) %nin% rownames(IRE_CV)),]

