library(BBmisc)
library(h2o)
library(data.table)


NA_Chck(MTG)
NA_Chck(RL)
NA_Chck(IRE)

#Should have stratified later!!

#Apply bins generated by scorecard
#Mortgages
Var_Bin_List <- c("YRS_EMPLOYED","OWN_CAR_AGE","APARTMENTS_AVG","BASEMENTAREA_AVG","YEARS_BEGINEXPLUATATION_AVG"
             ,"YEARS_BUILD_AVG","COMMONAREA_AVG","ELEVATORS_AVG","ENTRANCES_AVG","FLOORSMAX_AVG","FLOORSMIN_AVG"
             ,"LANDAREA_AVG","LIVINGAPARTMENTS_AVG","LIVINGAREA_AVG","NONLIVINGAPARTMENTS_AVG","NONLIVINGAREA_AVG"
             ,"ORGANIZATION_TYPE")
 
Woe_Append <- function(x){return(paste(x,'_bin',sep=''))}
MTG_Train[,Var_Bin_List] <- as.data.frame(woebin_ply(MTG_Train, MTG_Woe , to='bin'))[,Woe_Append(Var_Bin_List)]
MTG_Test[,Var_Bin_List] <- as.data.frame(woebin_ply(MTG_Test, MTG_Woe , to='bin'))[,Woe_Append(Var_Bin_List)]
MTG_CV[,Var_Bin_List] <- as.data.frame(woebin_ply(MTG_CV, MTG_Woe , to='bin'))[,Woe_Append(Var_Bin_List)]

MTG_Train[,Var_Bin_List] <- lapply(MTG_Train[,Var_Bin_List] , as.factor)
MTG_Test[,Var_Bin_List] <- lapply(MTG_Test[,Var_Bin_List] , as.factor)
MTG_CV[,Var_Bin_List] <- lapply(MTG_CV[,Var_Bin_List] , as.factor)


#Renaming Organization type to something more readable
MTG_Train[,"ORGANIZATION_TYPE"] <- as.character(MTG_Train[,"ORGANIZATION_TYPE"])
MTG_Test[,"ORGANIZATION_TYPE"] <- as.character(MTG_Test[,"ORGANIZATION_TYPE"])
MTG_CV[,"ORGANIZATION_TYPE"] <- as.character(MTG_CV[,"ORGANIZATION_TYPE"])

MTG_Train[MTG_Train[,"ORGANIZATION_TYPE"] == "Business Entity Type 3%,%Cleaning%,%Construction" ,"ORGANIZATION_TYPE"] <- 'Group 1'
MTG_Train[MTG_Train[,"ORGANIZATION_TYPE"] == "Culture%,%Electricity%,%Emergency%,%Government%,%Hotel%,%Housing%,%Industry: type 1%,%Industry: type 11%,%Industry: type 12%,%Industry: type 2%,%Industry: type 3%,%Industry: type 4%,%Industry: type 5%,%Industry: type 7%,%Industry: type 9%,%Insurance%,%Kindergarten%,%Legal Services%,%Medicine%,%Military%,%Mobile%,%Other%,%Police%,%Postal%,%Realtor%,%Restaurant%,%School%,%Security%,%Security Ministries" ,"ORGANIZATION_TYPE"] <- 'Group 2'
MTG_Train[MTG_Train[,"ORGANIZATION_TYPE"] == "University%,%XNA" ,"ORGANIZATION_TYPE"] <- 'Group 3'
MTG_Train[MTG_Train[,"ORGANIZATION_TYPE"] == "Advertising%,%Agriculture%,%Bank%,%Business Entity Type 1%,%Business Entity Type 2" ,"ORGANIZATION_TYPE"] <- 'Group 4'
MTG_Train[MTG_Train[,"ORGANIZATION_TYPE"] == "Self-employed%,%Services%,%Telecom%,%Trade: type 1%,%Trade: type 2%,%Trade: type 3%,%Trade: type 6%,%Trade: type 7%,%Transport: type 1%,%Transport: type 2%,%Transport: type 3%,%Transport: type 4" ,"ORGANIZATION_TYPE"] <- 'Group 5'

MTG_Test[MTG_Test[,"ORGANIZATION_TYPE"] == "Business Entity Type 3%,%Cleaning%,%Construction" ,"ORGANIZATION_TYPE"] <- 'Group 1'
MTG_Test[MTG_Test[,"ORGANIZATION_TYPE"] == "Culture%,%Electricity%,%Emergency%,%Government%,%Hotel%,%Housing%,%Industry: type 1%,%Industry: type 11%,%Industry: type 12%,%Industry: type 2%,%Industry: type 3%,%Industry: type 4%,%Industry: type 5%,%Industry: type 7%,%Industry: type 9%,%Insurance%,%Kindergarten%,%Legal Services%,%Medicine%,%Military%,%Mobile%,%Other%,%Police%,%Postal%,%Realtor%,%Restaurant%,%School%,%Security%,%Security Ministries" ,"ORGANIZATION_TYPE"] <- 'Group 2'
MTG_Test[MTG_Test[,"ORGANIZATION_TYPE"] == "University%,%XNA" ,"ORGANIZATION_TYPE"] <- 'Group 3'
MTG_Test[MTG_Test[,"ORGANIZATION_TYPE"] == "Advertising%,%Agriculture%,%Bank%,%Business Entity Type 1%,%Business Entity Type 2" ,"ORGANIZATION_TYPE"] <- 'Group 4'
MTG_Test[MTG_Test[,"ORGANIZATION_TYPE"] == "Self-employed%,%Services%,%Telecom%,%Trade: type 1%,%Trade: type 2%,%Trade: type 3%,%Trade: type 6%,%Trade: type 7%,%Transport: type 1%,%Transport: type 2%,%Transport: type 3%,%Transport: type 4" ,"ORGANIZATION_TYPE"] <- 'Group 5'

MTG_CV[MTG_CV[,"ORGANIZATION_TYPE"] == "Business Entity Type 3%,%Cleaning%,%Construction" ,"ORGANIZATION_TYPE"] <- 'Group 1'
MTG_CV[MTG_CV[,"ORGANIZATION_TYPE"] == "Culture%,%Electricity%,%Emergency%,%Government%,%Hotel%,%Housing%,%Industry: type 1%,%Industry: type 11%,%Industry: type 12%,%Industry: type 2%,%Industry: type 3%,%Industry: type 4%,%Industry: type 5%,%Industry: type 7%,%Industry: type 9%,%Insurance%,%Kindergarten%,%Legal Services%,%Medicine%,%Military%,%Mobile%,%Other%,%Police%,%Postal%,%Realtor%,%Restaurant%,%School%,%Security%,%Security Ministries" ,"ORGANIZATION_TYPE"] <- 'Group 2'
MTG_CV[MTG_CV[,"ORGANIZATION_TYPE"] == "University%,%XNA" ,"ORGANIZATION_TYPE"] <- 'Group 3'
MTG_CV[MTG_CV[,"ORGANIZATION_TYPE"] == "Advertising%,%Agriculture%,%Bank%,%Business Entity Type 1%,%Business Entity Type 2" ,"ORGANIZATION_TYPE"] <- 'Group 4'
MTG_CV[MTG_CV[,"ORGANIZATION_TYPE"] == "Self-employed%,%Services%,%Telecom%,%Trade: type 1%,%Trade: type 2%,%Trade: type 3%,%Trade: type 6%,%Trade: type 7%,%Transport: type 1%,%Transport: type 2%,%Transport: type 3%,%Transport: type 4" ,"ORGANIZATION_TYPE"] <- 'Group 5'

MTG_Train[,"ORGANIZATION_TYPE"] <- as.factor(MTG_Train[,"ORGANIZATION_TYPE"])
MTG_Test[,"ORGANIZATION_TYPE"] <- as.factor(MTG_Test[,"ORGANIZATION_TYPE"])
MTG_CV[,"ORGANIZATION_TYPE"] <- as.factor(MTG_CV[,"ORGANIZATION_TYPE"])



#vehicle Loans
Var_Bin_List <- c('')



#Add weight vectors
nrow(MTG_Train)/sum(MTG_Train['TARGET'])
nrow(MTG_Test)/sum(MTG_Test['TARGET'])
nrow(MTG_CV)/sum(MTG_CV['TARGET'])

nrow(RL_Train)/sum(RL_Train['TARGET'])
nrow(RL_Test)/sum(RL_Test['TARGET'])
nrow(RL_CV)/sum(RL_CV['TARGET'])

nrow(IRE_Train)/sum(IRE_Train['TARGET'])
nrow(IRE_Test)/sum(IRE_Test['TARGET'])
nrow(IRE_CV)/sum(IRE_CV['TARGET'])

#All weights are similar so apply training weights like in Scorecard MOdel
#Mortgages
MTG_Train[,'weight'] <- NA
MTG_Train[MTG_Train['TARGET'] == 1,'weight'] <- nrow(MTG_Train)/sum(MTG_Train['TARGET'])
MTG_Train[MTG_Train['TARGET'] == 0,'weight'] <- nrow(MTG_Train)/(nrow(MTG_Train) - sum(MTG_Train['TARGET']))

MTG_Test[,'weight'] <- NA
MTG_Test[MTG_Test['TARGET'] == 1,'weight'] <- nrow(MTG_Train)/sum(MTG_Train['TARGET'])
MTG_Test[MTG_Test['TARGET'] == 0,'weight'] <- nrow(MTG_Train)/(nrow(MTG_Train) - sum(MTG_Train['TARGET']))

MTG_CV[,'weight'] <- NA
MTG_CV[MTG_CV['TARGET'] == 1,'weight'] <- nrow(MTG_Train)/sum(MTG_Train['TARGET'])
MTG_CV[MTG_CV['TARGET'] == 0,'weight'] <- nrow(MTG_Train)/(nrow(MTG_Train) - sum(MTG_Train['TARGET']))


#Vehicles
RL_Train[,'weight'] <- NA
RL_Train[RL_Train['TARGET'] == 1,'weight'] <- nrow(RL_Train)/sum(RL_Train['TARGET'])
RL_Train[RL_Train['TARGET'] == 0,'weight'] <- nrow(RL_Train)/(nrow(RL_Train) - sum(RL_Train['TARGET']))

RL_Test[,'weight'] <- NA
RL_Test[RL_Test['TARGET'] == 1,'weight'] <- nrow(RL_Train)/sum(RL_Train['TARGET'])
RL_Test[RL_Test['TARGET'] == 0,'weight'] <- nrow(RL_Train)/(nrow(RL_Train) - sum(RL_Train['TARGET']))

RL_CV[,'weight'] <- NA
RL_CV[RL_CV['TARGET'] == 1,'weight'] <- nrow(RL_Train)/sum(RL_Train['TARGET'])
RL_CV[RL_CV['TARGET'] == 0,'weight'] <- nrow(RL_Train)/(nrow(RL_Train) - sum(RL_Train['TARGET']))


#Irish Data
IRE_Train[,'weight'] <- NA
IRE_Train[IRE_Train['TARGET'] == 1,'weight'] <- nrow(IRE_Train)/sum(IRE_Train['TARGET'])
IRE_Train[IRE_Train['TARGET'] == 0,'weight'] <- nrow(IRE_Train)/(nrow(IRE_Train) - sum(IRE_Train['TARGET']))

IRE_Test[,'weight'] <- NA
IRE_Test[IRE_Test['TARGET'] == 1,'weight'] <- nrow(IRE_Train)/sum(IRE_Train['TARGET'])
IRE_Test[IRE_Test['TARGET'] == 0,'weight'] <- nrow(IRE_Train)/(nrow(IRE_Train) - sum(IRE_Train['TARGET']))

IRE_CV[,'weight'] <- NA
IRE_CV[IRE_CV['TARGET'] == 1,'weight'] <- nrow(IRE_Train)/sum(IRE_Train['TARGET'])
IRE_CV[IRE_CV['TARGET'] == 0,'weight'] <- nrow(IRE_Train)/(nrow(IRE_Train) - sum(IRE_Train['TARGET']))



#Standardization - Should speed things up!
MTG_Train[,(!names(MTG_Train) %in% 'weight')] <- normalize(MTG_Train[,(!names(MTG_Train) %in% 'weight')], method = 'range', range = c(0, 1))
MTG_Test[,(!names(MTG_Train) %in% 'weight')] <- normalize(MTG_Test[,(!names(MTG_Train) %in% 'weight')], method = 'range', range = c(0, 1))
MTG_CV[,(!names(MTG_Train) %in% 'weight')] <- normalize(MTG_CV[,(!names(MTG_Train) %in% 'weight')], method = 'range', range = c(0, 1))

RL_Train[,(!names(RL_Train) %in% 'weight')] <- normalize(RL_Train[,(!names(RL_Train) %in% 'weight')], method = 'range', range = c(0, 1))
RL_Test[,(!names(RL_Train) %in% 'weight')] <- normalize(RL_Test[,(!names(RL_Train) %in% 'weight')], method = 'range', range = c(0, 1))
RL_CV[,(!names(RL_Train) %in% 'weight')] <- normalize(RL_CV[,(!names(RL_Train) %in% 'weight')], method = 'range', range = c(0, 1))

IRE_Train[,(!names(IRE_Train) %in% c('weight','issue_d'))] <- normalize(IRE_Train[,(!names(IRE_Train) %in% c('weight','issue_d'))], method = 'range', range = c(0, 1))
IRE_Test[,(!names(IRE_Train) %in% c('weight','issue_d'))] <- normalize(IRE_Test[,(!names(IRE_Train) %in% c('weight','issue_d'))], method = 'range', range = c(0, 1))
IRE_CV[,(!names(IRE_Train) %in% c('weight','issue_d'))] <- normalize(IRE_CV[,(!names(IRE_Train) %in% c('weight','issue_d'))], method = 'range', range = c(0, 1))



#Dimensional reduction
MTG_Train <- MTG_Train[,append(MTG_Col_List,'weight')]
MTG_CV <- MTG_CV[,append(MTG_Col_List,'weight')]
MTG_Test <- MTG_Test[,append(MTG_Col_List,'weight')]

IRE_Train <- IRE_Train[,append(append(IRE_Col_List,'weight'),'issue_d')]
IRE_CV <- IRE_CV[,append(append(IRE_Col_List,'weight'),'issue_d')]
IRE_Test <- IRE_Test[,append(append(IRE_Col_List,'weight'),'issue_d')]

RL_Train <- RL_Train[,append(RL_Col_List,'weight')]
RL_CV <- RL_CV[,append(RL_Col_List,'weight')]
RL_Test <- RL_Test[,append(RL_Col_List,'weight')]


#Create H2o Objects
h2o.init(max_mem_size = "12g")

MTG_Train_h2o <- MTG_Train
MTG_Test_h2o <- MTG_Test
MTG_CV_h2o <- MTG_CV

MTG_Train_h2o[,'TARGET'] <- as.factor(MTG_Train_h2o[,'TARGET'])
MTG_Test_h2o[,'TARGET'] <- as.factor(MTG_Test_h2o[,'TARGET'])
MTG_CV_h2o[,'TARGET'] <- as.factor(MTG_CV_h2o[,'TARGET'])

MTG_Train_h2o <- as.h2o(as.data.table(MTG_Train_h2o))
MTG_Test_h2o <- as.h2o(as.data.table(MTG_Test_h2o))
MTG_CV_h2o <- as.h2o(as.data.table(MTG_CV_h2o))


RL_Train_h2o <- RL_Train
RL_Test_h2o <- RL_Test
RL_CV_h2o <- RL_CV

RL_Train_h2o[,'TARGET'] <- as.factor(RL_Train_h2o[,'TARGET'])
RL_Test_h2o[,'TARGET'] <- as.factor(RL_Test_h2o[,'TARGET'])
RL_CV_h2o[,'TARGET'] <- as.factor(RL_CV_h2o[,'TARGET'])

RL_Train_h2o <- as.h2o(as.data.table(RL_Train_h2o))
RL_Test_h2o <- as.h2o(as.data.table(RL_Test_h2o))
RL_CV_h2o <- as.h2o(as.data.table(RL_CV_h2o))


IRE_Train_h2o <- IRE_Train
IRE_Test_h2o <- IRE_Test
IRE_CV_h2o <- IRE_CV

IRE_Train_h2o[,'TARGET'] <- as.factor(IRE_Train_h2o[,'TARGET'])
IRE_Test_h2o[,'TARGET'] <- as.factor(IRE_Test_h2o[,'TARGET'])
IRE_CV_h2o[,'TARGET'] <- as.factor(IRE_CV_h2o[,'TARGET'])

IRE_Train_h2o <- as.h2o(as.data.table(IRE_Train_h2o))
IRE_Test_h2o <- as.h2o(as.data.table(IRE_Test_h2o))
IRE_CV_h2o <- as.h2o(as.data.table(IRE_CV_h2o))
