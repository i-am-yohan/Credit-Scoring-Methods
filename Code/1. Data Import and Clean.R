#1. Importing the data
library('httr')
library('stringi')
library('stringr')
library(sqldf)
library(naniar)


#Check for NAs
NA_Chck <- function(In_Data , exclude = NULL){
  for (i in 1:ncol(In_Data)){
    if (any(is.na(In_Data[,i])) == TRUE & any(exclude == colnames(In_Data)[i]) == FALSE & is.factor(In_Data[,i]) == FALSE) {
    print(colnames(In_Data)[i])
    }
  }
}

NA_Chck(MTG)
NA_Chck(RL)
NA_Chck(IRE)


#Visualize missings
gg_miss_var(MTG[,colSums(is.na(MTG)) != 0], show_pct = TRUE)
#gg_miss_var(RL[,colSums(is.na(RL)) != 0], show_pct = TRUE)
#gg_miss_var(IRE[,colSums(is.na(RL)) != 0], show_pct = TRUE)


#2.Cleaning the data
#2.1 Mortgages Data
#2.1.1 the annuity variable

#Regression fill function
NA_LM_Reg_Fill <- function(expl , preds , In_Data){
  idx_na <- is.na(In_Data[expl])
  expl_train <- In_Data[-idx_na,]
  expl_test <- In_Data[idx_na,]
  
  Preds_form <- paste(preds , collapse = '+')
  Full_form <- paste(expl , '~' , Preds_form)
  
  Formula = eval(parse(text = Full_form))

  REG <- lm(formula = Formula , data = expl_train)
  Pred <- predict(REG , newdata = expl_test)
  
  In_Data[idx_na,expl] <- Pred
  Out <- In_Data
  return(Out)
  
}

MTG <- NA_LM_Reg_Fill(expl = 'AMT_ANNUITY'
              ,preds = c('AMT_CREDIT','AMT_INCOME_TOTAL')
              ,In_Data = MTG
               )


#2.1.2 The AMT_GOODS_PRICE variable
MTG <- NA_LM_Reg_Fill(expl = 'AMT_GOODS_PRICE'
                      ,preds = 'AMT_CREDIT'
                      ,In_Data = MTG
)


#2.1.3 The Own Car variable
#Count the blanks
OCA_Idx <- is.na(MTG['OWN_CAR_AGE'])
nrow(MTG[OCA_Idx,])

#Count blanks where Car OWner is Y
OCA_Idx <- is.na(MTG['OWN_CAR_AGE']) & MTG['FLAG_OWN_CAR'] == 'Y'
nrow(MTG[OCA_Idx,])

med <- median(as.matrix(MTG['OWN_CAR_AGE']) , na.rm = TRUE)

#infill with median - Ok because number of blanks is so small
MTG[OCA_Idx,'OWN_CAR_AGE'] <- med

#Check where car age is populated and car owner is N
OCA_Idx <- !is.na(MTG['OWN_CAR_AGE']) & MTG['FLAG_OWN_CAR'] == 'N'
nrow(MTG[OCA_Idx,]) #Ok no cleaning needed!
#Blanks left to accomadate people with no car some binning may be required later.



#2.1.4 The number of family members
FAM_Idx <- is.na(MTG['CNT_FAM_MEMBERS'])
nrow(MTG[FAM_Idx,])
MTG[FAM_Idx , 'CNT_FAM_MEMBERS'] <- 0 #Fill with 0 because Only Two rows blank


#2.1.5 EXT_SOURCE_1, EXT_SOURCE_2, EXT_SOURCE_3 - scores from external source
#Places where they are all blank
SCR_Idx <- is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3'])
nrow(MTG[SCR_Idx,])

#identifying blanks
SCR_GB <- MTG[c('EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')]
SCR_GB['Ind'] <- NA
SCR_GB['Ind'][is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3']),] <- 'All Blank'
SCR_GB['Ind'][!is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 1 populated only'
SCR_GB['Ind'][is.na(MTG['EXT_SOURCE_1']) & !is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 2 populated only'
SCR_GB['Ind'][is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & !is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 3 populated only'
SCR_GB['Ind'][!is.na(MTG['EXT_SOURCE_1']) & !is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 1 & 2 populated only'
SCR_GB['Ind'][!is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & !is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 1 & 3 populated only'
SCR_GB['Ind'][is.na(MTG['EXT_SOURCE_1']) & !is.na(MTG['EXT_SOURCE_2']) & !is.na(MTG['EXT_SOURCE_3']),] <- 'EXT_SOURCE 2 & 3 populated only'
SCR_GB['Ind'][!is.na(MTG['EXT_SOURCE_1']) & !is.na(MTG['EXT_SOURCE_2']) & !is.na(MTG['EXT_SOURCE_3']),] <- 'All Populated'
SCR_GB <- SCR_GB['Ind']
table(SCR_GB$Ind)


#Use other scores to predict eachoter?
cor(MTG[,c('TARGET','EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')] , use = 'complete.obs')

aggregate(MTG[, c('EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')], list(MTG$TARGET), mean ,na.rm=TRUE)

#Use row average
MTG['Avg_Ext_Scre'] <- rowMeans(MTG[ c('EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')] , na.rm=TRUE)
MTG[ c('Avg_Ext_Scre','EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')]

Means <- aggregate(MTG['Avg_Ext_Scre'], list(MTG$TARGET) , mean , na.rm=TRUE)

MTG[is.na(MTG['Avg_Ext_Scre']) & MTG['TARGET'] == 1,'Avg_Ext_Scre'] <- Means[Means['Group.1'] == 1 ,'Avg_Ext_Scre']
MTG[is.na(MTG['Avg_Ext_Scre']) & MTG['TARGET'] == 0,'Avg_Ext_Scre'] <- Means[Means['Group.1'] == 0 ,'Avg_Ext_Scre']

sum(is.na(MTG['Avg_Ext_Scre']))

#Engineer a new feature
MTG['Credit_History'] <- ''
MTG[!(is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3'])),'Credit_History'] <- 'Full'
MTG[is.na(MTG['EXT_SOURCE_1']) | is.na(MTG['EXT_SOURCE_2']) | is.na(MTG['EXT_SOURCE_3']),'Credit_History'] <- 'Partial'
MTG[is.na(MTG['EXT_SOURCE_1']) & is.na(MTG['EXT_SOURCE_2']) & is.na(MTG['EXT_SOURCE_3']),'Credit_History'] <- 'None'
MTG[,'Credit_History'] <- as.factor(MTG[,'Credit_History'])


#2.1.6 Drop redundant columns
drops <- c('EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')
MTG <- MTG[,!(names(MTG) %in% drops)]





#2.1.7 "Social Circle" stuff -  replace with 0s
MTG[is.na(MTG['OBS_30_CNT_SOCIAL_CIRCLE']),'OBS_30_CNT_SOCIAL_CIRCLE'] <- 0
MTG[is.na(MTG['DEF_30_CNT_SOCIAL_CIRCLE']),'DEF_30_CNT_SOCIAL_CIRCLE'] <- 0
MTG[is.na(MTG['OBS_60_CNT_SOCIAL_CIRCLE']),'OBS_60_CNT_SOCIAL_CIRCLE'] <- 0
MTG[is.na(MTG['DEF_60_CNT_SOCIAL_CIRCLE']),'DEF_60_CNT_SOCIAL_CIRCLE'] <- 0


#2.1.8 Days since last phone change
sum(is.na(MTG['DAYS_LAST_PHONE_CHANGE']))
MTG[is.na(MTG['DAYS_LAST_PHONE_CHANGE']) , 'DAYS_LAST_PHONE_CHANGE'] <- median(as.matrix(MTG['DAYS_LAST_PHONE_CHANGE']) , na.rm = TRUE)


#2.1.9 AMT_REQ_CREDIT variables
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_HOUR']))
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_DAY']))
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_WEEK']))
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_MON']))
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_QRT']))
sum(is.na(MTG['AMT_REQ_CREDIT_BUREAU_YEAR']))

#Infilling missings with 0s
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_HOUR']),'AMT_REQ_CREDIT_BUREAU_HOUR'] <- 0
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_DAY']),'AMT_REQ_CREDIT_BUREAU_DAY'] <- 0
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_WEEK']),'AMT_REQ_CREDIT_BUREAU_WEEK'] <- 0
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_MON']),'AMT_REQ_CREDIT_BUREAU_MON'] <- 0
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_QRT']),'AMT_REQ_CREDIT_BUREAU_QRT'] <- 0
MTG[is.na(MTG['AMT_REQ_CREDIT_BUREAU_YEAR']),'AMT_REQ_CREDIT_BUREAU_YEAR'] <- 0

#2.1.10 Put all neccesary columns as factors
#MTG$AMT_REQ_CREDIT_BUREAU_HOUR <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_HOUR)
#MTG$AMT_REQ_CREDIT_BUREAU_DAY <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_DAY)
#MTG$AMT_REQ_CREDIT_BUREAU_WEEK <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_WEEK)
#MTG$AMT_REQ_CREDIT_BUREAU_MON <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_MON)
#MTG$AMT_REQ_CREDIT_BUREAU_QRT <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_QRT)
#MTG$AMT_REQ_CREDIT_BUREAU_YEAR <- as.factor(MTG$AMT_REQ_CREDIT_BUREAU_YEAR)
MTG$FLAG_MOBIL <- as.factor(MTG$FLAG_MOBIL)
MTG$FLAG_WORK_PHONE <- as.factor(MTG$FLAG_WORK_PHONE)
MTG$FLAG_CONT_MOBILE <- as.factor(MTG$FLAG_CONT_MOBILE)
MTG$FLAG_PHONE <- as.factor(MTG$FLAG_PHONE)
MTG$FLAG_EMP_PHONE <- as.factor(MTG$FLAG_EMP_PHONE)
MTG$FLAG_EMAIL <- as.factor(MTG$FLAG_EMAIL)
MTG$CNT_CHILDREN <- as.factor(MTG$CNT_CHILDREN)
MTG$CNT_FAM_MEMBERS <- as.factor(MTG$CNT_FAM_MEMBERS)
MTG$REGION_RATING_CLIENT <- as.factor(MTG$REGION_RATING_CLIENT)
MTG$REGION_RATING_CLIENT_W_CITY <- as.factor(MTG$REGION_RATING_CLIENT_W_CITY)
MTG$HOUR_APPR_PROCESS_START <- as.factor(MTG$HOUR_APPR_PROCESS_START)
MTG$REG_REGION_NOT_LIVE_REGION <- as.factor(MTG$REG_REGION_NOT_LIVE_REGION)
MTG$REG_REGION_NOT_WORK_REGION <- as.factor(MTG$REG_REGION_NOT_LIVE_REGION)
MTG$LIVE_REGION_NOT_WORK_REGION <- as.factor(MTG$LIVE_REGION_NOT_WORK_REGION)
MTG$REG_CITY_NOT_LIVE_CITY <- as.factor(MTG$REG_CITY_NOT_LIVE_CITY)
MTG$REG_CITY_NOT_WORK_CITY <- as.factor(MTG$REG_CITY_NOT_WORK_CITY)
MTG$LIVE_CITY_NOT_WORK_CITY <- as.factor(MTG$LIVE_CITY_NOT_WORK_CITY)
MTG$OBS_30_CNT_SOCIAL_CIRCLE <- as.factor(MTG$OBS_30_CNT_SOCIAL_CIRCLE)
MTG$DEF_30_CNT_SOCIAL_CIRCLE <- as.factor(MTG$DEF_30_CNT_SOCIAL_CIRCLE)
MTG$OBS_60_CNT_SOCIAL_CIRCLE <- as.factor(MTG$OBS_60_CNT_SOCIAL_CIRCLE)
MTG$DEF_60_CNT_SOCIAL_CIRCLE <- as.factor(MTG$DEF_60_CNT_SOCIAL_CIRCLE)
MTG$DEF_60_CNT_SOCIAL_CIRCLE <- as.factor(MTG$DEF_60_CNT_SOCIAL_CIRCLE)
MTG$FLAG_DOCUMENT_2 <- as.factor(MTG$FLAG_DOCUMENT_2)
MTG$FLAG_DOCUMENT_3 <- as.factor(MTG$FLAG_DOCUMENT_3)
MTG$FLAG_DOCUMENT_4 <- as.factor(MTG$FLAG_DOCUMENT_4)
MTG$FLAG_DOCUMENT_5 <- as.factor(MTG$FLAG_DOCUMENT_5)
MTG$FLAG_DOCUMENT_6 <- as.factor(MTG$FLAG_DOCUMENT_6)
MTG$FLAG_DOCUMENT_7 <- as.factor(MTG$FLAG_DOCUMENT_7)
MTG$FLAG_DOCUMENT_8 <- as.factor(MTG$FLAG_DOCUMENT_8)
MTG$FLAG_DOCUMENT_9 <- as.factor(MTG$FLAG_DOCUMENT_9)
MTG$FLAG_DOCUMENT_10 <- as.factor(MTG$FLAG_DOCUMENT_10)
MTG$FLAG_DOCUMENT_11 <- as.factor(MTG$FLAG_DOCUMENT_11)
MTG$FLAG_DOCUMENT_12 <- as.factor(MTG$FLAG_DOCUMENT_12)
MTG$FLAG_DOCUMENT_13 <- as.factor(MTG$FLAG_DOCUMENT_13)
MTG$FLAG_DOCUMENT_14 <- as.factor(MTG$FLAG_DOCUMENT_14)
MTG$FLAG_DOCUMENT_15 <- as.factor(MTG$FLAG_DOCUMENT_15)
MTG$FLAG_DOCUMENT_16 <- as.factor(MTG$FLAG_DOCUMENT_16)
MTG$FLAG_DOCUMENT_17 <- as.factor(MTG$FLAG_DOCUMENT_17)
MTG$FLAG_DOCUMENT_18 <- as.factor(MTG$FLAG_DOCUMENT_18)
MTG$FLAG_DOCUMENT_19 <- as.factor(MTG$FLAG_DOCUMENT_19)
MTG$FLAG_DOCUMENT_20 <- as.factor(MTG$FLAG_DOCUMENT_20)
MTG$FLAG_DOCUMENT_21 <- as.factor(MTG$FLAG_DOCUMENT_21)



#2.1.10 NA check before binning
NA_Excl_Idx <- colnames(MTG)[str_detect(colnames(MTG) , '_AVG') & sapply(MTG, class) == 'numeric']
NA_Chck(MTG, exclude = NA_Excl_Idx)

drops = c(
  "APARTMENTS_MODE"	,
  "BASEMENTAREA_MODE"	,
  "ELEVATORS_MODE"	,
  "LANDAREA_MODE"	,
  "NONLIVINGAREA_MODE"	,
  "YEARS_BUILD_MEDI"	,
  "FLOORSMAX_MEDI"	,
  "LIVINGAREA_MEDI"	,
  "YEARS_BEGINEXPLUATATION_MODE"	,
  "ENTRANCES_MODE"	,
  "LIVINGAPARTMENTS_MODE"	,
  "APARTMENTS_MEDI"	,
  "COMMONAREA_MEDI"	,
  "FLOORSMIN_MEDI"	,
  "NONLIVINGAPARTMENTS_MEDI"	,
  "YEARS_BUILD_MODE"	,
  "FLOORSMAX_MODE"	,
  "LIVINGAREA_MODE"	,
  "BASEMENTAREA_MEDI"	,
  "ELEVATORS_MEDI"	,
  "LANDAREA_MEDI"	,
  "NONLIVINGAREA_MEDI"	,
  "COMMONAREA_MODE"	,
  "FLOORSMIN_MODE"	,
  "NONLIVINGAPARTMENTS_MODE"	,
  "YEARS_BEGINEXPLUATATION_MEDI"	,
  "ENTRANCES_MEDI"	,
  "LIVINGAPARTMENTS_MEDI"	
)

MTG <- MTG[,!(names(MTG) %in% drops)]


#2.1.11 Check for duplicates
#sum(duplicated(MTG['SK_ID_CURR'])) #No Dups

#Create for the dashboard visualization
MTG[,'TARGET_CHAR'] <- str_replace(str_replace(as.factor(MTG[,'TARGET']),'0','Non-Default'),'1','Default')


#2.2 the Vehicle Loans Dataset
#2.2.1 Check for blanks
NA_Chck(RL) #No Blank numeric variables

#2.2.2 Changing to date functions
RL$DATE_OF_BIRTH <- as.Date(RL$DATE_OF_BIRTH , format="%d-%m-%Y")
RL$DISBURSAL_DATE <- as.Date(RL$DISBURSAL_DATE , format="%d-%m-%Y")


#2.2.3 Yrs columns, Character to Numeric
Yrs_Char2Num <- function(In_Col){
  In_Col_alt <- str_remove(In_Col , 'yrs')
  In_Col_alt <- str_remove(In_Col_alt , 'mon')

  Mat <- str_split_fixed(In_Col_alt, " ", 2)
  Mat <- apply(Mat , 2, as.numeric)

  Out <- Mat[,1] + Mat[,2]/12
  return(Out)
  }

RL$AVERAGE_ACCT_AGE <- Yrs_Char2Num(RL$AVERAGE_ACCT_AGE)
RL$CREDIT_HISTORY_LENGTH <- Yrs_Char2Num(RL$CREDIT_HISTORY_LENGTH)

#2.2.4 Check ID for Duplicates
#sum(duplicated(RL['UNIQUEID'])) #No Dups

names(RL)[names(RL) == 'LOAN_DEFAULT'] <- 'TARGET'
RL[,'TARGET_CHAR'] <- str_replace(str_replace(as.factor(RL[,'TARGET']),'0','Non-Default'),'1','Default')

#2.2.5 Change Necessary variable to factors
RL$BRANCH_ID <- as.factor(RL$BRANCH_ID)
RL$SUPPLIER_ID <- as.factor(RL$SUPPLIER_ID)
RL$MANUFACTURER_ID <- as.factor(RL$MANUFACTURER_ID)
RL$CURRENT_PINCODE_ID <- as.factor(RL$CURRENT_PINCODE_ID)
RL$STATE_ID <- as.factor(RL$STATE_ID)
RL$MOBILENO_AVL_FLAG <- as.factor(RL$MOBILENO_AVL_FLAG)
RL$AADHAR_FLAG <- as.factor(RL$AADHAR_FLAG)
RL$PAN_FLAG <- as.factor(RL$PAN_FLAG)
RL$VOTERID_FLAG <- as.factor(RL$VOTERID_FLAG)
RL$DRIVING_FLAG <- as.factor(RL$DRIVING_FLAG)
RL$PASSPORT_FLAG <- as.factor(RL$PASSPORT_FLAG)



#2.3 The ROI Loans dataset
#2.3.1 Check for blanks and Dups
NA_Chck(IRE) #No Blank numeric variables
#sum(duplicated(RL['UNIQUEID'])) #No Dups

#2.3.2 Clean Date columns
IRE$issue_d <- as.Date(IRE$issue_d , format="%d/%m/%Y")
IRE$final_d <- as.Date(paste('0',as.character(IRE$final_d) , sep = '')  , format="%d%m%Y")

#2.3.3 Drop Unnessecary columns
sqldf('
select 
  income_cat
, count(*) as num_Lns
from IRE
group by 1
')


drops <- c('loan_condition','interest_payment_cat','application_type_cat','home_ownership_cat','term_cat','grade_cat','purpose_cat','income_cat')
IRE <- IRE[,!(names(IRE) %in% drops)]

names(IRE)[names(IRE) == 'loan_condition_cat'] <- 'TARGET'
IRE[,'TARGET_CHAR'] <- str_replace(str_replace(as.factor(IRE[,'TARGET']),'0','Non-Default'),'1','Default')



