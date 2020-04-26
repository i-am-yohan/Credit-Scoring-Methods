library(ggplot2)
library(bsts)
library(sqldf)
library(caret)
library(PerformanceAnalytics)
library(rpart)
library(rpart.plot)
library(grDevices)
library(Rfast)
library(reshape)
library(cowplot)

str(MTG)

#3.Checking Various behaviour of the data
#A visualization of all the plots
#The numeric variables
#excluded because they are useless
excl<- c("AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK","AMT_REQ_CREDIT_BUREAU_MON","AMT_REQ_CREDIT_BUREAU_QRT","AMT_REQ_CREDIT_BUREAU_YEAR")
theme_set(theme_cowplot(font_size=8))
MTG_list <-lapply(names(MTG[,sapply(MTG, class) == 'numeric' & !(names(MTG) %in% excl)]),
              function(col) ggplot(data=MTG) 
              + geom_histogram(aes_string(x=col, color='TARGET_CHAR')
                               , fill='white'
                               , bins=30
                               , position = position_stack(reverse = TRUE))
              + theme(legend.position = "none")
              )
cowplot::plot_grid(plotlist = MTG_list)

#The factors
excl<- c("TARGET_CHAR",
         "FLAG_DOCUMENT_3"	,
         "FLAG_DOCUMENT_5"	,
         "FLAG_DOCUMENT_9"	,
         "FLAG_DOCUMENT_13"	,
         "FLAG_DOCUMENT_17"	,
         "FLAG_DOCUMENT_21"	,
         "FLAG_DOCUMENT_4"	,
         "FLAG_DOCUMENT_6"	,
         "FLAG_DOCUMENT_10"	,
         "FLAG_DOCUMENT_14"	,
         "FLAG_DOCUMENT_18"	,
         "FLAG_DOCUMENT_7"	,
         "FLAG_DOCUMENT_11"	,
         "FLAG_DOCUMENT_15"	,
         "FLAG_DOCUMENT_19"	,
         "FLAG_DOCUMENT_8"	,
         "FLAG_DOCUMENT_12"	,
         "FLAG_DOCUMENT_16"	,
         "FLAG_DOCUMENT_20"	,
         'FLAG_EMAIL',
         'FLAG_PHONE',
         'FLAG_MOBIL',
         'FLAG_OWN_CAR',
         'FLAG_OWN_REALTY',
         'FLAG_EMP_PHONE',
         'FLAG_WORK_PHONE'
)
MTG_list <-lapply(names(MTG[,sapply(MTG, class) == 'factor' & !(names(MTG) %in% excl)]),
                  function(col) ggplot(data=MTG) 
                  + geom_bar(aes_string(x=col, color='TARGET_CHAR')
                                  , fill = 'white'
                                   , position = position_stack(reverse = TRUE))
                  + theme(legend.position = "none")
)
cowplot::plot_grid(plotlist = MTG_list)


#3.2 Analyse correlations of numeric variables
#Identify high correlations given that 
cor_table = as.data.frame(as.table(cor(MTG[,sapply(MTG, class) == 'numeric'] , use = 'complete.obs'))) #A lot of highly correlated variables there, dimensional reduction required
cor_table[cor_table['Freq'] > 0.7 & cor_table['Freq'] != 1,] #A couple here, might be reduced


cor(IRE[,sapply(IRE, class) == 'numeric'] , use = 'complete.obs') #Two highly correlated variables here

#3.3 combining variables for MTG
#3.3.1 LTV
reduc1 <- glm(TARGET ~ AMT_GOODS_PRICE + AMT_CREDIT + AMT_GOODS_PRICE*AMT_CREDIT, data = MTG, family = "binomial")
summary(reduc1) #Parrallel lines observed

MTG['LTV'] <- MTG['AMT_CREDIT']/MTG['AMT_GOODS_PRICE'] #A bit subjective, Lets assume no installments or change in house price! I know I know!!


#3.3.2 PAyment ratio
reduc2 <- glm(TARGET ~ AMT_ANNUITY + AMT_INCOME_TOTAL + AMT_ANNUITY*AMT_INCOME_TOTAL, data = MTG, family = "binomial")
summary(reduc2) #Parrallel lines not observed

MTG['PAYMENT_INCOME_RATIO'] <- MTG['AMT_ANNUITY']/MTG['AMT_INCOME_TOTAL']


#3.5 Looking at data structure - should have been done first I know!!


str(IRE)


#3.6 Understanding data - Decision trees
#MTG_Tree <- rpart(TARGET~., data = MTG, method = 'class') #No info there
#rpart.plot(MTG_Tree)

#RL_Tree <- rpart(LOAN_DEFAULT~., data = RL, method = 'class') #No info there
#rpart.plot(RL_Tree)

#IRE_Tree <- rpart(loan_condition_cat~., data = IRE, method = 'class')
#rpart.plot(IRE_Tree) #Very useful


#3.7 Exploration - Lets see what's going on
#3.7.1 Mortgages data
#3.7.1.1 Income
MTG <- MTG[MTG['AMT_INCOME_TOTAL'] != max(MTG['AMT_INCOME_TOTAL']), ] #Remove Top
#
MTG['AMT_INCOME_TOTAL_log'] <- log(1 + MTG['AMT_INCOME_TOTAL']) #Log of 1+ to compensat for 0 values
drops <- 'AMT_INCOME_TOTAL'
MTG <- MTG[,!(names(MTG) %in% drops)]


#3.7.1.2 Annual Repayment
boxplot(MTG$AMT_ANNUITY)
boxplot(log(1+MTG$AMT_ANNUITY))

MTG['AMT_ANNUITY_log'] <- log(1+MTG['AMT_ANNUITY'])



#3.7.1.3 Population
boxplot(MTG$REGION_POPULATION_RELATIVE) #outlier - shouldn't remove because there is so many!


#3.7.1.4 The "AVG" Variables. These will have to be feature engineered
#Might do this later using Scorecard binning



#3.7.1.5 Social Circle Varaibles
#Observed
sqldf('select 
  OBS_30_CNT_SOCIAL_CIRCLE
, OBS_60_CNT_SOCIAL_CIRCLE
, count(*) as num_soc_circ
from MTG
group by 1,2
') #not much info there

sqldf('select
count(*) as num_soc_circ
from MTG
where OBS_30_CNT_SOCIAL_CIRCLE != OBS_60_CNT_SOCIAL_CIRCLE
') #not a lot of contrasts

sqldf('select 
  OBS_30_CNT_SOCIAL_CIRCLE
, OBS_60_CNT_SOCIAL_CIRCLE
, count(*) as num_soc_circ
, sum(TARGET) as Num
from MTG
where OBS_30_CNT_SOCIAL_CIRCLE != OBS_60_CNT_SOCIAL_CIRCLE
group by 1,2
') #Not sure what to do


#OBS vs DEF
sqldf('select
count(*) as num_soc_circ
from MTG
where OBS_30_CNT_SOCIAL_CIRCLE != DEF_30_CNT_SOCIAL_CIRCLE
') #Many differences keep these features separate


#3.7.1.6 Credit Enquires
cor(MTG[c('AMT_REQ_CREDIT_BUREAU_HOUR'
          ,'AMT_REQ_CREDIT_BUREAU_DAY'
          ,'AMT_REQ_CREDIT_BUREAU_WEEK'
          ,'AMT_REQ_CREDIT_BUREAU_MON'
          ,'AMT_REQ_CREDIT_BUREAU_QRT'
          ,'AMT_REQ_CREDIT_BUREAU_YEAR'
          )])

sqldf('select
      count(*) from MTG
      where AMT_REQ_CREDIT_BUREAU_HOUR = 0 and AMT_REQ_CREDIT_BUREAU_HOUR != 0
      ') #No values


sqldf('select 
  case when AMT_REQ_CREDIT_BUREAU_HOUR = 0 then 0 else 1 end as HOUR
, case when AMT_REQ_CREDIT_BUREAU_DAY = 0 then 0 else 1 end as DAY
, case when AMT_REQ_CREDIT_BUREAU_WEEK = 0 then 0 else 1 end as WEEK
, case when AMT_REQ_CREDIT_BUREAU_MON = 0 then 0 else 1 end as MON
, case when AMT_REQ_CREDIT_BUREAU_QRT = 0 then 0 else 1 end as QRT
, case when AMT_REQ_CREDIT_BUREAU_YEAR = 0 then 0 else 1 end as YEAR
, count(*) as num
, sum(TARGET) as Num_Defaults
from MTG
group by 1,2,3,4,5,6
') #Need to combine these variables to someting more concise

MTG['AMT_REQ_CREDIT_BUREAU_TOTAL'] = MTG[,'AMT_REQ_CREDIT_BUREAU_HOUR'] +
  MTG[,'AMT_REQ_CREDIT_BUREAU_DAY'] +
  MTG[,'AMT_REQ_CREDIT_BUREAU_WEEK'] +
  MTG[,'AMT_REQ_CREDIT_BUREAU_MON'] + 
  MTG[,'AMT_REQ_CREDIT_BUREAU_QRT'] + 
  MTG[,'AMT_REQ_CREDIT_BUREAU_YEAR']

#Create new feature to indicate most recent Credit BEureau Enquiry
MTG[MTG['AMT_REQ_CREDIT_BUREAU_HOUR'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '0-1 Hour'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_DAY'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '1 Hour - 1 Day'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_WEEK'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '1 Day - 1 Week'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_MON'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '1 Week - 1 Month'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_QRT'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '1 Month - 3 Months'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_YEAR'] != 0 , 'AMT_REQ_CREDIT_LATEST'] <- '3 Months - 1 Year'
MTG[is.na(MTG['AMT_REQ_CREDIT_LATEST']) , 'AMT_REQ_CREDIT_LATEST'] <- 'None'
MTG[,'AMT_REQ_CREDIT_LATEST'] <- as.factor(MTG[,'AMT_REQ_CREDIT_LATEST'])

#Create new feature to indicate first Credit BEureau Enquiry
MTG[MTG['AMT_REQ_CREDIT_BUREAU_YEAR'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '3 Months - 1 Year'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_QRT'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '1 Month - 3 Months'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_MON'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '1 Week - 1 Month'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_WEEK'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '1 Day - 1 Week'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_DAY'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '1 Hour - 1 Day'
MTG[MTG['AMT_REQ_CREDIT_BUREAU_HOUR'] != 0 , 'AMT_REQ_CREDIT_FIRST'] <- '0-1 Hour'
MTG[is.na(MTG['AMT_REQ_CREDIT_FIRST']) , 'AMT_REQ_CREDIT_FIRST'] <- 'None'
MTG[,'AMT_REQ_CREDIT_FIRST'] <- as.factor(MTG[,'AMT_REQ_CREDIT_FIRST'])

drops <- c('AMT_REQ_CREDIT_BUREAU_YEAR'
           ,'AMT_REQ_CREDIT_BUREAU_QRT'
           ,'AMT_REQ_CREDIT_BUREAU_MON'
           ,'AMT_REQ_CREDIT_BUREAU_WEEK'
           ,'AMT_REQ_CREDIT_BUREAU_DAY'
           ,'AMT_REQ_CREDIT_BUREAU_HOUR'
)
MTG <- MTG[,!(names(MTG) %in% drops)]



#3.7.1.7 Time Variables
MTG['DAYS_BIRTH'] <- MTG['DAYS_BIRTH']/-365.25 #change to age
names(MTG)[names(MTG) == 'DAYS_BIRTH'] <- 'AGE'

MTG[MTG['DAYS_EMPLOYED'] == 365243 ,'DAYS_EMPLOYED'] <- NA
MTG['DAYS_EMPLOYED'] <- MTG['DAYS_EMPLOYED']/-365.25
names(MTG)[names(MTG) == 'DAYS_EMPLOYED'] <- 'YRS_EMPLOYED'

MTG['DAYS_REGISTRATION'] <- MTG['DAYS_REGISTRATION']/-365.25
names(MTG)[names(MTG) == 'DAYS_REGISTRATION'] <- 'YRS_REGISTRATION'

MTG['DAYS_ID_PUBLISH'] <- MTG['DAYS_ID_PUBLISH']/-365.25
names(MTG)[names(MTG) == 'DAYS_ID_PUBLISH'] <- 'YRS_ID_PUBLISH'



#3.7.1.8 Count Children
MTG[,'CNT_CHILDREN'] <- as.character(MTG[,'CNT_CHILDREN'])
MTG[is.na(MTG['CNT_CHILDREN']) , 'CNT_CHILDREN'] <- 'Not Specified'
MTG[as.numeric(MTG[,'CNT_CHILDREN']) >= 3, 'CNT_CHILDREN'] <- '3 or more' #Group all large families unnessesary together
MTG[,'CNT_CHILDREN'] <- as.factor(MTG[,'CNT_CHILDREN'])

#3.7.1.9 Name Type Suite - Who accompanies the loan applicant
MTG[,'NAME_TYPE_SUITE'] <- as.character(MTG[,'NAME_TYPE_SUITE'])
MTG[MTG[,'NAME_TYPE_SUITE'] == 'Children', 'NAME_TYPE_SUITE'] <- 'Family' #Children count as family right?
Accomp_list <- c('Other_A','Other_B','Group of people','')
MTG[MTG[,'NAME_TYPE_SUITE'] %in% Accomp_list, 'NAME_TYPE_SUITE'] <- 'Other' 
MTG[,'NAME_TYPE_SUITE'] <- as.factor(MTG[,'NAME_TYPE_SUITE'])

#3.7.1.10 Income Type
MTG[,'NAME_INCOME_TYPE'] <- as.character(MTG[,'NAME_INCOME_TYPE'])
inc_list <- c('Student','Unemployed','Maternity leave','Businessman')
MTG[MTG[,'NAME_INCOME_TYPE'] %in% inc_list, 'NAME_INCOME_TYPE'] <- 'No Stable Income' 
MTG[,'NAME_INCOME_TYPE'] <- as.factor(MTG[,'NAME_INCOME_TYPE'])

#3.7.1.11 Education Type
MTG[,'NAME_EDUCATION_TYPE'] <- as.character(MTG[,'NAME_EDUCATION_TYPE'])
MTG[MTG[,'NAME_EDUCATION_TYPE'] == 'Academic degree', 'NAME_EDUCATION_TYPE'] <- 'Higher education' #Academic Degree and higher education are the same thing
MTG[,'NAME_EDUCATION_TYPE'] <- as.factor(MTG[,'NAME_EDUCATION_TYPE'])

#3.8.1.12 Family Status - Dont know how to couple this?

#3.8.1.13 Housing type - Dont know how to group this also?

#3.8.1.14 flag mobile - always 1, gonna remove this
drops <- c('FLAG_MOBIL','FLAG_CONT_MOBILE')
MTG <- MTG[,!(names(MTG) %in% drops)]


#3.8.1.15 Count Family members
MTG[,'CNT_FAM_MEMBERS'] <- as.character(MTG[,'CNT_FAM_MEMBERS'])
MTG[as.numeric(MTG[,'CNT_FAM_MEMBERS']) >= 5, 'CNT_FAM_MEMBERS'] <- '5 or more'
MTG[MTG[,'CNT_FAM_MEMBERS'] == '0', 'CNT_FAM_MEMBERS'] <- '1'
MTG[,'CNT_FAM_MEMBERS'] <- as.factor(MTG[,'CNT_FAM_MEMBERS'])


#See how it compares to number of children
sqldf('select 
  CNT_FAM_MEMBERS
, CNT_Children
, count(*) as num_soc_circ
from MTG
group by 1,2
order by 1,2
')

#We can engineer a variable out of this
MTG[,'Single_Parent_Ind'] <- NA
MTG[MTG[,'CNT_FAM_MEMBERS'] == '2' & MTG[,'CNT_CHILDREN'] == '1','Single_Parent_Ind'] <- 1
MTG[MTG[,'CNT_FAM_MEMBERS'] == '3' & MTG[,'CNT_CHILDREN'] == '2','Single_Parent_Ind'] <- 1
MTG[MTG[,'CNT_FAM_MEMBERS'] == '4' & MTG[,'CNT_CHILDREN'] == '3','Single_Parent_Ind'] <- 1
MTG[is.na(MTG[,'Single_Parent_Ind']),'Single_Parent_Ind'] <- 0
MTG[,'Single_Parent_Ind'] <- as.factor(MTG[,'Single_Parent_Ind'])


#3.8.1.16 Organization type - graph not that useful
ORG_DF <- sqldf('select 
  ORGANIZATION_TYPE
, count(*) as num
, sum(TARGET) as DFs
from MTG
group by 1
order by 1
')

ORG_DF['DR'] <- ORG_DF['DFs']/ORG_DF['num']
ORG_DF['Prop'] <- ORG_DF['num']/nrow(MTG)
ORG_DF #Lots of categories

#Test if the default Rates are the same and couple similar jobs together:
#COmbine Smaller groups into ones with closest risk profile
MTG[,'ORGANIZATION_TYPE'] <- as.character(MTG[,'ORGANIZATION_TYPE'])
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Trade: type 4','ORGANIZATION_TYPE'] <- 'Trade: type 6'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Trade: type 5','ORGANIZATION_TYPE'] <- 'Trade: type 2'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Industry: type 8','ORGANIZATION_TYPE'] <- 'Industry: type 1'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Industry: type 13','ORGANIZATION_TYPE'] <- 'Industry: type 1'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Industry: type 10','ORGANIZATION_TYPE'] <- 'Industry: type 9'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Industry: type 6','ORGANIZATION_TYPE'] <- 'Industry: type 2'
MTG[MTG[,'ORGANIZATION_TYPE'] == 'Religion','ORGANIZATION_TYPE'] <- 'Culture'


#Redo - to check
ORG_DF <- sqldf('select 
  ORGANIZATION_TYPE
, count(*) as num
, sum(TARGET) as DFs
from MTG
group by 1
order by 1
')

ORG_DF['DR'] <- ORG_DF['DFs']/ORG_DF['num']
ORG_DF['Prop'] <- ORG_DF['num']/nrow(MTG)
ORG_DF #Lots of categories still - might be combined further

MTG[,'ORGANIZATION_TYPE'] <- as.factor(MTG[,'ORGANIZATION_TYPE'])


#3.8.1.17 FONDKAPREMONT_MODE Whatever that is?
MTG[,'FONDKAPREMONT_MODE'] <- as.character(MTG[,'FONDKAPREMONT_MODE'])
MTG[MTG[,'FONDKAPREMONT_MODE']=='','FONDKAPREMONT_MODE'] <- 'not specified' #Missing and not specified are the same
MTG[,'FONDKAPREMONT_MODE'] <- as.factor(MTG[,'FONDKAPREMONT_MODE'])

#3.8.1.18 HOUSETYPE_MODE
MTG[,'HOUSETYPE_MODE'] <- as.character(MTG[,'HOUSETYPE_MODE'])
MTG[MTG[,'HOUSETYPE_MODE'] != 'block of flats','HOUSETYPE_MODE'] <- 'house' #Block of flats is biggest one there?!?
MTG[,'HOUSETYPE_MODE'] <- as.factor(MTG[,'HOUSETYPE_MODE'])

#3.8.1.19
MTG[,'WALLSMATERIAL_MODE'] <- as.character(MTG[,'WALLSMATERIAL_MODE'])
MTG[MTG[,'WALLSMATERIAL_MODE'] == '' ,'WALLSMATERIAL_MODE'] <- 'Not Specified'
MTG[MTG[,'WALLSMATERIAL_MODE'] == 'Monolithic' ,'WALLSMATERIAL_MODE'] <- 'Others'
MTG[MTG[,'WALLSMATERIAL_MODE'] == 'Mixed' ,'WALLSMATERIAL_MODE'] <- 'Others'
MTG[,'WALLSMATERIAL_MODE'] <- as.factor(MTG[,'WALLSMATERIAL_MODE'])

#3.8.1.20
MTG[,'EMERGENCYSTATE_MODE'] <- as.character(MTG[,'EMERGENCYSTATE_MODE'])
MTG[MTG[,'EMERGENCYSTATE_MODE'] == '' ,'EMERGENCYSTATE_MODE'] <- 'Not Specified'
MTG[,'EMERGENCYSTATE_MODE'] <- as.factor(MTG[,'EMERGENCYSTATE_MODE'])

#3.8.1.21
MTG[,'OBS_30_CNT_SOCIAL_CIRCLE'] <- as.character(MTG[,'OBS_30_CNT_SOCIAL_CIRCLE'])
MTG[as.numeric(MTG[,'OBS_30_CNT_SOCIAL_CIRCLE']) >= 12 ,'OBS_30_CNT_SOCIAL_CIRCLE'] <- '12 or more'
MTG[,'OBS_30_CNT_SOCIAL_CIRCLE'] <- as.factor(MTG[,'OBS_30_CNT_SOCIAL_CIRCLE'])

#3.8.1.22
MTG[,'DEF_30_CNT_SOCIAL_CIRCLE'] <- as.character(MTG[,'DEF_30_CNT_SOCIAL_CIRCLE'])
MTG[as.numeric(MTG[,'DEF_30_CNT_SOCIAL_CIRCLE']) >= 3 ,'DEF_30_CNT_SOCIAL_CIRCLE'] <- '3 or more'
MTG[,'DEF_30_CNT_SOCIAL_CIRCLE'] <- as.factor(MTG[,'DEF_30_CNT_SOCIAL_CIRCLE'])

#3.8.1.23
MTG[,'OBS_60_CNT_SOCIAL_CIRCLE'] <- as.character(MTG[,'OBS_60_CNT_SOCIAL_CIRCLE'])
MTG[as.numeric(MTG[,'OBS_60_CNT_SOCIAL_CIRCLE']) >= 12 ,'OBS_60_CNT_SOCIAL_CIRCLE'] <- '12 or more'
MTG[,'OBS_60_CNT_SOCIAL_CIRCLE'] <- as.factor(MTG[,'OBS_60_CNT_SOCIAL_CIRCLE'])

#3.8.1.24
MTG[,'DEF_60_CNT_SOCIAL_CIRCLE'] <- as.character(MTG[,'DEF_60_CNT_SOCIAL_CIRCLE'])
MTG[as.numeric(MTG[,'DEF_60_CNT_SOCIAL_CIRCLE']) >= 3 ,'DEF_60_CNT_SOCIAL_CIRCLE'] <- '3 or more'
MTG[,'DEF_60_CNT_SOCIAL_CIRCLE'] <- as.factor(MTG[,'DEF_60_CNT_SOCIAL_CIRCLE'])

#3.8.1.25 Occupation_type, forgot this one first time round
MTG[,'OCCUPATION_TYPE'] <- as.character(MTG[,'OCCUPATION_TYPE'])
MTG[MTG[,'OCCUPATION_TYPE']=='','OCCUPATION_TYPE'] <- 'not specified'
MTG[,'OCCUPATION_TYPE'] <- as.factor(MTG[,'OCCUPATION_TYPE'])

#LEts try make smoe more features!
MTG[,'creditannuityratio'] <- MTG[,'AMT_CREDIT']/MTG[,'AMT_ANNUITY']
MTG[,'creditdownpayment'] <- MTG[,'AMT_GOODS_PRICE'] - MTG[,'AMT_CREDIT']


#3.8.1.25 - the documents - just gonna throw away all unneccesary variables
drops <- c('FLAG_DOCUMENT_2'
           ,'FLAG_DOCUMENT_4'
           ,'FLAG_DOCUMENT_7'
           ,'FLAG_DOCUMENT_9'
           ,'FLAG_DOCUMENT_10'
           ,'FLAG_DOCUMENT_11'
           ,'FLAG_DOCUMENT_12'
           ,'FLAG_DOCUMENT_13'
           ,'FLAG_DOCUMENT_14'
           ,'FLAG_DOCUMENT_15'
           ,'FLAG_DOCUMENT_17'
           ,'FLAG_DOCUMENT_19'
           ,'FLAG_DOCUMENT_20'
           ,'FLAG_DOCUMENT_21'
           ,'AMT_GOODS_PRICE' #Redundant
           ,'AMT_CREDIT' #Redundant
           ,'AMT_ANNUITY' #Redundant
           )

MTG <- MTG[,!(names(MTG) %in% drops)]
