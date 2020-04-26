str(RL)

cor(RL[,sapply(RL, class) == 'numeric'] , use = 'complete.obs') #Two highly correlated variables here

#A Full visualization:
excl <- 'TARGET'
RL_list <-lapply(names(RL[,sapply(RL, class) %in% c('numeric','integer') & !(names(RL) %in% excl)]),
                  function(col) ggplot(data=RL) 
                  + geom_histogram(aes_string(x=col, color='TARGET_CHAR')
                                   , fill='white'
                                   , bins=30
                                   , position = position_stack(reverse = TRUE))
                  + theme(legend.position = "none")
)
cowplot::plot_grid(plotlist = RL_list)



#3.7.2 The Vehicle loans dataset
#3.7.2.1 The loan amount
RL['DISBURSED_AMOUNT_log'] <- log(1 + RL[,'DISBURSED_AMOUNT'])
RL <- RL[RL['DISBURSED_AMOUNT_log'] != max(RL['DISBURSED_AMOUNT_log']), ] #Remove Top


#3.7.2.2 The asset cost
RL['ASSET_COST_log'] <- log(1 + RL[,'ASSET_COST'])
RL <- RL[RL['ASSET_COST'] != max(RL['ASSET_COST']), ]#Remove two outliers
RL <- RL[RL['ASSET_COST'] != max(RL['ASSET_COST']), ]


#3.7.2.3 see if we have parrallel lines
reduc3 <- glm(TARGET ~ DISBURSED_AMOUNT + ASSET_COST + LTV, data = RL, family = "binomial")
summary(reduc3) #no Parrallel lines observed

#3.7.2.4 Score

#3.7.2.5 Number of accounts - some very large numbers here
RL['PRI_ACTIVE_ACCTS_log'] <- log(1 + RL[,'PRI_ACTIVE_ACCTS'])

sqldf('select 
  PRI_ACTIVE_ACCTS
, count(*) as num
, sum(TARGET) as DFs
from RL
group by 1
order by 1
')

cor(RL[,c('PRI_ACTIVE_ACCTS','PRI_NO_OF_ACCTS')]) #Highly Correlated - Keep only active accounts

RL[RL[,'PRI_ACTIVE_ACCTS'] >= 10,'PRI_ACTIVE_ACCTS'] <- 10

#3.7.2.6 Overdue Accounts
sqldf('select 
  PRI_OVERDUE_ACCTS
, count(*) as num
, sum(TARGET) as DFs
from RL
group by 1
order by 1
')

RL[RL[,'PRI_ACTIVE_ACCTS'] >= 5,'PRI_ACTIVE_ACCTS'] <- 5
RL[,'PRI_ACTIVE_ACCTS'] <- as.factor(RL[,'PRI_ACTIVE_ACCTS'])



#3.7.2.5 Current Balance
aggregate(RL['TARGET'] , RL['SEC_NO_OF_ACCTS'] , 'length')
aggregate(RL['TARGET'] , RL['SEC_NO_OF_ACCTS'] , 'sum') #Might remove secondary info, not very relevant


RL[,'PRI_CURRENT_BAL_Status'] <- NA
RL[RL[,'PRI_CURRENT_BALANCE'] < 0 ,'PRI_CURRENT_BAL_Status'] <- 'Negative Balance'
RL[RL[,'PRI_CURRENT_BALANCE'] == 0 ,'PRI_CURRENT_BAL_Status'] <- 'Zero Balance'
RL[RL[,'PRI_CURRENT_BALANCE'] > 0 ,'PRI_CURRENT_BAL_Status'] <- 'Positive Balance'
RL[,'PRI_CURRENT_BAL_Status'] <- as.factor(RL[,'PRI_CURRENT_BAL_Status'])

max1 <- function(x){return(max(x,1))}
RL[,'PRI_CURRENT_BALANCE_log'] <- log(apply(RL['PRI_CURRENT_BALANCE'],1,max1))


#3.7.2.6 Sanctioned Amount
RL['PRI_SANCTIONED_AMT_0_ind'] <- NA
RL[RL['PRI_SANCTIONED_AMOUNT'] == 0,'PRI_SANCTIONED_AMT_0_ind'] <- 1
RL[RL['PRI_SANCTIONED_AMOUNT'] != 0,'PRI_SANCTIONED_AMT_0_ind'] <- 0
RL['PRI_SANCTIONED_AMT_0_ind'] <- as.factor(RL[,'PRI_SANCTIONED_AMT_0_ind'])
RL['PRI_SANCTIONED_AMOUNT_log'] <- log(1 + RL[,'PRI_SANCTIONED_AMOUNT'])


#3.7.2.6 Disbursed Amount
#Analyse relationship between that and sanction amt
sqldf('select
count(*) as num_accs
,sum(TARGET) as num_defs
from RL
where PRI_SANCTIONED_AMOUNT != PRI_DISBURSED_AMOUNT
') #equal in a lot of cases

sqldf('select
count(*) as num_accs
,sum(TARGET) as num_defs
from RL
where PRI_SANCTIONED_AMOUNT > PRI_DISBURSED_AMOUNT
')

RL['SANCT_EQ_DISB_ind'] <- NA
RL[RL['PRI_SANCTIONED_AMOUNT'] == RL['PRI_DISBURSED_AMOUNT'],'SANCT_EQ_DISB_ind'] <- 1
RL[RL['PRI_SANCTIONED_AMOUNT'] != RL['PRI_DISBURSED_AMOUNT'],'SANCT_EQ_DISB_ind'] <- 0
RL['PRI_DISBURSED_AMOUNT_log'] <- log(1 + RL[,'PRI_DISBURSED_AMOUNT']) #apply log too
RL[,'SANCT_EQ_DISB_ind'] <- as.factor(RL[,'SANCT_EQ_DISB_ind'])


#analyse relationship between PRI_DISBURSED_AMOUNT and DISBURSED_AMOUNT
sqldf('select
count(*) as num_accs
,sum(TARGET) as num_defs
from RL
where PRI_DISBURSED_AMOUNT != DISBURSED_AMOUNT
') #not equal in a lot of cases

RL['LOAN_VALUE_OF_TOTAL'] <- log(1+RL['PRI_DISBURSED_AMOUNT']/RL['DISBURSED_AMOUNT'])



#3.7.2.7 Installment Amount
RL['PRIMARY_INSTAL_AMT_log'] <- log(1 + RL['PRIMARY_INSTAL_AMT'])

#3.7.2.8 NEW_ACCTS_IN_LAST_SIX_MONTHS
RL[RL[,'NEW_ACCTS_IN_LAST_SIX_MONTHS'] >= 5,'NEW_ACCTS_IN_LAST_SIX_MONTHS'] <- 5

#3.7.2.9 Delinquent accounts in last few months
RL[RL[,'DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS'] > 0,'DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS'] <- 1 #Change to binary variable

#3.7.2.10 Account_AGE
RL['AVERAGE_ACCT_AGE_log'] <- log(1 + RL[,'AVERAGE_ACCT_AGE'])

#3.7.2.11 Credit History Length
RL['CREDIT_HISTORY_LENGTH_log'] <- log(1 + RL[,'CREDIT_HISTORY_LENGTH'])

#3.7.2.12 Number of Enquries
RL[RL[,'NO_OF_INQUIRIES'] >= 3,'NO_OF_INQUIRIES'] <- 3
RL[,'NO_OF_INQUIRIES'] <- as.factor(RL[,'NO_OF_INQUIRIES'])

#3.7.2.13 
RL[,'MANUFACTURER_ID'] <- as.character(RL[,'MANUFACTURER_ID'])
clist <- c('145','152','153','156')
RL[RL[,'MANUFACTURER_ID'] %in% clist , 'MANUFACTURER_ID'] <- 'other'
RL[,'MANUFACTURER_ID'] <- as.factor(RL[,'MANUFACTURER_ID'])

#3.7.2.14
RL[,'EMPLOYMENT_TYPE'] <- as.character(RL[,'EMPLOYMENT_TYPE'])
RL[RL[,'EMPLOYMENT_TYPE'] == '', 'EMPLOYMENT_TYPE'] <- 'Not specified'
RL[,'EMPLOYMENT_TYPE'] <- as.factor(RL[,'EMPLOYMENT_TYPE'])

#Date of birth - Convert to Age
RL['TODAY'] <- as.Date('31-03-2020' , format="%d-%m-%Y")
RL['AGE'] <- (as.numeric(RL[,'TODAY']) - as.numeric(RL[,'DATE_OF_BIRTH']))/365.25


#LEts create some ratio variables
RL['AST_2_LN_RATIO'] <- RL['DISBURSED_AMOUNT']/RL['ASSET_COST']
RL['COST_2_INSTLLMNT'] <- RL['PRIMARY_INSTAL_AMT']/RL['ASSET_COST']

RL[RL['PRIMARY_INSTAL_AMT'] == 0 ,'IO_Ind'] <- 1
RL[is.na(RL['IO_Ind']) ,'IO_Ind'] <- 0  #an Interest only indicator

#Ratio of delinquent accts to number of accounts
RL['Ratio_of_delinquent_accounts'] <- RL['PRI_NO_OF_ACCTS']/RL['DELINQUENT_ACCTS_IN_LAST_SIX_MONTHS']
RL[is.infinite(RL[,'Ratio_of_delinquent_accounts']),'Ratio_of_delinquent_accounts'] <- 0
RL[is.nan(RL[,'Ratio_of_delinquent_accounts']),'Ratio_of_delinquent_accounts'] <- 0

# Drop the columns
drops <- c('DISBURSED_AMOUNT','ASSET_COST'
           , 'PRI_CURRENT_BALANCE'
           , 'PRI_NO_OF_ACCTS'
           , 'PRI_ACTIVE_ACCTS'
           , 'PRIMARY_INSTAL_AMT'
           , 'AVERAGE_ACCT_AGE'
           , 'CREDIT_HISTORY_LENGTH'
           , 'SEC_NO_OF_ACCTS'
           , 'SEC_ACTIVE_ACCTS'
           , 'SEC_OVERDUE_ACCTS'
           , 'SEC_CURRENT_BALANCE'
           , 'SEC_SANCTIONED_AMOUNT'
           , 'SEC_DISBURSED_AMOUNT'
           , 'SEC_INSTAL_AMT'
           , 'PRI_SANCTIONED_AMOUNT'
           , 'PRI_DISBURSED_AMOUNT'
           , 'CURRENT_PINCODE_ID' #silly variable
           , 'BRANCH_ID'
           , 'SUPPLIER_ID' #an ID varaible, not much use
           , 'MOBILENO_AVL_FLAG'
           , 'DISBURSAL_DATE' #Not very useful - Takes place over three months
           , 'DATE_OF_BIRTH'
           , 'TODAY'
           , 'PASSPORT_FLAG')

RL <- RL[,!(names(RL) %in% drops)]
