IRE_list <-lapply(names(IRE[,sapply(IRE, class) %in% c('numeric','integer') & !(names(IRE) %in% excl)]),
                 function(col) ggplot(data=IRE) 
                 + geom_histogram(aes_string(x=col, color='TARGET_CHAR')
                                  , fill='white'
                                  , bins=30
                                  , position = position_stack(reverse = TRUE))
                 + theme(legend.position = "none")
)
cowplot::plot_grid(plotlist = IRE_list)


#No Time dimension in MTG dataset or no useful time dimension in RL dataset
IRE_TS_Date <- IRE[,c('issue_d','TARGET')]
IRE_TS_Date['issue_month'] <- LastDayInMonth(IRE_TS_Date$issue_d)
IRE_TS_Date <- sqldf('
                    select 
                      issue_month
                    , sum(TARGET) as Num_dfs
                    , count(*) as num_Lns
                    from IRE_TS_Date
                    group by 1
                    ')

IRE_TS_Date['ODR'] <- IRE_TS_Date['Num_dfs']/IRE_TS_Date['num_Lns']

ggplot(data = IRE_TS_Date, aes(x = issue_month , y = ODR))+
  geom_line(color = "#00AFBB", size = 2)

#Number of defaults not static throughout time - may stratify sample through out time.

#Annual Income - Take log for skewness
IRE[,'annual_inc_LOG'] <- log(IRE[,'annual_inc'])

#dti - Take log for skewness
IRE[,'dti_LOG'] <- log(1 + IRE[,'dti'])
IRE <- IRE[IRE['dti'] != max(IRE['dti']),] # Remove outlier


#total_rec_prncp - Take log for skewness
IRE[,'total_rec_prncp'] <- log(1 + IRE[,'total_rec_prncp'])

#Recoveries - Take log for skewness 
IRE[,'home_ownership'] <- as.character(IRE[,'home_ownership'])
clist <- c('ANY','NONE')
IRE[IRE[,'home_ownership'] %in% clist , 'home_ownership'] <- 'OTHER'
IRE[,'home_ownership'] <- as.factor(IRE[,'home_ownership'])

#Purpose - lets group the small varaibles together by similarity
IRE[,'purpose'] <- as.character(IRE[,'purpose'])
IRE[IRE[,'purpose'] == 'car', 'purpose'] <- 'major_purchase'
IRE[IRE[,'purpose'] == 'home_improvement', 'purpose'] <- 'house'
IRE[IRE[,'purpose'] == 'renewable_energy', 'purpose'] <- 'other'
IRE[IRE[,'purpose'] == 'moving', 'purpose'] <- 'house'
IRE[IRE[,'purpose'] == 'medical', 'purpose'] <- 'other'
IRE[IRE[,'purpose'] == 'educational', 'purpose'] <- 'other'
IRE[IRE[,'purpose'] == 'wedding', 'purpose'] <- 'vacation'
IRE[,'purpose'] <- as.factor(IRE[,'purpose'])



drops <- c('year' #Redundant because Date is there
           ,'annual_inc'
           ,'recoveries' #Dependent on default so we remove
           ,'application_type'
           ,'dti'
          )
IRE <- IRE[,!(names(IRE) %in% drops)]
