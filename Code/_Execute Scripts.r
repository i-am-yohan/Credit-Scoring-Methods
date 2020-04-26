
#Set data directory, the only user input
setwd('C:/Users/i_am_yohan/Desktop/Postgraduate Diploma in Data Analytics/Semester II/Data Mining and Machine Learning/Project/Submission')



#Kaggle COmpetition Data
MTG <- read.csv('Data/application_train.csv' , stringsAsFactors = TRUE , row.names = 1)

#Kaggle Vehicle Loan Data
RL <- read.csv('Data/train.csv', stringsAsFactors = TRUE , row.names = 1)

#ROI Dummy Data
IRE <- read.csv('Data/loan_final313.csv', stringsAsFactors = TRUE , row.names = 1)


source("Code/1. Data Import and Clean.R" , local=TRUE)
source("Code/2.1 Exploratory Analysis - MTG.R" , local=TRUE)
source("Code/2.2 Exploratory Analysis - RL.R" , local=TRUE)
source("Code/2.3 Exploratory Analysis - IRE.R" , local=TRUE)
source("Code/3. Stratified Sampling.R" , local=TRUE)
source("Code/4. Typical Linear Scorecard Example.R", local=TRUE)
yes

source("Code/6. Further Data Processing.R", local=TRUE)
source("Code/7.1 Decision Tree and Random Forest.R", local=TRUE)
source("Code/7.2 Decision Tree and Random Forest.R", local=TRUE)
source("Code/8. Support Vector Machine.R", local=TRUE)
source("Code/9. Artificial Nueral Networks.R", local=TRUE)

#The test results
#Remember, predicted results are on the x-axis! It's done this way to match H2O package

#Home Credit
Test_SC(MTG_SC_Model , MTG_SC_Test, 0.5)
Test_DT(MTG_Tree , MTG_Test)
Test_h2o(MTG_RF , MTG_Test_h2o)
Test_SVM(MTG_svm , MTG_Test_SVM_Woe)
Test_h2o(MTG_nn, MTG_Test_h2o)


#Vehicle Loans
Test_SC(RL_SC_Model , RL_SC_Test, 0.5)
Test_DT(RL_Tree , RL_Test)
Test_h2o(RL_RF , RL_Test_h2o)
Test_SVM(RL_svm , RL_Test_SVM_Woe)
Test_h2o(RL_nn, RL_Test_h2o)


#IRE Loans
Test_SC(IRE_SC_Model , IRE_SC_Test, 0.5)
Test_DT(IRE_Tree , IRE_Test)
Test_h2o(IRE_RF , IRE_Test_h2o)
Test_SVM(IRE_svm , IRE_Test_SVM_Woe)
Test_h2o(IRE_nn, IRE_Test_h2o)


h2o.shutdown(prompt=FALSE)
