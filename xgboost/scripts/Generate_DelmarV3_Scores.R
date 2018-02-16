
genDelMarV3Score <- function(df) {

  
  
  predictors = c(
    'Opportunity ID' = 'ID',
    'Application Date' = 'Credit_Sub_Date',
    'Approval Date' = 'Approval_Date',
    'Funded Date' = 'Funded_Date',
    'TIB_months' = 'TIB_months',
    'TIB' = 'TIB',
    'Deposit Number Last Month Primary' = 'Deposit_cnt_Last_Month',
    'CR Annual Revenue' = 'AGS',
    #'AVG NSFs 3 Month Primary' = 'avg_NSFs',
    'AVG NSFs ALL Month Primary' = 'avg_NSFs',
    #'MAX Negative Days 3 Month Primary' = 'max_NDs',
    'MAX Negative Days ALL Month Primary' = 'max_NDs',
    #'AVG Negative Days 3 Month Primary' = 'avg_NDs',
    'AVG Negative Days ALL Month Primary' = 'avg_NDs',
    #'STDEV Negative Days 3 Month Primary' = 'stdev_NDs',
    'STDEV Negative Days ALL Month Primary' = 'stdev_NDs',
    #'AVG Average Daily Balance 3 Month Primary' = 'avg_Avg_Daily_Balance',
    'AVG Average Daily Balance ALL Month Primary' = 'avg_Avg_Daily_Balance',
    #'AVG Counter Deposits 3 Month Primary' = 'avg_Deposits_Amount',
    'AVG Counter Deposits ALL Month Primary' = 'avg_Deposits_Amount',
    #'MAX Average Daily Balance 3 Month Primary' = 'max_Avg_Daily_Balance',
    'MAX Average Daily Balance ALL Month Primary' = 'max_Avg_Daily_Balance',
    #'MIN Average Daily Balance 3 Month Primary' = 'min_Avg_Daily_Balance',
    'MIN Average Daily Balance ALL Month Primary' = 'min_Avg_Daily_Balance',
    #'STDEV Average Daily Balance 3 Month Primary' = 'stdev_Avg_Daily_Balance',
    'STDEV Average Daily Balance ALL Month Primary' = 'stdev_Avg_Daily_Balance',
    #'AVG Deposit Number 3 Month Primary' = 'avg_Deposit_cnt',
    'AVG Deposit Number ALL Month Primary' = 'avg_Deposit_cnt',
    #'MAX Deposit Number 3 Month Primary' = 'max_Deposit_cnt',
    'MAX Deposit Number ALL Month Primary' = 'max_Deposit_cnt',
    #'MIN Deposit Number 3 Month Primary' = 'min_Deposit_cnt',
    'MIN Deposit Number ALL Month Primary' = 'min_Deposit_cnt',
    'Negative Days Last Month Primary' = 'NDs_Last_Month',
    'CR Max FICO' = 'Max_FICO',
    'PG1_OLD' = 'OLD',
    'PG1_TRADES' = 'TRADES',
    'PG1_INQ12' = 'INQ12',
    'PG1_OCCUR6_30' = 'OCCUR6_30',
    'PG1_RCURB' = 'RCURB',
    'PG1_RUTIL' = 'RUTIL',
    #'PG1_HIT' = 'HIT',
    'PG1_SAT' = 'SAT',
    'PG1_MTRADE' = 'MTRADE',
    'PG1_COLLS' = 'COLLS',
    'PG1_INQ6' = 'INQ6',
    'PG1_PAST' = 'PAST',
    'PG1_OPEN12' = 'OPEN12',
    'frac_Deposits_Under_10k_derived' = 'frac_Deposits_Under_10k',
    'DepositVol_derived' = 'DepositVol',
    'DepositCntVol_derived' = 'DepositCntVol',
    'BalanceVol_derived' = 'BalanceVol',
    'BalanceIncrease_derived' = 'BalanceIncrease',
    'PlatinumPayments_derived' = 'PlatinumPayments',
    'PG1_OKTRADE' = 'OKTRADE',
    'Total_Liens_Judgments__c' = 'Open_Public_Records',
    'Average Daily Balance Last Month' = 'Avg_Daily_Balance_Last_Month',
    #'STDEV Counter Deposits 3 Month Primary' = 'stdev_Deposits',
    'STDEV Counter Deposits ALL Month Primary' = 'stdev_Deposits',
    
    #'AVG Counter Deposits 3 Month Primary' = 'avg_Deposits',
    
    #'STDEV Deposit Number 3 Month Primary' = 'stdev_Deposit_cnt',
    'STDEV Deposit Number ALL Month Primary' = 'stdev_Deposit_cnt',
    'logSales262_derived' = 'logSales',
    'OKRATIO_derived' = 'OKRATIO',
    'PercentNewTrades_derived' = 'PercentNewTrades',
    'PCT_SAT_derived' = 'PCT_SAT'
  )
  
  
  df = plyr::rename(df, predictors, warn_missing = FALSE, warn_duplicated = FALSE)
  
  # Convert Variables to Numeric 
  
  
  to_numeric = c( 
    'TIB_months',                        
    'TIB',                               
    'Deposit_cnt_Last_Month',            
    'AGS',                               
    'avg_NSFs',                          
    'max_NDs',                           
    'avg_NDs',                           
    'stdev_NDs',                         
    'avg_Avg_Daily_Balance',             
    'avg_Deposits_Amount',               
    'max_Avg_Daily_Balance',             
    'min_Avg_Daily_Balance',             
    'stdev_Avg_Daily_Balance',
    'avg_Deposit_cnt',
    'max_Deposit_cnt',
    'min_Deposit_cnt',
    'NDs_Last_Month',
    'Max_FICO',
    'OLD',
    'TRADES',
    'INQ12',
    'OCCUR6_30',
    'RCURB',
    'RUTIL',
    'SAT',
    'MTRADE',
    'COLLS',
    'INQ6',
    'PAST',
    'OPEN12',
    'frac_Deposits_Under_10k',
    'DepositVol',
    'DepositCntVol',
    'BalanceVol',
    'BalanceIncrease',
    'PlatinumPayments',
    'OKTRADE',
    'Open_Public_Records',
    'Avg_Daily_Balance_Last_Month',
    'stdev_Deposits',
    #'avg_Deposits',
    'stdev_Deposit_cnt',
    'logSales',
    'OKRATIO',
    'PercentNewTrades',
    'PCT_SAT'
  )
  
  for (v in to_numeric) {
    df[[v]] = suppressWarnings(as.numeric(df[[v]]))
  }
  
  
  
  # Variables to Keep
  to_keep = c(
    'ID',
    'Credit_Sub_Date',
    'Approval_Date',
    'Funded_Date',
    'TIB_months',                        
    'TIB',                               
    'Deposit_cnt_Last_Month',            
    'AGS',                               
    'avg_NSFs',                          
    'max_NDs',                           
    'avg_NDs',                           
    'stdev_NDs',                         
    'avg_Avg_Daily_Balance',             
    'avg_Deposits_Amount',               
    'max_Avg_Daily_Balance',             
    'min_Avg_Daily_Balance',             
    'stdev_Avg_Daily_Balance',           
    'avg_Deposit_cnt',                   
    'max_Deposit_cnt',                   
    'min_Deposit_cnt',                   
    'NDs_Last_Month',                    
    'Max_FICO',                          
    'OLD',                               
    'TRADES',                            
    'INQ12',                             
    'OCCUR6_30',                         
    'RCURB',                             
    'RUTIL',                             
    'SAT',                               
    'MTRADE',                            
    'COLLS',                             
    'INQ6',                              
    'PAST',                              
    'OPEN12',                            
    'frac_Deposits_Under_10k',           
    'DepositVol',                        
    'DepositCntVol',                     
    'BalanceVol',                        
    'BalanceIncrease',                   
    'PlatinumPayments',                  
    'OKTRADE',                           
    'Open_Public_Records',               
    'Avg_Daily_Balance_Last_Month',      
    'stdev_Deposits',                    
    #'avg_Deposits',                      
    'stdev_Deposit_cnt',                 
    'logSales',                          
    'OKRATIO',                           
    'PercentNewTrades',
    'PCT_SAT'
  )
  
  
  
  df = df[, to_keep]
  
  df$avg_Deposits = df$avg_Deposits_Amount
  
  

df[is.na(df['Max_FICO']), 'Max_FICO'] = 650
df[is.na(df['TIB']), 'TIB'] = 6


DelmarV2_Approval_Model <- readRDS("data/ApprovalModel_v2.rds")


# Calculate the approval score 
appr_model = DelmarV2_Approval_Model
appr_score = appr_model$predict(appr_model$trained, df, type='response')
write(sprintf("Del Mar V2 Approval Scores Generated"), stderr())

df$delMarV2APPRScore = appr_score


# Calculate the CO score 
co_model1 = readRDS("data/COModel_v3_Bagging_20180116.rds")
co_model2 = readRDS("data/COModel_v3_Bagging_CASHFLOW_20180116.rds")
co_model3 = readRDS("data/COModel_v3_Bagging_FICOTIB_20180116.rds")

df$delMarV3COProb1 = predict(co_model1, df, type='response')
df$delMarV3COProb2 = predict(co_model2, df, type='response')
df$delMarV3COProb3 = predict(co_model3, df, type='response')

df$delMarV3COProb = coalesce(df$delMarV3COProb1, df$delMarV3COProb2, df$delMarV3COProb3)

co_model_ranker = readRDS("data/ranker_COModel_v3_Bagging_20180118.rds")

df$delMarV3COScore = co_model_ranker(df$delMarV3COProb)



# Identify Del Mar V3 risk class
df$delMarV3RiskClass = as.character(ifelse(is.na(df$delMarV2APPRScore) | is.na(df$delMarV3COScore), 'NA',
                                           ifelse(df$delMarV2APPRScore >= .25 & df$delMarV3COScore <= .2, '12',
                                                  ifelse(df$delMarV3COScore <= .5, '10',
                                                         ifelse(df$delMarV2APPRScore <= .5 & df$delMarV2APPRScore >= .25 & df$delMarV3COScore >= .8, '8',
                                                                ifelse(df$delMarV2APPRScore <= .25 & df$delMarV3COScore >= .8, 'Model Review',
                                                                       ifelse(df$delMarV3COScore >= .5,'9','No Score?')))))))



return(df)

}