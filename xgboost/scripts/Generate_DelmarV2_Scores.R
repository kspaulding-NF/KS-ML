

genDelMarV2Score <- function(df) {
  

  
# Rename Variables  
  
  
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
    'PercentNewTrades_derived' = 'PercentNewTrades'
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
    'PercentNewTrades'
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
                'PercentNewTrades'                  
  )
  
  
   
  df = df[, to_keep]
  
  df$avg_Deposits = df$avg_Deposits_Amount
   
  
  #Load previously saved model objects
  DelmarV2_CO_Model <- readRDS("data/COModel_v2.rds")
  DelmarV2_Approval_Model <- readRDS("data/ApprovalModel_v2.rds")
  
  
  # Calculate the charge off score
  co_model = DelmarV2_CO_Model
  # If FICO or TIB are missing, set to some default values for the CO model
  df[is.na(df['Max_FICO']), 'Max_FICO'] = 650
  df[is.na(df['TIB']), 'TIB'] = 6
  coProbs = co_model$predict(co_model$trained, df, type='response')
  coScores = co_model$trained$ranker(coProbs)
  write(sprintf("Del Mar V2 Charge Off Scores Generated"), stderr())
  
  
  # Calculate the approval score 
  appr_model = DelmarV2_Approval_Model
  appr_score = appr_model$predict(appr_model$trained, df, type='response')
  write(sprintf("Del Mar V2 Approval Scores Generated"), stderr())
  

  # Calculate Del Mar V2 score
  alpha = 0.5
  delMarV2Score = data.frame(delMarV2Score = ((1 - coScores) * alpha + appr_score * (1-alpha)))
  delMarV2Score_RAW = data.frame(delMarV2ScoreRAW = ((1 - coProbs) * alpha + appr_score * (1-alpha)))
  
  
  # Segment / Identify Del Mar V2 risk class
  delMarV2RiskClass = as.character(ifelse(is.na(delMarV2Score), NA, 
                                          ifelse(delMarV2Score <= 0.085, 'Model Review',
                                                 ifelse(delMarV2Score <= 0.265, '8',
                                                        ifelse(delMarV2Score <= 0.445, '9',
                                                               ifelse(delMarV2Score <= 0.710, '10', '12'))))))
  
  
  df$delMarV2APPRScore = appr_score
  df$delMarV2COProb = coProbs
  df$delMarV2COScore = coScores
  df$delMarV2Score = delMarV2Score$delMarV2Score
  df$delMarV2ScoreRAW = delMarV2Score_RAW$delMarV2ScoreRAW
  df$delMarV2RiskClass = delMarV2RiskClass
  
  return(df)
  
}