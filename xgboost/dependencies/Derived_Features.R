
First.Fix = function(df) {
  
  
  #*************************************************************************#
  #***************************** Adjust FICO *******************************#
  #*************************************************************************#
  # Change Max and Min FICOs that are -1 or 0 to NA
  df$`CR Max FICO`[ is.na(df$`CR Max FICO`) ] <- 0
  df$`CR Min FICO`[ is.na(df$`CR Min FICO`) ] <- 0
  
  df$`CR Max FICO`[ df$`CR Max FICO` == 0 | df$`CR Max FICO` == -1] = NA
  df$`CR Min FICO`[ df$`CR Min FICO` == 0 | df$`CR Min FICO` == -1] = NA
  
  
  #******************************************************************************************#
  #***************************** Convert Necessary Variables ********************************#
  #******************************************************************************************#
  
  to_numeric = c(
    'Deposit Number Last Month Primary',
    'CR Annual Revenue',
    'AVG NSFs 3 Month Primary',
    'AVG NSFs ALL Month Primary',
    'MAX Negative Days 3 Month Primary',
    'MAX Negative Days ALL Month Primary',
    'AVG Negative Days 3 Month Primary',
    'AVG Negative Days ALL Month Primary',
    'STDEV Negative Days 3 Month Primary',
    'STDEV Negative Days ALL Month Primary',
    'AVG Average Daily Balance 3 Month Primary',
    'AVG Average Daily Balance ALL Month Primary',
    'AVG Counter Deposits 3 Month Primary',
    'AVG Counter Deposits ALL Month Primary',
    'MAX Average Daily Balance 3 Month Primary',
    'MAX Average Daily Balance ALL Month Primary',
    'MIN Average Daily Balance 3 Month Primary',
    'MIN Average Daily Balance ALL Month Primary',
    'STDEV Average Daily Balance 3 Month Primary',
    'STDEV Average Daily Balance ALL Month Primary',
    'AVG Deposit Number 3 Month Primary',
    'AVG Deposit Number ALL Month Primary',
    'MAX Deposit Number 3 Month Primary',
    'MAX Deposit Number ALL Month Primary',
    'MIN Deposit Number 3 Month Primary',
    'MIN Deposit Number ALL Month Primary',
    'Negative Days Last Month Primary',
    'CR Max FICO',
    'PG1_OLD',
    'PG1_TRADES',
    'PG1_INQ12',
    'PG1_OCCUR6_30',
    'PG1_RCURB',
    'PG1_RUTIL',
    'PG1_SAT',
    'PG1_MTRADE',
    'PG1_COLLS',
    'PG1_INQ6',
    'PG1_PAST',
    'PG1_OPEN12',
    'PG1_OKTRADE',
    'Total_Liens_Judgments__c',
    'Average Daily Balance Last Month',
    'STDEV Counter Deposits 3 Month Primary',
    'STDEV Counter Deposits ALL Month Primary',
    'STDEV Deposit Number 3 Month Primary',
    'STDEV Deposit Number ALL Month Primary',
    'PG1_FICO',
    'PG2_FICO',
    'CR Max FICO',
    'CR Min FICO',
    'AVG Other Deposits 3 Month Primary',
    'AVG Other Deposits ALL Month Primary',
    'Average Daily Balance Last Month Primary',
    'MAX Counter Deposits 3 Month Primary',
    'MAX Counter Deposits ALL Month Primary',
    'MIN Counter Deposits 3 Month Primary',
    'MIN Counter Deposits ALL Month Primary',
    'Counter Deposits Last Month Primary',
    'STDEV Average Daily Balance 3 Month Primary',
    'STDEV Average Daily Balance ALL Month Primary',
    'PG1_SAT',
    'PG1_TRADES',
    'PG1_NEW',
    'PG1_OLD',
    'PG1_OPEN12',
    'CR TIB',
    'CR YIB',
    'SUM Deposits Under 10k 3 Month Primary',
    'SUM Deposits Under 10k ALL Month Primary',
    'SUM Deposit Number 3 Month Primary',
    'SUM Deposit Number ALL Month Primary'
  )
  
  for (v in to_numeric) {
    df[[v]] = suppressWarnings(as.numeric(df[[v]]))
  }
  
  
  to_date = c('CR Created Date',
              'Application Date',
              'CR Business Start Date',
              'Approval Date',
              'Funded Date',
              'BK_Discharge_Date__c',
              'PG_1_BK_Discharge_Date__c',
              'PG_2_BK_Discharge_Date__c',
              'PG_3_BK_Discharge_Date__c',
              'Renewal Funded Date',
              'Loan Termination Date',
              'Renewal Loan Termination Date'
              
  )
  for (v in to_date) {
    df[[v]] = suppressWarnings(as.Date(df[[v]]))
  }
  
  return(df)
}






computeBK = function(df) {
      # Compute the following variables:
      #   Is there any open BK (so a PG or the business has a BK but no discharge date)
      #   What is the last BK Discharge Date
      
      bk_date_vars = c(paste0('PG_', 1:3, '_BK_Discharge_Date__c'), 'BK_Discharge_Date__c')
      prev_bk_vars = c(paste0('PG_', 1:3, '_Previous_BK__c'), 'Previous_Bankruptcy__c')
      
      # Check all required variables are present
      checkVars(df, c('CR Created Date', bk_date_vars, prev_bk_vars))
      
      # Of the discharged BKs, which one is the most recent
      df$Last_BK_Discharge_Date = parseDate(suppressWarnings(apply(df[,bk_date_vars],1,function(x) max(x, na.rm=TRUE))))
      
      # Do we have any BKs at all
      df$AnyBK = suppressWarnings(apply(df[,prev_bk_vars] == 'Yes', 1, function(r) any(r, na.rm=TRUE)))
      
      # Does any PG or business have an open BK (so yes previous bk but no discharge date)
      has_open_bk = df[,prev_bk_vars] == 'Yes' & is.na(df[,bk_date_vars])
      df$AnyOpenBK = suppressWarnings(apply(has_open_bk, 1, function(r) any(r, na.rm = TRUE)))
      
      # Months since most recent discharged BK. Inf if there is no ever, 0 if there is an open bk
      df$Months_Since_BK = Inf
      df[df$AnyOpenBK,'Months_Since_BK'] = 0
      
      x = !is.na(df$Last_BK_Discharge_Date) & !df$AnyOpenBK  # Obsevations that have a discharged BK but no open BK
      
      # Time since last discharged BK, in days, then converted months
      daysdiff = difftime(df[x, 'CR Created Date'], df[x,'Last_BK_Discharge_Date'], 'days')
      units(daysdiff) = 'days'
      df[x, 'Months_Since_BK'] = pmax(as.numeric(daysdiff)/30.4, 0)
      
  return(df)
}

computeTIB = function(df) {
  
    # Check all required variables are present
    checkVars(df, c('CR Created Date', 
                    'CR Business Start Date', 
                    'CR YIB', 
                    'CR TIB'))
    
    # Verify that CreatedDate are Business_Start_Date__c are are POSIXct and cast if not
    if (class(df$`CR Created Date`)[1] != 'POSIXct') {
      df$`CR Created Date` = parseDate(df$`CR Created Date`)
    }
    if (class(df$`CR Business Start Date`)[1] != 'POSIXct') {
      df$`CR Business Start Date` = parseDate(df$`CR Business Start Date`)
    }
    
    # Initialize results
    df$TIB_months = NA
    df$TIB = NA
    
    # If business start date is available, use it. 
    daysdiff = difftime(df$`CR Created Date`, df$`CR Business Start Date`, "days")
    units(daysdiff) = 'days'
    df$TIB_months = as.numeric(daysdiff)/30.4
    
    # Otherwise use the max time in business from Credit Review and Opp
    use_tib = is.na(df$TIB_months)
    if(any(use_tib)) {
      max_tib_months = pmax(df$`CR YIB`, df$`CR TIB`, na.rm=TRUE)*12
      df$TIB_months[use_tib] = max_tib_months[use_tib]
    }
    df$TIB_months = as.numeric(df$TIB_months)
    df$TIB = df$TIB_months/12
  
  return(df)
}



# Script to generate derived variables
GenDerived.Features = function(df) {
  
  
    df$CO_derived = df$`National Funding Charge Off Flag` == 'TRUE'
    
    df$MinFICO_derived = coalesce(ifelse(!is.na(df$PG1_FICO) & !is.na(df$PG2_FICO) & df$PG1_FICO > df$PG2_FICO, df$PG2_FICO, df$PG1_FICO), df$`CR Max FICO`)
    
    
    # Saturation AGS -> the AGS that gives 300k at 9 months, 13% Impact, 25 points, 1.26 buy rate
    df$SaturationAGS_derived = 300000 * (12/9) *(1.26 + 0.25) / 0.13
    df$CalcAGS_derived <- ifelse(df$`AVG Counter Deposits ALL Month Primary` == 0 | is.na(df$`AVG Counter Deposits ALL Month Primary`), df$`AVG Counter Deposits ALL Month Primary` * 12, df$`AVG Other Deposits ALL Month Primary` * 12)
    df$CalcAGS_derived <- ifelse(((df$CalcAGS_derived == 0 | is.na(df$CalcAGS_derived)) & df$`CR Annual Revenue` > 0), df$`CR Annual Revenue`, df$CalcAGS_derived)  
    
    new_vars = data.frame(
      # Number of platinum payments supported by ADB
      #   Platinum Daily Payment = Daily Sales * 13% = (AGS / 262)*13%
      'PlatinumPayments_derived' = df$`AVG Average Daily Balance ALL Month Primary` / (df$`CR Annual Revenue` * 0.13 / 262),
      
      # Max percent drop in ADB
      'BalanceVol_derived' = 100*((df$`MAX Average Daily Balance ALL Month Primary` - df$`MIN Average Daily Balance ALL Month Primary`)/df$`MAX Average Daily Balance ALL Month Primary`),
      
      # Percent increase (decrease) in balance in the last month relative to the average.
      'BalanceIncrease_derived' = 100*(df$`Average Daily Balance Last Month Primary` - df$`AVG Average Daily Balance ALL Month Primary`)/df$`AVG Average Daily Balance ALL Month Primary`,
      
      # Max percent drop in monthly deposits
      'DepositVol_derived' = 100*((df$`MAX Counter Deposits ALL Month Primary` - df$`MIN Counter Deposits ALL Month Primary`)/df$`MAX Counter Deposits ALL Month Primary`),
      
      # Max percent drop in monthly deposits count
      'DepositCntVol_derived' = 100*((df$`MAX Deposit Number ALL Month Primary` - df$`MIN Deposit Number ALL Month Primary`)/df$`MAX Deposit Number ALL Month Primary`),
      
      # Percent increase (decrease) in deposits in the last month relative to the average.
      'DepositIncrease_derived' = 100*(df$`Counter Deposits Last Month Primary` - df$`AVG Counter Deposits ALL Month Primary`)/df$`AVG Counter Deposits ALL Month Primary`,
      
      # Log of daily sales
      'logSales262_derived' = log(df$`CR Annual Revenue`/262),
      
      # AGS relative to Saturation AGS
      'FundingSaturation_derived' = 100 * df$`CR Annual Revenue` / df$SaturationAGS_derived,
      
      # Measure of how much buffer they maintain in their bank accounts
      'Churnish_derived' = df$`AVG Average Daily Balance ALL Month Primary` / df$`AVG Counter Deposits ALL Month Primary`,
      
      # How long can balance potentially go?
      'ADB_minus_StdevADB_derived' = df$`AVG Average Daily Balance ALL Month Primary` - df$`STDEV Average Daily Balance ALL Month Primary`,
      
      # How high can NDs potentially go?
      'AvgNDs_plus_StdevNDs_derived' = df$`AVG Negative Days ALL Month Primary` + df$`STDEV Negative Days ALL Month Primary`,
      
      # NDs trend
      'LastMonthNDs_vs_AvgNDs_derived' = ((df$`Negative Days Last Month Primary` - df$`AVG Negative Days ALL Month Primary`) / df$`AVG Negative Days ALL Month Primary`) * 100,
      
      # proportion of trades which are always satisfactory
      'PCT_SAT_derived' = df$PG1_SAT / df$PG1_TRADES,
      
      # relation between NEW and OLD
      'NEW_vs_OLD_derived' = df$PG1_NEW / df$PG1_OLD,
      
      # Percent new trades
      'PercentNewTrades_derived' = 100*ifelse(df$PG1_TRADES != 0, df$PG1_OPEN12 / df$PG1_TRADES, 0),
      
      #Fraction of deposits under 10k
      'frac_Deposits_Under_10k_derived' = 100*ifelse(df$`SUM Deposits Under 10k ALL Month Primary` != 0, df$`SUM Deposits Under 10k ALL Month Primary` / df$`SUM Deposit Number ALL Month Primary`, 0),
      
      #OKRATIO
      'OKRATIO_derived' = df$PG1_OKTRADE/df$PG1_TRADES
      
  
    )
    
    
    new_vars$LastMonthNDs_vs_AvgNDs_derived[ is.na(new_vars$LastMonthNDs_vs_AvgNDs_derived) ] <- 0
    new_vars$AvgNDs_plus_StdevNDs_derived[ is.na(new_vars$AvgNDs_plus_StdevNDs_derived) ] <- 0
    new_vars$Churnish_derived[ is.infinite(new_vars$Churnish_derived) ] <- 1.25 # from looking at distribution on training data set
    
  
    df = cbind(df, new_vars)
  
  return(df)
}



adjustVars = function(df) {
  
  # Create new variables and truncate extreme values
  

  df$BalanceVol = pmin(df$BalanceVol, 100) # Because min balance can be negative
  df$BalanceIncrease = pmax(pmin(df$BalanceIncrease, 100), -100) # Doubling of ADB in last month is extreme
  df$ADB = pmax(pmin(df$avg_Avg_Daily_Balance, 75000),1500) # We don't fund below 1500, and 75k is near 95% quantile
  
  # We don't fund above 10 max NDs
  df$max_NDs = pmin(df$max_NDs, 10)
  
  # More than 5 NSFs per month is extreme for funded loans
  df$avg_NSFs = pmin(df$avg_NSFs, 5)
  
  # Cap deposits at 50: ~2 per day on average
  df$nDeposits = pmax(pmin(df$avg_Deposit_cnt, 50),1)
  
  # Possible low balance has a floor of zero
  df$Stdev_Min_Balance = df$avg_Avg_Daily_Balance - df$stdev_Avg_Daily_Balance
  df$Stdev_Min_Balance = pmax(pmin(df$Stdev_Min_Balance,50000),0)
  
  # We don't fund below 150k AGS
  df$AGS = pmax(df$AGS,150000) 
  df$DailySales  = df$AGS/(12*22)
  df$logSales = log(df$DailySales)
  
  # Cap number of platinum payments supported at 6 months
  df$PlatinumPayments = pmin(df$PlatinumPayments, 132)
  
  # TIB: Cap at 20 years, and we dont fund below 1 year generally
  df$TIB = pmax(pmin(df$TIB,20),1)
  
  # Trade counts capped
  df$TRADES = pmax(pmin(df$TRADES, 40),0)
  df$SAT = pmax(pmin(df$SAT, 40),0)
  
  # Cap # Mortgages at 5
  df$MTRADE = pmax(pmin(df$MTRADE, 5), 0)
  
  # Cap # Rev trades with balances at 10
  df$RCURB = pmax(pmin(df$RCURB, 10), 0)
  
  # Very few have more than 5 collections
  df$COLLS = pmin(df$COLLS, 5)
  df$AnyCOLLS = df$COLLS >= 1
  
  # If there are no PG trades at all, then 0 percent are new
  df$PercentNewTrades = 100*ifelse(df$TRADES != 0, df$OPEN12 / df$TRADES, 0)
  
  # We don't fund FICO below 500
  df$Max_FICO = pmax(pmin(df$Max_FICO, 800),500)
  
  # More than 15 Inquiries is about the 95% quantile
  df$INQ12 = pmin(df$INQ12, 15)
  df$INQ6 = pmin(df$INQ6, 15)
  
  # Have any Past Due if the PAST DUE > 100
  df$AnyPastDue = df$PAST >= 100
  
  # Cap 30 day occurances at 5
  df$OCCUR6_30 = pmin(df$OCCUR6_30, 5)
  
  df$OKRATIO = df$OKTRADE/df$TRADES
  
  # RUTIL = 999 convert to 0
  df[!is.na(df$RUTIL) & df$RUTIL == 999, 'RUTIL'] = 0
  
  return(df)
}




