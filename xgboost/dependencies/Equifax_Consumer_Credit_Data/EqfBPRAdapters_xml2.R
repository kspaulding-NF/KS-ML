# Adapt USTrade node
adaptTrade_xml2 = function(xml_trade, ReportDate) {
  xml_trade = xml2::as_list(xml_trade)
  trade = list()
  trade$BalanceAmount = integer.or.zero(xml_trade$BalanceAmount)
  trade$CreditLimit = integer.or.zero(xml_trade$CreditLimit)
  trade$AmountPastDue = integer.or.zero(xml_trade$PastDueAmount)
  
  trade$OpenDate_dt = EQF_dt(xml_trade$DateOpened)
  trade$Balance_dt = EQF_dt(xml_trade$DateReported)
  
  if(!is.null(xml_trade[['PortfolioType']])) {
    trade$is_revolving = attr(xml_trade$PortfolioType, 'code') == "R"
    trade$is_mortgage = attr(xml_trade$PortfolioType, 'code') == "M"        
  } else {
    trade$is_revolving = FALSE
    trade$is_mortgage = FALSE
  }
  
  if(!is.null(xml_trade$Status)) {
    trade$status = attr(xml_trade$Status, 'code')
  } else {
    trade$status = NA
  }
  
  trade$Satisfactory = is.null(xml_trade[['PreviousHighPaymentRates']]) && 
    is.null(xml_trade[['DateOfFirstDelinquency']]) &&
    !is.na(trade$status) && trade$status == "1"
  
  if(is.null(xml_trade[['PaymentHistory']])) {
    trade$PaymentProfile_vec = NA
  } else {
    PaymentProfile_vec = unlist(strsplit(xml_trade[['PaymentHistory']][[1]],''))
    PaymentProfile_vec = PaymentProfile_vec[PaymentProfile_vec != "/"]
    trade$PaymentProfile_vec = PaymentProfile_vec
  }
  
  # Exclusion criteria for trades
  is_excluded = FALSE
  is_excluded = is_excluded ||
                ( # SPOUSE SUPPORT, CHILD SUPPORT, FAMILY SUPPORT
                  !is.null(xml_trade[['AccountType']]) &&
                  attr(xml_trade[['AccountType']], 'code') %in% c('94', '93', '50')) ||
                ( # Lost/Stolen
                  !is.null(xml_trade[['ActivityDesignator']]) &&
                  attr(xml_trade[['ActivityDesignator']], 'code') %in% c("L")
                ) ||
                ( # Authorized User
                  !is.null(xml_trade[['AccountDesignator']]) &&
                  attr(xml_trade[['AccountDesignator']], 'code') %in% c("A")
                )
  trade$Excluded = is_excluded
  
  # Determine if the trade is "closed"
  is_closed = FALSE
  is_closed = is_closed ||
              # There is a date closed element
              !is.null(xml_trade[['DateClosed']]) ||
              ( # Paid and Closed, Closed, Transfer/Sold/Paid, Lost/Stolen, Refinanced, Transfer/Sold
                !is.null(xml_trade[['ActivityDesignator']]) &&
                attr(xml_trade[['ActivityDesignator']], 'code') %in% c("B", "C", "D", "L", "R", "T")
              ) ||
              ( # Terminated for this consumer or is only an Authorized User
                !is.null(xml_trade[['AccountDesignator']]) &&
                attr(xml_trade[['AccountDesignator']], 'code') %in% c("A", "T")
              ) || 
              ( # Status. All but the following are considered closed: 
                #    0 = Too new to rate; Approved but not used
                #    1 = Pays account as agreed
                #    2 = Not more than two payments past due
                #    3 = Not more than three payments past due
                #    4 = Not more than four payments past due
                #    5 = At least 120 days or more than four payments past due
                #    6 = Collection account
                !is.null(xml_trade[['Status']]) &&
                !(attr(xml_trade[['Status']], 'code') %in% 0:6)
              ) || 
              ( # Is Excluded from Equifax based criteria
                is_excluded
              )    
  
  trade$OpenOrClosed = ifelse(is_closed, "C", "O")
  # Trade is open if DateClosed is missing
  
  
  # Determine if the trade is "active" from the point of view of NF credit policy
  trade$LastActivity = EQF_dt(xml_trade$DateOfLastActivity)
  inactive =    is_closed ||
              ( # Status. Remove open collections or very delinquent trades
                is.null(xml_trade[['Status']]) &&
                (attr(xml_trade[['Status']], 'code') %in% c(5,6))
              ) ||
              ( # More than a year since last activity
                !is.na(trade$LastActivity) &&
                (ReportDate - trade$LastActivity)/365 > 1
              ) ||
              ( # More than a year since last time the trade info was reported
                !is.na(trade$Balance_dt) &&
                (ReportDate - trade$Balance_dt)/365 > 1
              ) || 
              ( # Is Excluded from Equifax based criteria
                is_excluded
              )
  trade$Active = !inactive
  
  
  # MonthlyPaymentAmount, based on ActualPaymentAmount or ScheduledPaymentAmount depending on what is available
  trade$MonthlyPaymentAmount = NA
  
  ActualPaymentAmount = xml_trade[['ActualPaymentAmount']]
  ScheduledPaymentAmount = xml_trade[['ScheduledPaymentAmount']]
  
  if(!is.null(ScheduledPaymentAmount)) { # First try with ScheduledPaymentAmount
    trade$MonthlyPaymentAmount = integer.or.zero(ScheduledPaymentAmount)
  } else if(!is.null(ActualPaymentAmount)) { # Next try with ActualPaymentAmount
    trade$MonthlyPaymentAmount = integer.or.zero(ActualPaymentAmount)
  } 
  
  # Indicate if a tradeline has a 30, 60, 90, 120+ day DQ in the past 12 months
  # Initialized to 0 and filled in later
  trade$Recent30D  = 0
  trade$Recent60D  = 0
  trade$Recent90D  = 0
  trade$Recent120D = 0
  trade$Recent150D = 0
  trade$Recent180D = 0
  
  
  return(trade)
}



# Adapt USInquiry node
adaptInquiry_xml2 = function(xml_inquiry, ReportDate) {
  xml_inquiry = xml2::as_list(xml_inquiry)
  inquiry = list()
  inquiry$Date_dt = EQF_dt((xml_inquiry[['DateOfInquiry']]))
  return(inquiry)
}


# Adapt USTaxLiens nodes
adaptLien_xml2 = function(xml_lien, ReportDate) {
  lien = list()
  lien$Amount = integer.or.zero(
                  xml2::xml_text(
                    xml2::xml_find_one(xml_lien, 'Amount'), trim = TRUE))
  
  lien$ReleaseDate = EQF_dt(
                      xml2::xml_text(
                        xml2::xml_find_one(xml_lien, 'ReleaseDate'), trim = TRUE))
  
  lien$DateFiled = EQF_dt(
                    xml2::xml_text(
                      xml2::xml_find_one(xml_lien, 'DateFiled'), trim = TRUE))
  
  lien$isOpen = is.na(lien$ReleaseDate)
  return(lien)
}

# Adapt USLegalItem nodes
adaptJudgement_xml2 = function(xml_judge, ReportDate) {
  judge = list()
  judge$Amount = integer.or.zero(
                    xml2::xml_text(
                      xml2::xml_find_one(xml_judge, 'Amount'), trim = TRUE))
  
  judge$DateSatisfied = EQF_dt(
                          xml2::xml_text(
                            xml2::xml_find_one(xml_judge, 'DateSatisfied'), trim = TRUE))
  
  judge$DateFiled = EQF_dt(
                      xml2::xml_text(
                        xml2::xml_find_one(xml_judge, 'DateFiled'), trim = TRUE))
  
  judge$isOpen = is.na(judge$DateSatisfied)
  return(judge)
}


# Adapt USCollection nodes
adaptCollection_xml2 = function(xml_col, ReportDate) {
  xml_col = xml2::as_list(xml_col)
  collection = list()
  collection$OriginalAmount = integer.or.zero((xml_col[['OriginalAmount']]))
  collection$BalanceAmount = integer.or.zero((xml_col[['BalanceAmount']]))
  collection$DateReported = EQF_dt((xml_col[['DateReported']])) 
  collection$AssignedDate = EQF_dt((xml_col[['AssignedDate']])) 
  collection$StatusDate = EQF_dt((xml_col[['StatusDate']])) 
  if(!is.null(xml_col[['Status']])) {
    collection$Status = attr(xml_col[['Status']], 'code')
  } else {
    collection$Status = NA
  }
  
  # Exclusion criteria for collections
  is_excluded = FALSE
  is_excluded = is_excluded ||
                ( # Disputed Collections
                  !is.null(xml_col[['Status']]) &&
                    attr(xml_col[['Status']], 'code') %in% c("S")
                ) ||
                ( # Medical Collections
                  !is.null(xml_col[['CreditorClassification']]) &&
                    attr(xml_col[['CreditorClassification']], 'code') %in% c("02")
                )
  collection$Excluded = is_excluded
  return(collection)
}


# Adapt USBankruptcy nodes
adaptBK_xml2 = function(xml_bk, ReportDate) {
  xml_bk = xml2::as_list(xml_bk)
  bk = list()
  bk$DateReported = EQF_dt((xml_bk[['DateReported']])) 
  bk$DateFiled = EQF_dt((xml_bk[['DateFiled']])) 
  disposition = xml_bk[['Disposition']]
  if(!is.null(disposition)) {
    disposition_code = attr(disposition, 'code')
    if(length(disposition) > 0) {
      disposition_dt = EQF_dt(disposition[[1]][[1]])
    } else {
      disposition_dt = NA
    }
    
    # for Discharged CH-7, CH-13, and CH-11. We are treating dismissed/closed as not discharged
    bk$isDischarged = disposition_code %in% c('A', 'L', 'F') 
    if (bk$isDischarged) {
      bk$DischargeDate = disposition_dt
    } else {
      bk$DischargeDate = NA
    }
  }
  return(bk)
}
