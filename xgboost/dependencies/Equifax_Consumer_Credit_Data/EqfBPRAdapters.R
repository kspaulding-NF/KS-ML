# Adapt USTrade node
adaptTrade = function(xml_trade) {
  trade = list()
  trade$BalanceAmount = integer.or.zero(xmlValue(xml_trade[['BalanceAmount']]))
  trade$CreditLimit = integer.or.zero(xmlValue(xml_trade[['CreditLimit']]))
  trade$AmountPastDue = integer.or.zero(xmlValue(xml_trade[['PastDueAmount']]))
  
  trade$OpenDate_dt = EQF_dt(xmlValue(xml_trade[['DateOpened']]))
  trade$Balance_dt = EQF_dt(xmlValue(xml_trade[['DateReported']]))
  
  if(!is.null(xml_trade[['PortfolioType']])) {
    trade$is_revolving = xmlAttrs(xml_trade[['PortfolioType']])['code'] == "R"
    trade$is_mortgage = xmlAttrs(xml_trade[['PortfolioType']])['code'] == "M"        
  } else {
    trade$is_revolving = FALSE
    trade$is_mortgage = FALSE
  }
  
  if(!is.null(xml_trade[['Status']])) {
    trade$status = xmlAttrs(xml_trade[['Status']])['code']   
  } else {
    trade$status = NA
  }
  
  trade$Satisfactory = is.null(xml_trade[['PreviousHighPaymentRates']]) && 
                       is.null(xml_trade[['DateOfFirstDelinquency']]) &&
                       !is.na(trade$status) && trade$status == "1"
  
  PaymentProfile_vec = unlist(strsplit(xmlValue(xml_trade[['PaymentHistory']]),''))
  PaymentProfile_vec = PaymentProfile_vec[PaymentProfile_vec != "/"]
  trade$PaymentProfile_vec = PaymentProfile_vec
  
  # Trade is open if DateClosed is missing
  trade$OpenOrClosed = ifelse(is.null(xml_trade[['DateClosed']]), "O", "C")
  if(!is.null(xml_trade[['ActivityDesignator']]) &&  xmlAttrs(xml_trade[['ActivityDesignator']])['code'] == "T") {
    # Transfered or Sold
    trade$OpenOrClosed = "C"
  }
  
  # MonthlyPaymentAmount, based on ActualPaymentAmount or ScheduledPaymentAmount depending on what is available
  trade$MonthlyPaymentAmount = NA

  ActualPaymentAmount = xml_trade[['ActualPaymentAmount']]
  ScheduledPaymentAmount = xml_trade[['ScheduledPaymentAmount']]

  if(!is.null(ScheduledPaymentAmount)) { # First try with ScheduledPaymentAmount
    trade$MonthlyPaymentAmount = integer.or.zero(xmlValue(ScheduledPaymentAmount))
  } else if(!is.null(ActualPaymentAmount)) { # Next try with ActualPaymentAmount
    trade$MonthlyPaymentAmount = integer.or.zero(xmlValue(ActualPaymentAmount))
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
adaptInquiry = function(xml_inquiry) {
  inquiry = list()
  inquiry$Date_dt = EQF_dt(xmlValue(xml_inquiry[['DateOfInquiry']]))
  return(inquiry)
}

# Adapt USTaxLiens nodes
adaptLien = function(xml_lien) {
  lien = list()
  lien$Amount = integer.or.zero(xmlValue(xml_lien[['Amount']]))
  lien$ReleaseDate = EQF_dt(xmlValue(xml_lien[['ReleaseDate']]))
  lien$DateFiled = EQF_dt(xmlValue(xml_lien[['DateFiled']])) 
  lien$isOpen = is.na(lien$ReleaseDate)
  return(lien)
}

# Adapt USLegalItem nodes
adaptJudgement = function(xml_judge) {
  judge = list()
  judge$Amount = integer.or.zero(xmlValue(xml_judge[['Amount']]))
  judge$DateSatisfied = EQF_dt(xmlValue(xml_judge[['DateSatisfied']])) 
  judge$DateFiled = EQF_dt(xmlValue(xml_judge[['DateFiled']])) 
  judge$isOpen = is.na(judge$DateSatisfied)
  return(judge)
}

# Adapt USCollection nodes
adaptCollection = function(xml_col) {
  collection = list()
  collection$OriginalAmount = integer.or.zero(xmlValue(xml_col[['OriginalAmount']]))
  collection$BalanceAmount = integer.or.zero(xmlValue(xml_col[['BalanceAmount']]))
  collection$DateReported = EQF_dt(xmlValue(xml_col[['DateReported']])) 
  collection$AssignedDate = EQF_dt(xmlValue(xml_col[['AssignedDate']])) 
  collection$StatusDate = EQF_dt(xmlValue(xml_col[['StatusDate']])) 
  if(!is.null(xml_col[['Status']])) {
    collection$Status = xmlAttrs(xml_col[['Status']])['code']       
  } else {
    collection$Status = NA
  }
  return(collection)
}

# Adapt USBankruptcy nodes
adaptBK = function(xml_bk) {
  bk = list()
  bk$DateReported = EQF_dt(xmlValue(xml_bk[['DateReported']])) 
  bk$DateFiled = EQF_dt(xmlValue(xml_bk[['DateFiled']])) 
  disposition = xml_bk[['Disposition']]
  if(!is.null(disposition)) {
    disposition_code = xmlAttrs(disposition)[['code']]
    disposition_dt = EQF_dt(xmlValue(disposition[['Date']]))
    
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