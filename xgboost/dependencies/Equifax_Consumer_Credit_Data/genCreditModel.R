
# Adapters to go from Equifax BPR nodes to elements without our credit model
EquifaxAdapters = list(
  
  trades      = list(node    = "USTrades/USTrade", 
                     adapter = adaptTrade_xml2),
  
  collections = list(node    = "USCollections/USCollection",
                     adapter = adaptCollection_xml2),
  
  liens       = list(node    = "USTaxLiens/USTaxLien",
                     adapter = adaptLien_xml2),
  
  judgements  = list(node    = "USLegalItems/USLegalItem",
                     adapter = adaptJudgement_xml2),
  
  inquiries   = list(node    = "USInquiries/USInquiry",
                     adapter = adaptInquiry_xml2),
  
  bks         = list(node    = "USBankruptcies/USBankruptcy",
                     adapter = adaptBK_xml2)
)

# Return a credit model with no data and HIT = FALSE
emptyEqfModel = function() {
  return(list(
    report_dt = NA,
    HIT = FALSE,
    FICO = NA,
    trades = list(),
    revolving = list(),
    mortgage = list(),
    collections = list(),
    inquiries = list(),
    judgements = list(),
    liens = list(),
    bks = list()
  ))
}

genCreditModel = function(xml_report) {
  if(is.na(xml_report)) {
    return(emptyEqfModel())
  }
  
  
  xml_report = gsub("[\r\n]\\s*", "", xml_report) # Remove extra newlines and whitespaces
  
  # Parse the xml
  doc = tryCatch(xml2::read_xml(xml_report),
                 error = function(e) {
                   return(NULL) # could not parse
                 })
  if (is.null(doc)) {
    return(emptyEqfModel())
  }
  
  USConsumerCreditReports = xml2::xml_find_all(doc, 'EfxReport/USConsumerCreditReports') 
  if(length(xml2::xml_find_all(USConsumerCreditReports, 'USErrorMessages'))) {
    # There is an error message. Return an empty model
    return(emptyEqfModel())
  }
    
  USConsumerCreditReport = USConsumerCreditReport = xml2::xml_find_one(USConsumerCreditReports, 'USConsumerCreditReport')
  
  if(length(xml2::xml_find_all(USConsumerCreditReport, 'USErrorMessages'))) {
    # There is an error message. Return an empty model
    return(emptyEqfModel())
  }
  
  
  results = emptyEqfModel() # Empty model result that will be filled in as we go
  
  CreditFile = xml2::xml_find_one(USConsumerCreditReport, 'USHeader/USCreditFile')[[1]]
  if (length(CreditFile) > 0) {
    CreditFile = xml2::as_list(CreditFile)
    
    # Whether or not there is a hit
    hitcode = attr(CreditFile$HitCode, 'code')
    results$HIT = hitcode == "1"
    
    results$report_dt = EQF_dt(CreditFile[['DateOfRequest']]) # Request date
  }
  
  results$FICO = integer.or.na(
                  xml2::xml_text(
                    xml2::xml_find_all(USConsumerCreditReport, 'USBeacon/BeaconScore'))) # FICO score
  
  # Adapt the node sets to the model
  for (j in 1:length(EquifaxAdapters)) {
    adapted = list()
    x = EquifaxAdapters[[j]]
    nodes = xml2::xml_find_all(USConsumerCreditReport, x$node)
    if (length(nodes) > 0) {
      adapted = lapply(nodes, function(node) x$adapter(node, ReportDate = results$report_dt))
    }
    results[[names(EquifaxAdapters)[j]]] = adapted
  }
  
  # Remove excluded trades and collections
  for (v in c('trades', 'collections')) {
    accounts = results[[v]]
    if (length(accounts) > 0) {
      results[[v]] = accounts[sapply(accounts, function(a) !a$Excluded)]
    }
  }
  
  # Pad the payment profile vector with '-' characters up to the report date
  if(length(results$trades) > 0) {
    for(i in 1:length(results$trades)) {
      trade = results$trades[[i]]
      if(!is.na(trade$Balance_dt) && !is.na(trade$PaymentProfile_vec)) {
        months_since_bal_dt = max(as.numeric(floor((results$report_dt - trade$Balance_dt)/30.42)),0)
        results$trades[[i]]$PaymentProfile_vec = c(rep('-', months_since_bal_dt), trade$PaymentProfile_vec)
        
        # Indicate if a tradeline has a 30, 60, 90, 120+ day DQ in the past 12 months
        # Note, with equifax we cant distinguish 120 vs 150 vs 180 in the payment string
        T = min(12, length(results$trades[[i]]$PaymentProfile_vec))
        results$trades[[i]]$Recent30D  = '2' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        results$trades[[i]]$Recent60D  = '3' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        results$trades[[i]]$Recent90D  = '4' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        results$trades[[i]]$Recent120D = '5' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        results$trades[[i]]$Recent150D = '5' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        results$trades[[i]]$Recent180D = '5' %in% results$trades[[i]]$PaymentProfile_vec[1:T]
        
      }
    }
  }
  
  # Create lists of revolving and mortgage trades
  if(!is.null(results$trades) && length(results$trades) > 0)  {
    results$revolving = results$trades[sapply(results$trades, function(t) t$is_revolving)]
    results$mortgage  = results$trades[sapply(results$trades, function(t) t$is_mortgage)]
  }
  
  return(results)
}