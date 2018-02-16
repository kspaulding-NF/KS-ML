
# Number of unsatisfied Judgments
JDGMNT.EQF = function(model) {
  
  if(!model$HIT) {
    return(NA)
  } else if (length(model$judgements) == 0) {
    return(0)
  } else {
    # Count number of lien records with no DateSatisfied
    return(sum(sapply(model$judgements, function(j) is.na(j$DateSatisfied))))
  }
}

# Number of Unreleased Liens  
TXLIEN.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  } else if (length(model$liens) == 0) {
    return(0)
  } else {
    # Count number of lien records with no ReleaseDate
    return(sum(sapply(model$liens, function(l) is.na(l$ReleaseDate))))
  }
}

# Number of open public records
PUBOPEN.EQF = function(model) {
  return(TXLIEN.EQF(model) + JDGMNT.EQF(model))
}

# Open public records balance
PUBOPENBAL.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  } else if (PUBOPEN.EQF(model) == 0) {
    return(0)
  } 
  
  # HIT with 1+ Open Public Records
  
  # Balance on Open Judgements
  JDGMNT_BAL = 0
  if (length(model$judgements) > 0) {
    open_judgements = sapply(model$judgements, function(j) is.na(j$DateSatisfied))
    if(any(open_judgements)) {
      JDGMNT_BAL = sum(sapply(model$judgements[open_judgements], function(j) j$Amount))
    }
  }
  
  # Balance on Open Tax Liens
  TXLIEN_BAL = 0
  if (length(model$liens) > 0) {
    open_liens = sapply(model$liens, function(l) is.na(l$ReleaseDate))
    if(any(open_liens)) {
      TXLIEN_BAL = sum(sapply(model$liens[open_liens], function(l) l$Amount))
    }
  }
  
  return (JDGMNT_BAL + TXLIEN_BAL)
}

# Number of Public Record Bankruptcies
PUBBKP.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  } else if (length(model$bks) == 0) {
    return(0)
  } else {
    nBK = length(model$bks)
    return(nBK)
  }
}

# Bankruptcy Flag
BKP_FLAG.EQF = function(model) {
  return(ifelse(PUBBKP.EQF(model) > 0, 1, 0))
}

# Discharge date of most recent BK. Null if most recent BK not discharged
BKDISCHARGE.EQF = function(model) {
  # Discharge date of the most recent BK, if it has been discharged
  if(!model$HIT || length(model$bks) == 0) {
    return(NA_character_)
  } 
  
  discharge_dt = NA_character_
  if (length(model$bks) > 0) {
    
    status_dates = lapply(model$bks, function(bk) bk$DateReported)
    filing_dates = lapply(model$bks, function(bk) bk$DateFiled)
    
    # For each BK, compute the number of months since the filing date and the status date, and take the max of the two
    # THe point is to figure out which is more recent, status, and which BK is most recent
    time_since_status = sapply(status_dates, function(dt) (model$report_dt - dt))
    time_since_filed = sapply(filing_dates, function(dt) (model$report_dt - dt))
    time_since_bk = pmin(time_since_status, time_since_filed, na.rm = TRUE)

    bk_order = order(time_since_bk, decreasing = FALSE, na.last = TRUE) # Sort the BKs to find the most recent
    recent_bk = model$bks[[bk_order[1]]]      # the most recent BK

    is_discharge = recent_bk$isDischarged 
    if (is_discharge) {
      discharge_dt = format(model$report_dt - time_since_bk[bk_order[1]], "%Y-%m-%d")# Date of discharge if it is a discharged bk
    }
  }
  return(discharge_dt)
}
