# Total high credit (CRED, RCRED, ICRED)


CRED.EQF <- function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  return(HighCRED_helper(model$trades))
}


RCRED.EQF <- function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  return(HighCRED_helper(model$revolving))
  
}


HighCRED_helper = function(trades) {
  CRED = 0
  if (!is.null(trades) && length(trades) > 0) {
    is_open = sapply(trades, function(t) t$OpenOrClosed == "O")
    if(any(is_open)) {
      CRED = sum(sapply(trades[is_open], function(t) t$CreditLimit))  
    }
  }
  return(CRED)
}