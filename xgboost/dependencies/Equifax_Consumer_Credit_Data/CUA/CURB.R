

# Helper to count number of trades passed in with positive balance
CURB_helper = function(trades) {
  npositive = 0
  if(!is.null(trades) && length(trades) > 0)  {
    npositive = sum(sapply(trades, function(t) t$BalanceAmount > 0))  
  }
  return(npositive)
}

CURB.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  return(CURB_helper(model$trades))
}

RCURB.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  return(CURB_helper(model$revolving))
}

MCURB.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  return(CURB_helper(model$mortgage))
}
