
# Total Balance
BAL.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  bal = 0
  if(!is.null(model$trades) && length(model$trades) > 0)  {
    bal = sum(sapply(model$trades, function(t) t$BalanceAmount))  
  }
  return(bal)
}

# Total Revolving Balance
RBAL.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  rbal = 0
  if(!is.null(model$revolving) && length(model$revolving) > 0)  {
    rbal = sum(sapply(model$revolving, function(t) t$BalanceAmount))
  }
  return(rbal)
}


# Total Mortgage Balance
MBAL.EQF = function(model) {
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  mbal = 0
  if(!is.null(model$mortgage) && length(model$mortgage) > 0)  {
    mbal = sum(sapply(model$mortgage, function(t) t$BalanceAmount))
  }
  return(mbal)
}