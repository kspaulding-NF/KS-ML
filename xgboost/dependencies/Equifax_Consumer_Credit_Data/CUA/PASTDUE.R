


# Total Past Due Amount
PAST.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  }
  past_due = 0
  if(!is.null(model$trades) && length(model$trades) > 0)  {
    past_due = sum(sapply(model$trades, function(t) t$AmountPastDue))  
  }
  return(past_due)
}

# Total Past Due Amount on Revolving Trades
RPAST.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  }
  rpast_due = 0
  if(!is.null(model$revolving) && length(model$revolving) > 0) {
    rpast_due = sum(sapply(model$revolving, function(t) t$AmountPastDue))  
  }
  return(rpast_due)
}