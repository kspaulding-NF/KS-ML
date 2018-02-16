

# Helper to get the total Monthly Payment Amount for the passed in trades
PAY_helper = function(trades) {
  if (is.null(trades) || length(trades) == 0) {
    return(0)
  }

  is_open = sapply(trades, function(t) t$OpenOrClosed == "O")
  if(any(is_open)) {
    pay_amt = sum(sapply(trades[is_open], function(t) integer.or.zero(t$MonthlyPaymentAmount)))
  } else {
    pay_amt = 0
  }
  return(pay_amt)  
}

# Total Monthly Payment Amount
PAY.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  }
  return(PAY_helper(model$trades))
}


# Total Monthly Payment Amount on Revolving Trades
RPAY.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  }
  return(PAY_helper(model$revolving))
}


# Total Monthly Payment Amount on Mortgage Trades
MPAY.EQF = function(model) {
  if(!model$HIT) {
    return(NA)
  }
  return(PAY_helper(model$mortgage))
}