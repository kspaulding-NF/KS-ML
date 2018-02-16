

# Converts D day delinq to character in equifax payment profle
# Note that in equifax payment profile string, "5" is 
# "At least 120 days or more than four payments past due",
# So we pretend that 150 and 180 day occurances are zero and lump them in with 120
DDtoPaymentProfileCode = function(D) {
  P = ''
  if(D == 30) {
    P = '2'
  } else if(D == 60) {
    P = '3'
  } else if(D == 90) {
    P = '4'
  } else if(D == 120) {
    P = '5'
  } else if(D == 150) {
    P = ''
  } else if(D == 180) {
    P = ''
  }
  return(P)
}

# Helper function to compute # of DD = {30,60,90,120} occurances in a given trade in the past T months
trade_occur = function(trade, asof, T, DD) {
  
  # Convert DD to corresponding char in payment string
  P = DDtoPaymentProfileCode(DD)
  if (P == '') {
    return(0) # No corresponding character so no occurances!
  }
  
  if(is.null(trade$PaymentProfile_vec) || is.na(trade$PaymentProfile_vec)) {
    return(0)
  }
  
  return(sum(trade$PaymentProfile_vec[1:min(T, length(trade$PaymentProfile_vec))] == P))
}

# Number of D Day Occurrences in Last T Months, for D = {30,60,90,120}, T = {6, 12, 18, 24}
OCCUR.EQF.FACTORY = function(T, D, asof) {
  return(function(model) {
    if(is.null(asof)) {
      asof = model$report_dt
    }
    if(!model$HIT) {
      return(NA)
    }
    trades = model$trades
    if(is.null(trades) || length(trades) == 0) {
      return(0)
    } else {
      # Get list of tradelines, pass each through the helper function that 
      # counts the number of DD occurances in that past T months
      return(sum(sapply(trades, function(t) trade_occur(t, asof, T, D))))
    }
  })
}


# CURR{D} -> Number of Trades Currently D Days, for D = {30, 60, 90, 120}
CURR.EQF.FACTORY = function(D, asof) {
  return(OCCUR.EQF.FACTORY(1, D, asof))
}



# Percentage of Trades DD-120+ Days Past Due in Last 12 Months, for DD = {30,60,90}
PCT12.EQF.FACTORY = function(DD, asof) {
  
  return(function(model) {
    if(is.null(asof)) {
      asof = model$report_dt
    }
    
    if(!model$HIT) {
      return(NA)
    }
    
    if(is.null(model$trades) || length(model$trades) == 0) {
      return(0)
    }
    
    T = 12 # Fixed 12 month look back horizon
    
    # number of trades with a balance date in the past T months
    is_recent = sapply(model$trades, function(t) in_window(t$Balance_dt, asof, T))
    nRecent = sum(is_recent)
    
    if(nRecent == 0) {
      return(0)
    }
    
    nRecent_DD = sum(sapply(model$trades, 
                            function(t) {
                              has_dq = t$Recent120D || t$Recent150D || t$Recent180D
                              if(DD <= 90) {
                                has_dq = has_dq || t$Recent90D
                              }
                              if(DD <= 60) {
                                has_dq = has_dq || t$Recent60D
                              }
                              if(DD <= 30) {
                                has_dq = has_dq || t$Recent30D
                              }
                              return(has_dq)
                            }
    )
    )
    
    # Compute percent of trades with at least 1 occurance of {DD, ... , 120}
    return (round(100*nRecent_DD / nRecent))
    
  })
}