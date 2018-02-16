
TRADES.EQF <- function(model) {
  if(!model$HIT) {
    return(NA)
  }
  return(length(model$trades))
}

RTRADE.EQF <- function(model) {
  # RTRADE - Number of revolving accounts on file
  if(!model$HIT) {
    return(NA)
  }
  return(length(model$revolving))
}

MTRADE.EQF <- function(model) {
  # MTRADE - Number of mortgage trades
  if(!model$HIT) {
    return(NA)
  }
  return(length(model$mortgage))
}


# Number of Collections
COLLS.EQF <- function(model) {
  if(!model$HIT) {
    return(NA)
  }
  return(length(model$collections))
}

# Number of Charge-Offs
CHOFF.EQF <- function(model) {
  if(is.null(model$trades) || length(model$trades) == 0) {
    return(0)
  }
  return(sum(
    sapply(model$trades, 
           function(t) !is.null(t$status) && t$status == '9'
    )
  )
  )
  
}


# Number of active trades
ACTIVE.EQF <- function(model) {
  # MTRADE - Number of mortgage trades
  if(!model$HIT) {
    return(NA)
  }
  if (length(model$trades) == 0) {
    return(0)
  }
  return(sum(sapply(model$trades, function(t) t$Active)))
}