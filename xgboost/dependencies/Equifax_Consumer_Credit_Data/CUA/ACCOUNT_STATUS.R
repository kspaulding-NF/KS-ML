

OKTRADE.EQF <- function(model) {
  # OKTRADE (Number of Currently Satisfactory Trades)
  # Computed as number of always satisfactory trades + number of current trades
  
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  
  # Compute trades with Status code = 1 --> Pays account as agreed
  OKTRADE = 0
  if(length(model$trades) > 0) {
    OKTRADE = integer.or.zero(
                sum(
                  sapply(model$trades,
                        function(t) !is.na(t$status) && t$status == "1")
                )
    )
  }
  return(OKTRADE)
}

SAT.EQF <- function(model) {
  # SAT - Number of trades always satisfactory
  
  if (!model$HIT) {
    # No hit. Return NA
    return(NA)
  }
  
  if(length(model$trades) == 0) {
    return(0)
  }
  SAT = integer.or.zero(
            sum(
              sapply(model$trades, 
                function(t) t$Satisfactory)))
  return(SAT)
}
