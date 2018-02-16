


# Number of Trades Opened in Last T months, for T = {3,6,9,12,24}
OPEN.EQF.FACTORY = function(T, asof) {
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
      return(
        sum(
          sapply(
            trades,
            function(t)
              in_window(t$OpenDate_dt, asof, T)
          )
        )
      )
    }
  }
  )
}

