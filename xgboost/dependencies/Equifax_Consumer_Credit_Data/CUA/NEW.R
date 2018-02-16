

NEW.EQF.FACTORY = function(asof) {
  return(function(model) {
    if(is.null(asof)) {
      asof = model$report_dt
    }
    if(!model$HIT) {
      return(NA)
    }
    trades = model$trades
    if(is.null(trades) || length(trades) == 0) {
      return(NA)
    } else {
      open_dates = lapply(trades, function(t) t$OpenDate_dt)
      open_dates = open_dates[!is.na(open_dates)]
      if(length(open_dates) == 0) {
        return(NA)
      }
      
      most_recent = max(structure(unlist(open_dates), class = "Date"))
      return(as.numeric( round( (asof - most_recent) /30.4)))
    }
  }
  )
}