

OLD.EQF.FACTORY <- function(asof) {
  # OLD - Age of Oldest Trade in Months
  return(function(model) {
    if(is.null(asof)) {
      asof = model$report_dt
    }
    if(!model$HIT || is.null(model$trades) || length(model$trades) == 0) {
      return(NA)
    }
    
    open_dates = sapply(model$trades, function(t) t$OpenDate_dt)
    open_dates = open_dates[!is.na(open_dates)]
    if(length(open_dates) == 0) {
      return(NA)
    }
    return(as.integer(
            round(
              max(
                sapply(open_dates, function(dt) asof-dt)/30.4))))
  })
}