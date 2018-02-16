
# Number of Inquiries in Last T months, for T = {3,6,9,12,15,18,24}
INQ.EQF.FACTORY = function(T, asof) {
  return(function(model) {
    if(is.null(asof)) {
      asof = model$report_dt
    }
    if(!model$HIT) {
      return(NA)
    }
    inq_dates = lapply(model$inquiries, function(i) i$Date_dt)
    inq_dates = inq_dates[!is.na(inq_dates)]
    if(length(inq_dates) == 0) {
      return(0)
    } else {
      return(
        sum(
          sapply(
            inq_dates,
            function(d)
              in_window(d, asof, T)
          )
        )
      )
    }
  })
}

