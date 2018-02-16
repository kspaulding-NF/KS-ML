FICO.EQF = function(model) {
  if (!model$HIT) {
    return(NA)
  } else {
    return(model$FICO)
  }
}