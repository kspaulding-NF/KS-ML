# Revolving Trade Utilization
RUTIL.EQF = function(model) {
  if (!model$HIT) {
    return(NA)
  } 
  rcred = RCRED.EQF(model)
  rbal = RBAL.EQF(model)
  if (is.na(rbal) || is.na(rcred) || rcred == 0) {
    return(0)
  } else {
    return(min(max(100*(rcred-rbal)/rcred,0),100))
  }
}
