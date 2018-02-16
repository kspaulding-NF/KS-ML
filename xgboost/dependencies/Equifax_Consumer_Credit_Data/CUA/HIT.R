# Indicates if we have a hit. Just a passthrough to model$hit
HIT.EQF = function(model) {
  if (!model$HIT) {
    return(0)
  } else {
    return(1)
  }
}