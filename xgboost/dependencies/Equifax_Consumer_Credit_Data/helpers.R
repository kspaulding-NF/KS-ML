# Helper functions for processing Equifax XML



#' Convert Equifax Date to R Date object
#'
#' @param dt_str Input date from Equifax XML. 
#'
#' @return Date object
EQF_dt = function(dt_str) {
  if (is.null(dt_str) || is.na(dt_str)) {
    return(NA)
  }
  dt_str = gsub('/00/', '/01/', dt_str)
  return(as.Date(as.character(dt_str), "%m/%d/%Y"))
}


#' Check if difference in months between two dates <= T
#'
#' @param dt   Earlier date
#' @param asof Later date
#'
#' @return True if difference in months is less than T. False otherwise, or if any inputs are NA
in_window = function(dt, asof, T) {
  if (length(dt) == 0 || is.null(dt) || is.na(dt) || is.na(asof) || is.na(T)) {
    return(FALSE)
  }
  
  return(((asof - dt))/30.42 <= T)
}

# Convert a string to an integer, and if it cannot be converted, set to default value
integer.or.default = function(s, default = 0) {
  if (is.null(s) || length(s) == 0) {
    return(default)
  } 
  int = suppressWarnings(as.integer(s))
  if (is.na(int)) {
    return(default)
  } else {
    return(int)
  }
}


# Convert a string to an integer, and if it cannot be converted, set to NULL
integer.or.null = function(s) {
  return(integer.or.default(s, default = NA))
}


# Convert a string to an integer, and if it cannot be converted, set to NA
integer.or.na = function(s) {
  return(integer.or.default(s, default = NA))
}


# Convert a string to an integer, and if it cannot be converted, set to zero
integer.or.zero = function(s) {
  return(integer.or.default(s, default = 0))
}

