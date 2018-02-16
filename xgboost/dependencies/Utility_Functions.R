
## Various utility functions


#' checkVars
#' Verifies that dataframe df contains variables in vars.
#' Throws an error with the list of missing vars
#'
#' @param df Dataframe
#' @param vars Dataframe
#'
#' @return None
checkVars = function(df, vars) {
  missing_vars = vars[!(vars %in% names(df))]
  if (length(missing_vars) > 0) {
    stop(paste0("ERROR: Dataframe is missing variables: ",
                paste0(missing_vars, collapse = ', ')))
  }
}

#' parseDate
#' Converts a vector of date or datetime strings to POSIXct with no time
parseDate = function(dt, fmt = "%Y-%m-%d") {
  result = as.POSIXct(strptime(dt, format = fmt))
  return (result)
}

datePart = function(datetimes) {
  date_str = strftime(datetimes, format = "%Y-%m-%d")
  dates = as.POSIXct(strptime(date_str, format="%Y-%m-%d"))
  return(dates)
}

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}