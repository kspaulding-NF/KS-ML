
loadConfig = function(env) {
  # Return config dictionary based on environment
  config_keys = c('DB_NAME',
                  'DB_PASSWORD',
                  'DB_SERVER',
                  'DB_USER')
  
  # Get value as yHat environment variable {env}-{config_key}
  config_data = lapply(paste0(env, '.', config_keys), Sys.getenv)
  names(config_data) = config_keys
  
  # Make sure we got data
  is_missing = sapply(config_data, function(c) c=='')
  if (any(is_missing)) {
    stop(sprintf('Missing [%s] configuration: (%s)', env, paste(names(config_data[is_missing]), collapse = ', ')))
  }
  
  return(config_data)
}