
runCCSEquifaxToCUA = function(input) {
  write('', stderr())
  write('<<<<   EQUIFAX CUA STARTED    >>>>', stderr())
  
  environment = input$environment
  if(is.null(environment) || length(environment) == 0) {
    write(sprintf("Error: No environment provided"), stderr())
    return(list(status = "Error", message = "No environment provided"));
  }
  
  write(sprintf('Running in [%s] environment', toupper(environment)), stderr())
  
  # Get configuration information based on the run environment
  config_data = tryCatch(loadConfig(environment),
                         error = function(e) {
                           return(list(status = 'Error', message = e$message))
                         })
  
  if('status' %in% names(config_data) && config_data$status == 'Error') {
    write(config_data$message, stderr())
    return(config_data) # Will contain error message with missing config info
  }
  
  write(sprintf('Successfully loaded configuration settings for (%s)', paste(names(config_data), collapse=', ')), stderr())
  
  ccs_ids = input$ccs_ids
  if (length(ccs_ids) == 0) {
    write(sprintf("Error: No Consumer Credit Summary Ids specified"), stderr());
    return(list(status = "Error", message = "No Consumer Credit Summary Ids specified"));
  } else if(length(ccs_ids) <= 5) {
    write(sprintf("Generating CUA variables for CCS_Id = [%s]", paste(ccs_ids, collapse = ', ')), stderr())
  } else {
    write(sprintf("Generating CUA variables for CCS_Id = [%i Total]", length(ccs_ids)), stderr())
  }
  
  start_total = proc.time()
  
  # Create the CUA vars
  CUA = createCUAVars()
  
  # Connect to the database on the server and grab the Equifax reports for the given
  # consumer credit summary ids
  
  # If the model is deployed on yHat, connect to the database based on specified environment variables
  if (!exists('deploy_mode') || deploy_mode != "local") {
    server = config_data[['DB_SERVER']]
    db_name = config_data[['DB_NAME']]
    con = getDBConnection(server,
                          dbname = db_name, 
                          credentials = list(username = config_data[['DB_USER']], password = config_data[['DB_PASSWORD']]),
                          driver = '~/local/Other_libs/sqljdbc42.jar')
  } else {
    # Use local settings
    server = '10.0.0.26'
    db_name = 'Bureau_DataAnalytics'
    creds <- list()
    creds[['username']] = 'Tableau'
    creds[['password']] = 'S3cretSquirrel'
    con = getDBConnection(server, db_name, creds)
  }
  write(sprintf("Connected to %s database on %s", db_name, server), stderr())
  
  start = proc.time()
  reports = lapply(ccs_ids, function(ccs_id) getEquifaxXMLByCCS(con, ccs_id, use.default=FALSE))
  dbDisconnect(con)
  elapsed = proc.time()-start
  write(sprintf("Successfully loaded %i Equifax XML reports. Elapsed time: %g seconds", length(reports), elapsed[[1]]), stderr())
  
  
  # Create data model from XML
  start = proc.time()
  BPR_data = lapply(reports, genCreditModel)
  elapsed = proc.time()-start
  write(sprintf("Pre-processing of XML reports completed. Elapsed time: %g seconds", elapsed[[1]]), stderr())
  
  # Delete the reports
  rm(list = c("reports"))
  
  # Compute CUA variables
  start = proc.time()
  CUA_result = lapply(BPR_data, function(x) genCUA(x, CUA))
  elapsed = proc.time()-start
  write(sprintf("Successfully computed CUA variables. Elapsed time: %g seconds", elapsed[[1]]), stderr())
  
  # Delete the BPR_data constructs since they have been converted to new data
  rm(list = c("BPR_data"))
  
  # Turn result into a dataframe
  start = proc.time()
  CUA_df = plyr::rbind.fill(lapply(CUA_result, data.frame))
  elapsed = proc.time()-start
  write(sprintf("CUA dataframe computed: [%g reports x %g variables]. Elapsed time: %g seconds", NROW(CUA_df), NCOL(CUA_df), elapsed[[1]]), stderr())
  
  # Delete the intermediate CUA_result
  rm(list = c("CUA_result"))
  
  # Done
  elapsed = proc.time()-start_total
  write(sprintf("Process fully completed. Elapsed time: %g seconds", elapsed[[1]]), stderr())
  
  CUA_result = data.frame(Id = ccs_ids, CUA_df)
  CUA_result$git = git_info
  
  return(CUA_result)
}