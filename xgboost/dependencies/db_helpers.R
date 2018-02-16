

getDBConnection_ODBC <- function(server, dbname) {
  require(RODBC)
  # Using ODBC
  connectionString = paste0("Driver=ODBC Driver 11 for SQL Server;Server=", server, "; Database=", dbname, ";Trusted_Connection=yes")
  con <- RODBC::odbcDriverConnect(connectionString)
  return(con)
}




dbQuery_ODBC <- function(server, dbname, query = NULL, table = NULL) {
  if (is.null(query) && is.null(table)) {
    return(NULL);
  }
  else if ( is.null(query) && !is.null(table) ) {
    query = paste0('SELECT * FROM ', table)
  }
  
  require(RODBC)
  con = getDBConnection_ODBC(server, dbname)
  df = RODBC::sqlQuery(con, query, stringsAsFactors=F, as.is=T) 
  RODBC::odbcClose(con)
  return(df)
}


getDBConnection <- function(server, 
                            dbname = 'Bureau_DataAnalytics',
                            credentials = NULL,
                            driver = "c:/Projects/sqljdbc_4.2/enu/sqljdbc42.jar") {
  # Assumes a specific driver and jarfile for the connection string. Change here if neccessary
  drv <- RJDBC::JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", driver)
  if (!is.null(credentials)) {
    # Use explicit username and password
    username = credentials$username
    password = credentials$password
    con <- RJDBC::dbConnect(drv, 
                            paste0("jdbc:sqlserver://", server, "; databaseName=", dbname, ";integratedSecurity=false"),
                            username,
                            password
    )
  } else {
    # Use integrated security
    con <- RJDBC::dbConnect(drv, paste0("jdbc:sqlserver://", server, "; databaseName=", dbname, ";integratedSecurity=true"))
  }
  
  return(con)
}



getEquifaxXMLByCCS <- function(con, ccs_id, use.default = TRUE) {
  
  defaultXML = '<EfxTransmit><EfxReport><USConsumerCreditReports><USConsumerCreditReport></USConsumerCreditReport></USConsumerCreditReports></EfxReport></EfxTransmit>'
  
  # Get Experian Pesonal XML result by looking up via the CCS Id
  query <- dbGetQuery(con, paste0("SELECT CAST(CreditReportXML AS VARCHAR(MAX)) AS XML FROM Equifax A inner join EquifaxRequest B on A.RequestId = B.RequestId WHERE Consumer_CreditId = '", ccs_id, "'"))
  
  xml_report = NA
  if (nrow(query) == 0 && !use.default) {
    return(NA)
  }
  else if(nrow(query) == 0 && use.default) {
    xml_report = defaultXML
  } else {
    xml_report = query$XML
  } 
  
  return(xml_report)
  
}
