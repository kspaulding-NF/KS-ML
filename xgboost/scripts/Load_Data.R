
source('scripts/Setup_Environment.R')



#***************************************************************************************************#
#************ Execute stored procedure(s) on S26 to update SF Credit Review datasets****************#
#***************************************************************************************************#

#OPPORTUNITY LEVEL LOAN PERFORMANCE DATA
dbhandle <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
db_SP_Execute_ODBC <- sqlExecute(dbhandle, "EXEC [ks].[LoanPerformanceSP]")
RODBC::odbcClose(dbhandle)

write('Loading data from s26.Analytics_WS.ks....', stderr())

LoanPerformance = dbQuery_ODBC('s26', 'Analytics_WS', table= '.ks.LoanPerformance');
# Save it as an RDS file
filename = paste0(DATA_DIR, '/LoanPerformance_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(LoanPerformance, file = rds_file)




#OPPORTUNITY LEVEL GENERAL BUSINESS INFORMATION
dbhandle <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
db_SP_Execute_ODBC <- sqlExecute(dbhandle, "EXEC [ks].[CreditReviewBusinessInfoSP]")
RODBC::odbcClose(dbhandle)

write('Loading data from s26.Analytics_WS.ks....', stderr())

CreditReviewBusinessInfo = dbQuery_ODBC('s26', 'Analytics_WS', table= '.ks.CreditReviewBusinessInfo');
# Save it as an RDS file
filename = paste0(DATA_DIR, '/CreditReviewBusinessInfo_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(CreditReviewBusinessInfo, file = rds_file)





#OPPORTUNITY LEVEL GENERAL BANK INFORMATION
dbhandle <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
db_SP_Execute_ODBC <- sqlExecute(dbhandle, "EXEC [ks].[BankInfoAggSP]")
RODBC::odbcClose(dbhandle)

write('Loading data from s26.Analytics_WS.ks....', stderr())

BankInfoAgg = dbQuery_ODBC('s26', 'Analytics_WS', table= '.ks.BankInfoAgg');
# Save it as an RDS file
filename = paste0(DATA_DIR, '/BankInfoAgg_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(BankInfoAgg, file = rds_file)





#OPPORTUNITY LEVEL GENERAL CUA INFORMATION
#Update New PG1 CUA records
deploy_mode = "local"

# Connect to the database
server = '10.0.0.28'
db_name = 'Salesforce_repl'
creds <- list()

creds[['username']] = 'Tableau'
creds[['password']] = 'S3cretSquirrel'

con = getDBConnection(server, db_name, creds)

# CAPTURE DATA WHERE LAST CUA RETRO FILE WAS RUN

# Create a query to get Equifax CCS Ids
#CCS_query = "SELECT PG1_EQF_Cons_Summary__c FROM Credit_Review__c WHERE PG1_EQF_Cons_Summary__c IS NOT NULL AND CreatedDate > '02/10/2018' "
CCS_query = "SELECT PG1_EQF_Cons_Summary__c FROM Credit_Review__c WHERE PG1_EQF_Cons_Summary__c IS NOT NULL"
query_result = dbGetQuery(con, CCS_query)
dbDisconnect(con)

ccs_ids = unique(query_result$PG1_EQF_Cons_Summary__c)
write(sprintf("Loaded %g PG1_EQF_Cons_Summary__c values", length(ccs_ids)), stdout())

# Grab existing CUA df record IDs
con <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
ExistingIDs_query = "SELECT id as PG1_EQF_Cons_Summary__c FROM CUA_df_PG1 WHERE HIT = 1"
ExistingIDs_query_result = sqlQuery(con, ExistingIDs_query)
close(con)

ExistingIDs_query_result$Exist_Flag = 1
NeedCUA_ids = merge(x = query_result, y = ExistingIDs_query_result, by = "PG1_EQF_Cons_Summary__c", all.x = TRUE)
NeedCUA_ids = NeedCUA_ids[is.na(NeedCUA_ids$Exist_Flag),]
NeedCUA_ids = NeedCUA_ids[,1]

ccs_ids = unique(NeedCUA_ids)

# Save the BPR reports for easier access later
saveRDS(ccs_ids, "data/PG1_EQF_Cons_Summary__c.rds")
write(sprintf("Saved PG1_EQF_Cons_Summary__c values to tests/PG1_EQF_Cons_Summary__c.rds"), stdout())


git_info = gitInfo() # Get the git info

options(stringsAsFactors = FALSE)

Sys.setenv('repl.DB_USER' = 'Tableau', 
           'repl.DB_PASSWORD' = 'S3cretSquirrel', 
           'repl.DB_SERVER' = '10.0.0.26', 
           'repl.DB_NAME' = 'Bureau_DataAnalytics')

# Get list of Equifax Consumer credit summary IDs ever used on Credit Reviews for PG1
ccs_ids = readRDS('data/PG1_EQF_Cons_Summary__c.rds')

chunk_size = 500 # process this many at a time
n_chunks = ceiling(length(ccs_ids)/chunk_size)
chunks = lapply(1:n_chunks, function(i) (1:chunk_size) + chunk_size*(i-1))
chunks[[length(chunks)]] = chunks[[n_chunks]][chunks[[n_chunks]] <= length(ccs_ids)]


write(sprintf('---- Computing CUA for %g Equifax CCS Ids ----', length(ccs_ids)), stderr())
write(sprintf('%g Chunks, %g CCS Ids per Chunk\n', n_chunks, chunk_size), stderr())



for (i in 1:n_chunks) {
  #for (i in 1:2) {     #for testing
  chunk = chunks[[i]]
  write(sprintf('\nProcessing Chunk %g: [%g - %g]', i, chunk[1], chunk[length(chunk)]),stderr())
  CUA_df_chunk = runCCSEquifaxToCUA(list(ccs_ids = ccs_ids[chunk], environment = 'repl'))
  if(exists('CUA_df_all')) {
    CUA_df_all = rbind(CUA_df_all, CUA_df_chunk)
  } else {
    CUA_df_all = CUA_df_chunk
  }
  gc(verbose = TRUE)
}

CUA_df_PG1 = CUA_df_all

# Save it as an RDS file
filename = paste0(DATA_DIR, '/CUA_df_PG1_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(CUA_df_PG1, file = rds_file)


con <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")


for(row in seq_len(nrow(CUA_df_PG1))){
  query <- paste0(
    "INSERT INTO CUA_df_PG1      (Id,HIT,FICO,TRADES,RTRADE,MTRADE,RUTIL,PAY,RPAY,PAST,RPAST,CURB,RCURB,MCURB,CRED,RCRED,BAL,RBAL,OKTRADE,SAT,COLLS,CHOFF,
                                  JDGMNT,TXLIEN,PUBBKP,BKP_FLAG,PUBOPEN,PUBOPENBAL,BKDISCHARGE,ACTIVE,INQ3,INQ6,INQ9,INQ12,INQ15,INQ18,INQ24,OCCUR6_30,
                                  OCCUR12_30,OCCUR18_30,OCCUR24_30,OCCUR6_60,OCCUR12_60,OCCUR18_60,OCCUR24_60,OCCUR6_90,OCCUR12_90,OCCUR18_90,OCCUR24_90,
                                  OCCUR6_120,OCCUR12_120,OCCUR18_120,OCCUR24_120,CURR30,CURR60,CURR90,CURR120,OPEN3,OPEN6,OPEN9,OPEN12,OPEN24,NEW,OLD,
                                  PCT1230,PCT1260,PCT1290,git )
    VALUES (
    '", CUA_df_PG1$Id[row],"', 
    '", CUA_df_PG1$HIT[row],"',
    '", CUA_df_PG1$FICO[row],"',
    '", CUA_df_PG1$TRADES[row],"',
    '", CUA_df_PG1$RTRADE[row],"',
    '", CUA_df_PG1$MTRADE[row],"',
    '", CUA_df_PG1$RUTIL[row],"',
    '", CUA_df_PG1$PAY[row],"',
    '", CUA_df_PG1$RPAY[row],"',
    '", CUA_df_PG1$PAST[row],"',
    '", CUA_df_PG1$RPAST[row],"',
    '", CUA_df_PG1$CURB[row],"',
    '", CUA_df_PG1$RCURB[row],"',
    '", CUA_df_PG1$MCURB[row],"',
    '", CUA_df_PG1$CRED[row],"',
    '", CUA_df_PG1$RCRED[row],"',
    '", CUA_df_PG1$BAL[row],"',
    '", CUA_df_PG1$RBAL[row],"',
    '", CUA_df_PG1$OKTRADE[row],"',
    '", CUA_df_PG1$SAT[row],"',
    '", CUA_df_PG1$COLLS[row],"',
    '", CUA_df_PG1$CHOFF[row],"',
    '", CUA_df_PG1$JDGMNT[row],"',
    '", CUA_df_PG1$TXLIEN[row],"',
    '", CUA_df_PG1$PUBBKP[row],"',
    '", CUA_df_PG1$BKP_FLAG[row],"',
    '", CUA_df_PG1$PUBOPEN[row],"',
    '", CUA_df_PG1$PUBOPENBAL[row],"',
    '", CUA_df_PG1$BKDISCHARGE[row],"',
    '", CUA_df_PG1$ACTIVE[row],"',
    '", CUA_df_PG1$INQ3[row],"',
    '", CUA_df_PG1$INQ6[row],"',
    '", CUA_df_PG1$INQ9[row],"',
    '", CUA_df_PG1$INQ12[row],"',
    '", CUA_df_PG1$INQ15[row],"',
    '", CUA_df_PG1$INQ18[row],"',
    '", CUA_df_PG1$INQ24[row],"',
    '", CUA_df_PG1$OCCUR6_30[row],"',
    '", CUA_df_PG1$OCCUR12_30[row],"',
    '", CUA_df_PG1$OCCUR18_30[row],"',
    '", CUA_df_PG1$OCCUR24_30[row],"',
    '", CUA_df_PG1$OCCUR6_60[row],"',
    '", CUA_df_PG1$OCCUR12_60[row],"',
    '", CUA_df_PG1$OCCUR18_60[row],"',
    '", CUA_df_PG1$OCCUR24_60[row],"',
    '", CUA_df_PG1$OCCUR6_90[row],"',
    '", CUA_df_PG1$OCCUR12_90[row],"',
    '", CUA_df_PG1$OCCUR18_90[row],"',
    '", CUA_df_PG1$OCCUR24_90[row],"',
    '", CUA_df_PG1$OCCUR6_120[row],"',
    '", CUA_df_PG1$OCCUR12_120[row],"',
    '", CUA_df_PG1$OCCUR18_120[row],"',
    '", CUA_df_PG1$OCCUR24_120[row],"',
    '", CUA_df_PG1$CURR30[row],"',
    '", CUA_df_PG1$CURR60[row],"',
    '", CUA_df_PG1$CURR90[row],"',
    '", CUA_df_PG1$CURR120[row],"',
    '", CUA_df_PG1$OPEN3[row],"',
    '", CUA_df_PG1$OPEN6[row],"',
    '", CUA_df_PG1$OPEN9[row],"',
    '", CUA_df_PG1$OPEN12[row],"',
    '", CUA_df_PG1$OPEN24[row],"',
    '", CUA_df_PG1$NEW[row],"',
    '", CUA_df_PG1$OLD[row],"',
    '", CUA_df_PG1$PCT1230[row],"',
    '", CUA_df_PG1$PCT1260[row],"',
    '", CUA_df_PG1$PCT1290[row],"',
    '", CUA_df_PG1$git[row],"'
    )"
  )

  sqlQuery(con, query)
}

odbcClose(con)

#Update New PG2 CUA records

deploy_mode = "local"

# Connect to the database
server = '10.0.0.28'
db_name = 'Salesforce_repl'
creds <- list()

creds[['username']] = 'Tableau'
creds[['password']] = 'S3cretSquirrel'

con = getDBConnection(server, db_name, creds)


# Create a query to get Equifax CCS Ids
#CCS_query = "SELECT PG2_EQF_Cons_Summary__c FROM Credit_Review__c WHERE PG2_EQF_Cons_Summary__c IS NOT NULL AND CreatedDate > '02/10/2018' "
CCS_query = "SELECT PG2_EQF_Cons_Summary__c FROM Credit_Review__c WHERE PG2_EQF_Cons_Summary__c IS NOT NULL"
query_result = dbGetQuery(con, CCS_query)
dbDisconnect(con)

ccs_ids = unique(query_result$PG2_EQF_Cons_Summary__c)
write(sprintf("Loaded %g PG2_EQF_Cons_Summary__c values", length(ccs_ids)), stdout())

# Grab existing CUA df record IDs
con <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
ExistingIDs_query = "SELECT id as PG2_EQF_Cons_Summary__c FROM CUA_df_PG2 WHERE HIT = 1"
ExistingIDs_query_result = sqlQuery(con, ExistingIDs_query)
close(con)

ExistingIDs_query_result$Exist_Flag = 1
NeedCUA_ids = merge(x = query_result, y = ExistingIDs_query_result, by = "PG2_EQF_Cons_Summary__c", all.x = TRUE)
NeedCUA_ids = NeedCUA_ids[is.na(NeedCUA_ids$Exist_Flag),]
NeedCUA_ids = NeedCUA_ids[,1]

ccs_ids = unique(NeedCUA_ids)


# Save the BPR reports for easier access later
saveRDS(ccs_ids, "data/PG2_EQF_Cons_Summary__c.rds")
write(sprintf("Saved PG2_EQF_Cons_Summary__c values to tests/PG2_EQF_Cons_Summary__c.rds"), stdout())


git_info = gitInfo() # Get the git info

options(stringsAsFactors = FALSE)

Sys.setenv('repl.DB_USER' = 'Tableau', 
           'repl.DB_PASSWORD' = 'S3cretSquirrel', 
           'repl.DB_SERVER' = '10.0.0.26', 
           'repl.DB_NAME' = 'Bureau_DataAnalytics')

# Get list of Equifax Consumer credit summary IDs ever used on Credit Reviews for PG2
ccs_ids = readRDS('data/PG2_EQF_Cons_Summary__c.rds')

chunk_size = 500 # process this many at a time
n_chunks = ceiling(length(ccs_ids)/chunk_size)
chunks = lapply(1:n_chunks, function(i) (1:chunk_size) + chunk_size*(i-1))
chunks[[length(chunks)]] = chunks[[n_chunks]][chunks[[n_chunks]] <= length(ccs_ids)]


write(sprintf('---- Computing CUA for %g Equifax CCS Ids ----', length(ccs_ids)), stderr())
write(sprintf('%g Chunks, %g CCS Ids per Chunk\n', n_chunks, chunk_size), stderr())



for (i in 1:n_chunks) {
  #for (i in 1:2) {     #for testing
  chunk = chunks[[i]]
  write(sprintf('\nProcessing Chunk %g: [%g - %g]', i, chunk[1], chunk[length(chunk)]),stderr())
  CUA_df_chunk = runCCSEquifaxToCUA(list(ccs_ids = ccs_ids[chunk], environment = 'repl'))
  if(exists('CUA_df_all')) {
    CUA_df_all = rbind(CUA_df_all, CUA_df_chunk)
  } else {
    CUA_df_all = CUA_df_chunk
  }
  gc(verbose = TRUE)
}

CUA_df_PG2 = CUA_df_all


# Save it as an RDS file
filename = paste0(DATA_DIR, '/CUA_df_PG2_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(CUA_df_PG2, file = rds_file)


con <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")


for(row in seq_len(nrow(CUA_df_PG2))){
  query <- paste0(
    "INSERT INTO CUA_df_PG2      (Id,HIT,FICO,TRADES,RTRADE,MTRADE,RUTIL,PAY,RPAY,PAST,RPAST,CURB,RCURB,MCURB,CRED,RCRED,BAL,RBAL,OKTRADE,SAT,COLLS,CHOFF,
    JDGMNT,TXLIEN,PUBBKP,BKP_FLAG,PUBOPEN,PUBOPENBAL,BKDISCHARGE,ACTIVE,INQ3,INQ6,INQ9,INQ12,INQ15,INQ18,INQ24,OCCUR6_30,
    OCCUR12_30,OCCUR18_30,OCCUR24_30,OCCUR6_60,OCCUR12_60,OCCUR18_60,OCCUR24_60,OCCUR6_90,OCCUR12_90,OCCUR18_90,OCCUR24_90,
    OCCUR6_120,OCCUR12_120,OCCUR18_120,OCCUR24_120,CURR30,CURR60,CURR90,CURR120,OPEN3,OPEN6,OPEN9,OPEN12,OPEN24,NEW,OLD,
    PCT1230,PCT1260,PCT1290,git )
    VALUES (
    '", CUA_df_PG2$Id[row],"', 
    '", CUA_df_PG2$HIT[row],"',
    '", CUA_df_PG2$FICO[row],"',
    '", CUA_df_PG2$TRADES[row],"',
    '", CUA_df_PG2$RTRADE[row],"',
    '", CUA_df_PG2$MTRADE[row],"',
    '", CUA_df_PG2$RUTIL[row],"',
    '", CUA_df_PG2$PAY[row],"',
    '", CUA_df_PG2$RPAY[row],"',
    '", CUA_df_PG2$PAST[row],"',
    '", CUA_df_PG2$RPAST[row],"',
    '", CUA_df_PG2$CURB[row],"',
    '", CUA_df_PG2$RCURB[row],"',
    '", CUA_df_PG2$MCURB[row],"',
    '", CUA_df_PG2$CRED[row],"',
    '", CUA_df_PG2$RCRED[row],"',
    '", CUA_df_PG2$BAL[row],"',
    '", CUA_df_PG2$RBAL[row],"',
    '", CUA_df_PG2$OKTRADE[row],"',
    '", CUA_df_PG2$SAT[row],"',
    '", CUA_df_PG2$COLLS[row],"',
    '", CUA_df_PG2$CHOFF[row],"',
    '", CUA_df_PG2$JDGMNT[row],"',
    '", CUA_df_PG2$TXLIEN[row],"',
    '", CUA_df_PG2$PUBBKP[row],"',
    '", CUA_df_PG2$BKP_FLAG[row],"',
    '", CUA_df_PG2$PUBOPEN[row],"',
    '", CUA_df_PG2$PUBOPENBAL[row],"',
    '", CUA_df_PG2$BKDISCHARGE[row],"',
    '", CUA_df_PG2$ACTIVE[row],"',
    '", CUA_df_PG2$INQ3[row],"',
    '", CUA_df_PG2$INQ6[row],"',
    '", CUA_df_PG2$INQ9[row],"',
    '", CUA_df_PG2$INQ12[row],"',
    '", CUA_df_PG2$INQ15[row],"',
    '", CUA_df_PG2$INQ18[row],"',
    '", CUA_df_PG2$INQ24[row],"',
    '", CUA_df_PG2$OCCUR6_30[row],"',
    '", CUA_df_PG2$OCCUR12_30[row],"',
    '", CUA_df_PG2$OCCUR18_30[row],"',
    '", CUA_df_PG2$OCCUR24_30[row],"',
    '", CUA_df_PG2$OCCUR6_60[row],"',
    '", CUA_df_PG2$OCCUR12_60[row],"',
    '", CUA_df_PG2$OCCUR18_60[row],"',
    '", CUA_df_PG2$OCCUR24_60[row],"',
    '", CUA_df_PG2$OCCUR6_90[row],"',
    '", CUA_df_PG2$OCCUR12_90[row],"',
    '", CUA_df_PG2$OCCUR18_90[row],"',
    '", CUA_df_PG2$OCCUR24_90[row],"',
    '", CUA_df_PG2$OCCUR6_120[row],"',
    '", CUA_df_PG2$OCCUR12_120[row],"',
    '", CUA_df_PG2$OCCUR18_120[row],"',
    '", CUA_df_PG2$OCCUR24_120[row],"',
    '", CUA_df_PG2$CURR30[row],"',
    '", CUA_df_PG2$CURR60[row],"',
    '", CUA_df_PG2$CURR90[row],"',
    '", CUA_df_PG2$CURR120[row],"',
    '", CUA_df_PG2$OPEN3[row],"',
    '", CUA_df_PG2$OPEN6[row],"',
    '", CUA_df_PG2$OPEN9[row],"',
    '", CUA_df_PG2$OPEN12[row],"',
    '", CUA_df_PG2$OPEN24[row],"',
    '", CUA_df_PG2$NEW[row],"',
    '", CUA_df_PG2$OLD[row],"',
    '", CUA_df_PG2$PCT1230[row],"',
    '", CUA_df_PG2$PCT1260[row],"',
    '", CUA_df_PG2$PCT1290[row],"',
    '", CUA_df_PG2$git[row],"'
    )"
  )
  
  sqlQuery(con, query)
}

odbcClose(con)


dbhandle <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server; Server=s26; Database=Analytics_WS; Trusted_Connection=yes")
db_SP_Execute_ODBC <- sqlExecute(dbhandle, "EXEC [ks].[CUAInfoSP]")
RODBC::odbcClose(dbhandle)

write('Loading data from s26.Analytics_WS.ks....', stderr())

CUAInfo = dbQuery_ODBC('s26', 'Analytics_WS', table= '.ks.CUAInfo');
# Save it as an RDS file
filename = paste0(DATA_DIR, '/CUAInfo_', strftime(Sys.Date(), format = '%Y%m%d'))
rds_file = paste0(filename, '.rds')
saveRDS(CUAInfo, file = rds_file)







#********************************************************************************#
#***************************** Create Data Layer ********************************#
#********************************************************************************#

Data1 =  merge(x = LoanPerformance, y = CreditReviewBusinessInfo, by = "Opportunity ID", all.x = TRUE)
Data2 =  merge(x = Data1, y = BankInfoAgg, by = "Opportunity ID", all.x = TRUE)
Data3 =  merge(x = Data2, y = CUAInfo, by = "Opportunity ID", all.x = TRUE)



#************************************************************************************************#
#********************** Fix Variables Formats and Add Derived Featured **************************#
#************************************************************************************************#

Data4 = First.Fix(Data3)

Data5 = GenDerived.Features(Data4)

Data6 = computeBK(Data5)

Data7 = computeTIB(Data6)


#***********************************************************************#
#********************** Generate Model Scores **************************#
#***********************************************************************#

V2Scores = genDelMarV2Score(Data7)

V3Scores = genDelMarV3Score(Data7)











