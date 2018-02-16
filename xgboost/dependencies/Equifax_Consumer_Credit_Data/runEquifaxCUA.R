
runEquifaxCUA = function(report_raw) {
  write('', stderr())
  write('<<<<   EQUIFAX CUA STARTED    >>>>', stderr())
  
  # Create the CUA variables
  CUA = createCUAVars()
  
  # Decode the base64 input and covert to xml
  report_xml = base64Decode(report_raw)
  write(sprintf("Decoded Base64 XML report"), stderr())
  
  model = genCreditModel(report_xml)
  write(sprintf("Pre-processing of XML report completed"), stderr())
  
  CUA_result = genCUA(model, CUA)
  CUA_result$git = git_info
  write(sprintf("Successfully computed %g CUA variables", length(CUA_result)), stderr())
  
  return(CUA_result)
}