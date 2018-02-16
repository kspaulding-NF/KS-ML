

createCUAVars = function(asof_date = NULL) {
  ## List of standard variable create functions that just take experian XML as input
  CUA_funcs = list(
    "HIT" = HIT.EQF,
    "FICO" = FICO.EQF,
    "TRADES" = TRADES.EQF,
    "RTRADE" = RTRADE.EQF,
    "MTRADE" = MTRADE.EQF,
    "RUTIL" = RUTIL.EQF,
    "PAY" = PAY.EQF,
    "RPAY" = RPAY.EQF,
    "PAST" = PAST.EQF,
    "RPAST" = RPAST.EQF,
    "CURB" = CURB.EQF,
    "RCURB" = RCURB.EQF,
    "MCURB" = MCURB.EQF,
    "CRED" = CRED.EQF,
    "RCRED" = RCRED.EQF,
    "BAL" = BAL.EQF,
    "RBAL" = RBAL.EQF,
    "OKTRADE" = OKTRADE.EQF,
    "SAT" = SAT.EQF,
    "COLLS" = COLLS.EQF,
    "CHOFF" = CHOFF.EQF,
    "JDGMNT" = JDGMNT.EQF,
    "TXLIEN" = TXLIEN.EQF,
    "PUBBKP" = PUBBKP.EQF,
    "BKP_FLAG" = BKP_FLAG.EQF,
    "PUBOPEN" = PUBOPEN.EQF,
    "PUBOPENBAL" = PUBOPENBAL.EQF,
    "BKDISCHARGE" = BKDISCHARGE.EQF,
    "ACTIVE" = ACTIVE.EQF
    
  )
  
  ## Factory methods
  # Inquires
  INQ_horizons = c(3,6,9,12,15,18,24)
  INQ_funcs = lapply(INQ_horizons, function(T) INQ.EQF.FACTORY(T, asof_date))
  INQ_names = sapply(INQ_horizons, function(T) paste0("INQ", T))
  names(INQ_funcs) = INQ_names
  CUA_funcs = append(CUA_funcs, INQ_funcs)
  
  # Past due occurances
  OCCUR_horizons = c(6, 12, 18, 24)
  OCCUR_D = c(30,60,90,120);
  OCCUR_funcs = lapply(OCCUR_D, 
                       function(D) {
                         lapply(OCCUR_horizons, 
                                function(T) {
                                  OCCUR.EQF.FACTORY(T, D, asof_date)
                                }
                         )
                       }
  )
  OCCUR_funcs = unlist(OCCUR_funcs)
  OCCUR_names = sapply(OCCUR_D, 
                       function(D) {
                         sapply(OCCUR_horizons, 
                                function(T) {
                                  paste0("OCCUR", T, "_", D)
                                }
                         )
                       }
  )
  OCCUR_names = as.vector(OCCUR_names)
  names(OCCUR_funcs) = OCCUR_names
  CUA_funcs = append(CUA_funcs, OCCUR_funcs)
  
  # Number of Trades Currently D Days, for D = {30, 60, 90, 120}
  CURR_D = c(30,60,90,120);
  CURR_funcs = lapply(CURR_D, 
                      function(D) {
                        CURR.EQF.FACTORY(D, asof_date)
                      }
  )
  CURR_funcs = unlist(CURR_funcs)
  CURR_names = sapply(CURR_D, 
                      function(D) {
                        paste0("CURR", D)
                      }
  )
  CURR_names = as.vector(CURR_names)
  names(CURR_funcs) = CURR_names
  CUA_funcs = append(CUA_funcs, CURR_funcs)
  
  
  # Number of Trades Opened in Last T months, for T = {3,6,9,12,24}
  OPEN_horizons = c(3,6,9,12,24)
  OPEN_funcs = lapply(OPEN_horizons, function(T) OPEN.EQF.FACTORY(T, asof_date))
  OPEN_names = sapply(OPEN_horizons, function(T) paste0("OPEN", T))
  names(OPEN_funcs) = OPEN_names
  CUA_funcs = append(CUA_funcs, OPEN_funcs)
  
  # Age of newest trade in months
  NEW_func = list("NEW" = NEW.EQF.FACTORY(asof_date))
  CUA_funcs = append(CUA_funcs, NEW_func)
  
  # Age of oldest
  OLD_func = list("OLD" = OLD.EQF.FACTORY(asof_date))
  CUA_funcs = append(CUA_funcs, OLD_func)
  
  # Percent of trades with a {DD, ..., 120} occurance in the past 12 months
  DDs = c(30, 60, 90)
  PCT12_funcs = lapply(DDs, function(d) PCT12.EQF.FACTORY(d, asof_date))
  PCT12_names = sapply(DDs, function(d) paste0("PCT12", d))
  names(PCT12_funcs) = PCT12_names;
  CUA_funcs = append(CUA_funcs, PCT12_funcs)
  
  return(CUA_funcs)
}


genCUA = function(xpn, CUA) {
  return(lapply(CUA, function(c) c(xpn)))
}
