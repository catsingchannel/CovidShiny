obtainids <- function(country = 'global', lineage = 'all', clade = 'all', date){
  ids_list <- list('%%', '%%', '%%', '%%', date[1], date[2])
  ids_stat <- dbSendQuery(dbcon, 'SELECT ID FROM meta WHERE region LIKE ? AND country LIKE ? AND lineage LIKE ? AND Gclade LIKE ? AND time BETWEEN ? AND ?')
  
  if(country != 'global'){
    if(country %in% countrylist[1:6]){
      ids_list[[1]] <- paste0("%", country, "%")
    } else {
      ids_list[[2]] <- paste0("%", country, "%")
    }
  }
  
  if(lineage != 'all'){
    ids_list[[3]] <- paste0("%", lineage, "%")
  }
  
  if(clade != 'all'){
    ids_list[[4]] <- paste0("%", clade, "%")
  }
  
  dbBind(ids_stat, ids_list)
  ret <- list(ids = dbFetch(ids_stat)$ID)
  dbClearResult(ids_stat)
  
  ret
}