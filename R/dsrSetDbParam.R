#' @title Set parameters in the remote databases
#' @description This function requires advanced knowledge of the database systems inolved, it should be used
#' only under supervision from a database administrator.
#' @param db_connection a character, the name of the SQLFlexClient object. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource.
#' @param param_list a list of the type (parameter_name = parameter_value) 
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @export
dsrSetDbParam<- function (db_connection, param_list, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  param_list <- sapply(param_list, function(x){
    if(!grepl('\'|"', param_list) && is.na(suppressWarnings(as.numeric(x)))){
      x <- paste0("'",x,"'")
    }
    return(x)
  }, simplify = FALSE)  # deal with quotes where necessary
  myexpr <- list(as.symbol('loadQuery'),x = as.symbol(db_connection), params = dsSwissKnifeClient:::.encode.arg(param_list))
  
  datashield.assign.expr(datasources, '.set' , as.call(myexpr), async = async)
}