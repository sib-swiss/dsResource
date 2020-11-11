#' @title Virtual object size
#' @description Estimate the size in memory of views or query result sets
#' @param virtual_objects a character vector containing either database view names or valid queries
#' @param db_connection a character, the name of the SQLFlexClient object . This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource
#' @param async same as in datashield.assign
#' @param datasources same as in datashi eld.assign
#' @return a data frame containing the column names and data types
#' @export
dsrEstimateObjectSize <- function (virtual_objects, db_connection, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  virtual_objects <- dsSwissKnifeClient:::.encode.arg(virtual_objects)
  # make it agnostic to table name or sql:
  myexpr <- paste0('viewSize(',db_connection,',"',virtual_objects, '")')
  datashield.aggregate(datasources, as.symbol(myexpr), async = async)
}