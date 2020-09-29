#' @title List available tables
#' @description Retrieve the list of tables from  a resource of the type SQL Query
#' @param db_connection a character, the name of the SQL Query. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource. 
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return a data frame containing the available tables and their respective schemas
#' @export
dsrShowTables <- function (db_connection, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  # make it agnostic to table name or sql:
  myexpr <- paste0('showTables(',db_connection,')')
  datashield.aggregate(datasources, as.symbol(myexpr), async = async)
}