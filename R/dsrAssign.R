#' @title Load data in the remote session(s) 
#' @description Assign data to a data frame from a resource of the type SQL Query
#' @param symbol a character, the name of the data frame
#' @param table_or_sql a character, either the name of a table in the remote database or a valid SQL query
#' @param db_connection a character, the name of the SQL Query. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource. 
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @export
dsrAssign <- function (symbol, table_or_sql, db_connection, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  # make it agnostic to table name or sql:
 sqltext <- dsSwissKnifeClient:::.encode.arg(paste0('select * from (', table_or_sql, ') xx'))
 expr <- paste0('loadQuery(', db_connection, ',"', sqltext, '")')
 datashield.assign.expr(datasources, symbol , as.symbol(myexpr))
 }