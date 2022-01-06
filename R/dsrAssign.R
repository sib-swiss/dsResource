#' @title Load data in the remote session(s) 
#' @description Assign data to a data frame from a resource of the type SQL Query
#' @param symbol a character, the name of the data frame
#' @param table a character, the name of a table in the remote database
#' @param collist a vector , names of the columns to load. If NULL (the default) all the columns will be loaded.
#' @param where_clause a character, an optional where clause to the sql statement that will load the table. It must not begin with "where".
#' @param db_connection a character, the name of the SQLFlexClient object. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource. 
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @export
dsrAssign <- function (symbol, table, collist = NULL, where_clause = NULL, db_connection, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
 
 myexpr <- list(as.symbol('loadQuery'), table, dsSwissKnifeClient:::.encode.arg(collist), dsSwissKnifeClient:::.encode.arg(where_clause))
 
 datashield.assign.expr(datasources, symbol , as.call(myexpr), async = async)
}