#' @title Describe a table 
#' @description List the columns and data types of a table from a resource of the type SQL Query
#' @param db_connection a character, the name of the SQLFlexClient object. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource
#' @param table_name a character, the name of the table (or view) as shown in the output of dsrShowTables(qualified with the schema name if the latter is not 'public', ex: 'information_schema.views')
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return a data frame containing the column names and data types
#' @export
dsrShowColumns <- function (table_name, db_connection, async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  # make it agnostic to table name or sql:
  myexpr <- paste0('showColumns(',db_connection,',"', table_name, '")')
  datashield.aggregate(datasources, as.symbol(myexpr), async = async)
}