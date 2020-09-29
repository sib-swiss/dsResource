#' @title Describe a table 
#' @description List the columns and data types of a table from a resource of the type SQL Query
#' @param db_connection a character, the name of the SQL Query. This object has to exist already in the remote
#' session(s) and is created with the function datashield.assign.resource
#' @param schema_name a caracter, the name of the schema in which the table resides.
#' @param table_name a character, the name of the table
#' @param schema_name a caracter, the name of the schema in which the table resides.
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return a data frame containing the column names and data types
#' @export
dsrShowColumns <- function (db_connection, table_name, schema_name = 'public', async = TRUE, datasources = NULL){
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  # make it agnostic to table name or sql:
  myexpr <- paste0('showColumns(db,"', table_name, '","', schema_name, '")')
  datashield.aggregate(datasources, as.symbol(myexpr), async = async)
}