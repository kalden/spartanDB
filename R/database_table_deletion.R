#' Remove all tables from the database
#'
#' Note this function should be used rather than the individual remove table functions
#' as the order of deletion is set by foreign key usage
#'
#' @param dblink A link to the database in which this table is being created
#'
#' @export
delete_database_structure <- function(dblink)
{
  tryCatch( {
    out<-delete_analysed_results_table(dblink)
    out<-delete_results_table(dblink)
    out<-delete_parameters_table(dblink)
    out<-delete_experiment_table(dblink)
    message("If present, SpartanDB database structure deleted")
  }, error = function(e)
  {
    message(paste("Error in deleting database structure. Returned Error:\n",e,sep=""))
  })
}

#' Remove the experiments table from the database, if this exists
#'
#' @param dblink A link to the database in which this table is being created
delete_experiment_table <- function(dblink)
{
  DBI::dbRemoveTable(dblink,"spartan_experiment")
}

#' Remove the parameters table from the database, if this exists
#'
#' @param dblink A link to the database in which this table is being created
delete_parameters_table <- function(dblink)
{
  DBI::dbRemoveTable(dblink,"spartan_parameters")
}

#' Remove the analysed results table from the database, if this exists
#'
#' @param dblink A link to the database in which this table is being created
delete_analysed_results_table <- function(dblink)
{
  DBI::dbRemoveTable(dblink,"spartan_analysed_results")
}

#' Remove the results table from the database, if this exists
#'
#' @param dblink A link to the database in which this table is being created
delete_results_table <- function(dblink)
{
  DBI::dbRemoveTable(dblink,"spartan_results")
}
