#' Returns the ID of a parameter set, using the experiment ID within which
#' this parameter set was generated
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set for which ID is being sought
#' @param experiment_id Experiment ID for which this parameter set is within
#' @return Parameter set ID
#'
get_parameter_set_id<-function(dblink, parameter_set,experiment_id)
{
  query_string<-"SELECT parameter_set_id FROM spartan_parameters WHERE "
  for(i in 1:length(parameter_set))
  {
    query_string <- paste(query_string,names(parameter_set[i]),"='",as.numeric(parameter_set[i]),"' AND ",sep="")
  }
  # Remove the final AND from the query string
  query_string <- paste(substr(query_string,1,nchar(query_string)-5)," AND experiment_id=",experiment_id,";",sep="")

  parameter_set_id<-RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,query_string))
  return(as.numeric(parameter_set_id))
}

#' Return the experiment ID for a given experiment type, date, and description
#'
#' @param dblink A link to the database in which this table is being created
#' @param experiment_type Whether the experiment was a consistency, robustness, LHC, or eFAST analysis
#' @param experiment_date Date the experiment was performed
#' @param experiment_description Description of the experiment being performed
#' @return Integer value for the ID of this experiment in the database
get_experiment_id <- function(dblink,experiment_type,experiment_date, experiment_description)
{
  id<-RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,paste("SELECT experiment_id FROM spartan_experiment WHERE experiment_type='",experiment_type,"' AND experiment_description='",experiment_description,
                                       "' AND experiment_date='",Sys.Date(),"';",sep="")))
  return(as.numeric(id))
}
