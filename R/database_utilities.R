#' Returns the ID of a parameter set, using the experiment ID within which
#' this parameter set was generated
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set for which ID is being sought
#' @param experiment_id Experiment ID for which this parameter set is within
#' @param row_reference For robustness analysis baseline, more than one ID could be returned. The calling function provides a row reference of which should be returned
#' @return Parameter set ID
#'
get_parameter_set_id<-function(dblink, parameter_set,experiment_id, row_reference=NULL)
{
  query_string<-"SELECT parameter_set_id FROM spartan_parameters WHERE "
  for(i in 1:length(parameter_set))
  {
    query_string <- paste(query_string,names(parameter_set[i]),"='",as.numeric(parameter_set[i]),"' AND ",sep="")
  }
  # Remove the final AND from the query string
  query_string <- paste(substr(query_string,1,nchar(query_string)-5)," AND experiment_id=",experiment_id,";",sep="")

  parameter_set_id<-RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,query_string))

  return(parameter_set_id)
  #if(nrow(parameter_set_id)==1)
  #  return(as.numeric(parameter_set_id))
  #else
  #  return(as.numeric(parameter_set_id[row_reference,]))
}

#' For a given parameter set in the database, retrieve the parameter of interest for that set in the analysis
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set for which ID is being sought
#' @param experiment_id Experiment ID for which this parameter set is within
#' @return Parameter of interest in this analysis
retrieve_parameter_of_interest<-function(dblink, parameter_set,experiment_id)
{
  query_string<-"SELECT paramOfInterest FROM spartan_parameters WHERE "
  for(i in 1:length(parameter_set))
  {
    query_string <- paste(query_string,names(parameter_set[i]),"='",as.numeric(parameter_set[i]),"' AND ",sep="")
  }
  # Remove the final AND from the query string
  query_string <- paste(substr(query_string,1,nchar(query_string)-5)," AND experiment_id=",experiment_id,";",sep="")

  param_of_interest<-RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,query_string))

  return(param_of_interest)
}

retrieve_parameter_of_interest_using_param_id<-function(dblink,parameter_set_id)
{
  return(RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,paste("SELECT paramOfInterest FROM spartan_parameters WHERE parameter_set_id=",parameter_set_id,";",sep=""))))
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
                                       "' AND experiment_date='",experiment_date,"';",sep="")))
  return(as.numeric(id))
}

#' Determines whether parameters have been added for a given experiment ID
#'
#' @param dblink A link to the database in which this table is being created
#' @param experiment_id Id of experiment for which parameter sets are being sought
#' @return Boolean showing whether parameter sets exist in database or not
check_parameter_sets_exist_for_given_experiment_id<-function(dblink, experiment_id)
{
  params<-DBI::dbGetQuery(dblink,paste("SELECT * FROM spartan_parameters WHERE experiment_id=",experiment_id,";",sep=""))
  if(nrow(params)>0)
    return(TRUE)
  else
    return(FALSE)
}
