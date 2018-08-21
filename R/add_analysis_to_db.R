#' Adds replicate runs to the database for a given experiment ID
#'
#' Takes a set of results and adds these to the database, ensuring
#' these are linked to the correct parameter ID and experiment ID. As
#' spartan outputs results in groups of the same parameter values, this
#' method takes advantage of this by collecting results into one set
#' before adding these to the database, to ensure better performance
#' and less connections required to the database.
#'
#'@param dblink A link to the database in which this table is being created
#'@param parameters The parameters of the simulation that are being analysed
#'@param measures The measures of the simulation that are being assessed
#'@param all_results Set of results to add to the database
#'@param experiment_id ID of this experiment in the database
add_replicate_runs_to_database<-function(dblink, parameters, measures, all_results, experiment_id)
{
  block_to_add_to_database<-NULL

  # Work on the top row initially, then we can do all the others separately
  # This ensures we don't have to query the database for a parameter set ID if the parameters are the same
  current_parameter_set<-all_results[1,1:length(parameters)]
  parameter_set_id<-get_parameter_set_id(current_parameter_set,experiment_id)

  f<-cbind(t(as.numeric(all_results[1,measures])),parameter_set_id,experiment_id)
  block_to_add_to_database<-rbind(block_to_add_to_database,f)

  #colnames(f)<-c(measures,"parameter_set_id","experiment_set_id")
  #dbWriteTable(dblink, value = as.data.frame(f),row.names=FALSE,name="spartan_results", append=TRUE)

  for(result in 2:nrow(all_results))
  {
    # Get the parameters
    parameter_values = all_results[result,1:length(parameters)]
    # See if we need to get the ID for this parameter set from the database, or whether it is equal to the last set
    if(!all(current_parameter_set==parameter_values))
    {
      # We need to commit the results seen so far for the last parameter set to the database
      colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id")
      RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_database),row.names=FALSE,name="spartan_results", append=TRUE)
      # Reset the block to empty
      block_to_add_to_database<-NULL

      # Now We need to query the value for the next set of parameters
      current_parameter_set = parameter_values
      parameter_set_id<-get_parameter_set_id(current_parameter_set,experiment_id)
       }

    # Add the result to the block to be written to the database
    f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id)
    block_to_add_to_database<-rbind(block_to_add_to_database,f)
  }
}
