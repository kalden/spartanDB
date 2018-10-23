#' Takes an emulated LHC experiment and adds the parameter sets and results to the database
#' @param dblink A link to the database in which this table is being created
#' @param parameter_value_set Set of parameters used in the experiment
#' @param emulation_predictions Emulation predictions for this set of parameters
#' @param prcc_vals Calculated PRCC values for this experiment
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
add_emulated_lhc_to_db<-function(dblink,parameter_value_set, emulation_predictions, prcc_vals, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  # Add Parameter set to DB
  new_experiment_id<-add_existing_lhc_sample_to_database(dblink, parameter_value_set, experiment_id, experiment_description, experiment_date, return_experiment_id=TRUE)
  # Add predictions
  param_ids<-DBI::dbGetQuery(dblink,paste0("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",new_experiment_id))
  results_to_add <- cbind(emulation_predictions,param_ids,rep(new_experiment_id,nrow(emulation_predictions)))
  colnames(results_to_add)<-c(colnames(emulation_predictions),"summarising_parameter_set_id","experiment_set_id")
  a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(results_to_add),row.names=FALSE,name="spartan_analysed_results", append=TRUE)

  # Add stats
  add_prcc_values_to_db(dblink, colnames(parameter_value_set), colnames(emulation_predictions), prcc_vals, new_experiment_id)

  message(paste0("Emulated LHC Data Added to Database with Experiment ID ",new_experiment_id))

}
