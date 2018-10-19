create_emulators_from_database_experiments<-function(dblink, experiment_id, experiment_description=NULL, experiment_date=NULL)
{
  experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

  # if experiment ID is ok:
  if(experiment_id != -1)
  {
    # Check whether there are results to process for this experiment
    experiment_results<-DBI::dbGetQuery(dblink,paste0("SELECT ",toString(parameters),",",toString(measures)," FROM spartan_analysed_results, spartan_parameters WHERE spartan_analysed_results.experiment_set_id IN (",toString(experiment_id),") AND spartan_parameters.experiment_id IN (",toString(experiment_id),") AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id=",experiment_id,";"))

    if(num_results > 0)
    {

    }
  }

}
