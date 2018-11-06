#' Store an ABC experiment in the simulation database
#'
#' @param dblink A link to the database in which this table is being created
#' @param abc_settings Settings object created when running the ABC analysis
#' @param abc_results Object produced by the EasyABC ABC_Sequential method, containing analysis results
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures Simulation output responses
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#' @param graph_results Whether the graphs should be plotted
#' @param output_directory If graph_results is TRUE, where the graph should be plotted to
#'
store_abc_experiment_in_db<-function(dblink, abc_settings, abc_results, parameters, measures, experiment_id=NULL, experiment_description=NULL,
                                     experiment_date = Sys.Date(), graph_results=FALSE, output_directory=NULL)
{
  tryCatch({
     # Going to store the posterior for each parameter in the parameters table. From these the density can then be calculated
    colnames(abc_resultSet$param)<-parameters
    new_experiment_id <- add_existing_lhc_sample_to_database(dblink, abc_resultSet$param, experiment_id=experiment_id, experiment_description=experiment_description,
                                        experiment_date=experiment_date, experiment_type="ABC", paramOfInterest="ABC", return_experiment_id = TRUE)

    if(new_experiment_id!=-1)
    {
      # add the predicted results for these parameters here (in the "stats" column)
      # For storing the results alongside these parameters, we need the database IDs in which these were stored
      param_ids<-DBI::dbGetQuery(dblink,paste0("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",new_experiment_id))

      # Now we can add to the database
      emulator_predictions<-cbind(abc_resultSet$stats, rep("ABC Posterior Prediction",nrow(abc_resultSet$stats)),
                                  param_ids, rep(new_experiment_id,nrow(abc_resultSet$stats)))
      # Set column name to match the database
      colnames(emulator_predictions)<-c(measures,"paramOfInterest","summarising_parameter_set_id","experiment_set_id")

      # Now we can add this set to the DB
      a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(emulator_predictions),row.names=FALSE,name="spartan_analysed_results", append=TRUE)
      message(paste0("Ensemble Predictions for ABC Generated Posterior Distribution Set added to Database under experiment id: ",new_experiment_id))

      # Worth also storing some of the settings so these could be recreated - mins and max of the range (so results can be replotted), and computation time
      algorithm_stats<-cbind("ABC","ABC",toString(abc_settings$built_ensemble$pre_normed_mins),toString(abc_settings$built_ensemble$pre_normed_maxes),abc_results$computime, new_experiment_id)
      colnames(algorithm_stats)<-c("parameter","measure","statistic_1","statistic_2","statistic_3","experiment_set_id")
      a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(algorithm_stats),row.names=FALSE,name="spartan_generated_stats", append=TRUE)
      message(paste0("ABC Results added to Database under experiment id: ",new_experiment_id))

      if(graph_results)
      {
        # Can graph these results if desired
        spartan::graph_Posteriors_All_Parameters(abc_results, parameters, abc_results$built_ensemble$pre_normed_mins, abc_results$built_ensemble$pre_normed_maxes)
        message(paste0("Posterior plots output to directory ",getwd()))
      }
    }
  }, error = function(e)
  {
    message(paste("Error in Using Ensemble for ABC Analysis.Error Message Generated: \n",e,sep=""))
    if(exists("new_experiment_id"))
      remove_errored_new_experiment_from_db(dblink, new_experiment_id)
  })
}

#' Retrieve posteriors from database and produce density plots
#' @param dblink A link to the database in which this table is being created
#' @param parameters Simulation parameters being examined
#' @param experiment_id Experiment ID for the ABC results. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this ABC experiment. May be NULL if adding by experiment ID
retrieve_abc_experiment_for_plotting<-function(dblink, parameters, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  tryCatch({
    # Check the experiment and parameters exist
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the data
      posteriors<-list("param"=sapply(DBI::dbGetQuery(dblink, paste0("SELECT ",toString(parameters)," FROM spartan_parameters WHERE experiment_id=",experiment_id,";")),as.numeric))
      # Need the ranges for the axes
      axes<-DBI::dbGetQuery(dblink, paste0("SELECT statistic_1,statistic_2 FROM spartan_generated_stats WHERE experiment_set_id=",experiment_id,";"))

      # Can graph these results if desired
      spartan::graph_Posteriors_All_Parameters(posteriors, parameters,as.numeric(unlist(strsplit(axes$statistic_1,",")))[1:length(parameters)],
                                               as.numeric(unlist(strsplit(axes$statistic_2,",")))[1:length(parameters)])
      message(paste0("Posterior plots output to directory ",getwd()))

    }
  }, error = function(e)
  {
    message(paste("Error in summarising replicate experimental results in database. Error message generated:\n",e,sep=""))
  })
}
