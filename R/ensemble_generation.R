#' Takes a list of database experiment ID's, creates emulators from that stored data, and forms an ensemble
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param emulator_list Vector of the types of emulation model to create.
#' Accepted abbreviations are: SVM (Support-Vector Machine), GP (Gaussian
#' Process Model), NNET (Neural Network), RF (Random Forest), GLM (General
#' Linear Model)
#' @param percent_train Percent of the dataset to use as training
#' @param percent_test Percent of the dataset to use as testing
#' @param percent_validation Percent of the dataset to use as validation
#' @param normalise_set Whether data retrieved from the database should be normalised
#' @param experiment_id List of experiment IDs in the database that form the test, training, and validation sets#
#' @param network_structures List of network structures to consider when making a neural network
#' @export
generate_emulators_and_ensemble_using_db<-function(dblink, parameters, measures, emulator_list, percent_train=75, percent_test=15, percent_validation=10,
                                                   normalise_set=FALSE, experiment_id, network_structures=NULL)
{

  # Check whether there are results to process for this experiment
  experiment_results<-DBI::dbGetQuery(dblink,paste0("SELECT ",toString(parameters),",",toString(measures)," FROM spartan_analysed_results, spartan_parameters WHERE spartan_analysed_results.experiment_set_id IN (",toString(experiment_id),") AND spartan_parameters.experiment_id IN (",toString(experiment_id),") AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;"))

  if(nrow(experiment_results) > 0)
  {
    # We need to create a new experiment in the database for storing emulator generation
    new_experiment_id<-setup_experiment(dblink,"EnsembleGeneration",experiment_date=Sys.Date(), paste0("Ensemble Generation Using Experiments ",toString(experiment_id)," ",toString(Sys.time())))
    message(paste0("New Ensemble Generation Experiment in Database, ID: ",new_experiment_id," with description: Ensemble Generation Using Experiments ",toString(experiment_id)," ",toString(Sys.time())))

    # Make results numeric
    experiment_results<-sapply(experiment_results,as.numeric)

    # Now we can use the spartan methods to generate emulations and ensemble
    # Partition this data
    partitioned_data <- spartan::partition_dataset(experiment_results, parameters, measures, percent_train, percent_test,
                                         percent_validation, normalise=normalise_set, sample_mins = apply(experiment_results,2,min), sample_maxes = apply(experiment_results,2,max))

    record_partitioned_data_parameters_to_db(dblink, partitioned_data, parameters, new_experiment_id)
    record_partitioned_data_measures_to_db(dblink, partitioned_data, measures, new_experiment_id)

    algorithmSettings<-spartan::emulation_algorithm_settings(network_structures=network_structures)
    generated_ensemble<-spartan::generate_emulators_and_ensemble(emulator_list, parameters, measures, partitioned_data,
                                                      algorithm_settings = algorithmSettings, normalised=TRUE)

    message(paste0("Emulators & Ensemble Built and Saved to ",file.path(getwd(), "built_ensemble.Rda")))

    # Record performance statistics
    add_emulator_performance_stats_to_db(dblink, generated_ensemble, measures, new_experiment_id)
    add_ensemble_performance_stats_to_db(dblink, generated_ensemble, measures, new_experiment_id)

    message("Spartan Ensemble Object returned as object. This can be used to make predictions or generate an ensemble, as shown in the spartan package vignettes")

    return(generated_ensemble)
  }
}

#' Add performance statistics for the ensemble to the database
#'
#' @param dblink A link to the database in which this table is being created
#' @param generated_ensemble Object containing the ensemble and emulators built by spartan
#' @param measures Simulation output responses
#' @param new_experiment_id ID of the emulator generation experiment in the database
add_ensemble_performance_stats_to_db<-function(dblink, generated_ensemble, measures, new_experiment_id)
{
  # Now we can add the stats to the database for training performance
  stats_for_db<-NULL
  for(m in 1:length(measures))
  {
    # We store the ensembles MSE and R2 values, as well as the weights calculated for each emulator
    stats_for_db<-rbind(stats_for_db,cbind(generated_ensemble$ensemble$performance_stats[,"Technique"],
                                           rep(measures[m],nrow(generated_ensemble$ensemble$performance_stats)),
                                           generated_ensemble$ensemble$performance_stats[,paste0(measures[m],"_MSE")],
                                           generated_ensemble$ensemble$performance_stats[,paste0(measures[m],"_R2")],
                                           toString(generated_ensemble$pre_normed_mins),
                                           toString(generated_ensemble$pre_normed_maxes),
                                           toString(generated_ensemble$ensemble$weights[measures[m],]),
                                           rep(new_experiment_id,nrow(generated_ensemble$ensemble$performance_stats))))
  }

  # Colnames to match database, and add
  colnames(stats_for_db)<-c("parameter","measure","statistic_1","statistic_2","statistic_3","statistic_4","statistic_5","experiment_set_id")
  a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(stats_for_db),row.names=FALSE,name="spartan_generated_stats", append=TRUE)
  message(paste0("Ensemble Performance Statistics Added to Database, for Experiment ",new_experiment_id))
}
