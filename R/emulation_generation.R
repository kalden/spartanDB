#' Takes a list of database experiment ID's  and creates emulators from that stored data
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
#' @param experiment_id List of experiment IDs in the database that form the test, training, and validation sets
#' @export
#'
create_emulators_from_database_experiments<-function(dblink, parameters, measures, emulator_list, percent_train=75, percent_test=15, percent_validation=10,
                                                     normalise_set=FALSE, experiment_id)
{

  experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description=NULL, experiment_date=NULL)

  # if experiment ID is ok:
  if(experiment_id != -1)
  {
    # Check whether there are results to process for this experiment
    experiment_results<-DBI::dbGetQuery(dblink,paste0("SELECT ",toString(parameters),",",toString(measures)," FROM spartan_analysed_results, spartan_parameters WHERE spartan_analysed_results.experiment_set_id IN (",toString(experiment_id),") AND spartan_parameters.experiment_id IN (",toString(experiment_id),") AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;"))

    if(nrow(experiment_results) > 0)
    {
      # We need to create a new experiment in the database for stoing emulator generation
      new_experiment_id<-setup_experiment(dblink,"EmulatorGeneration",experiment_date=Sys.Date(), paste0("Emulator Generation Using Experiments ",toString(experiment_id)," ",toString(Sys.time())))
      message(paste0("New Emulator Generation Experiment in Database, ID: ",new_experiment_id," with description: Emulator Generation Using Experiments ",toString(experiment_id)," ",toString(Sys.time())))

      # Make results numeric
      experiment_results<-sapply(experiment_results,as.numeric)

      # Now we can use the spartan methods to generate emulations
      # Partition this data
      partitioned_data <- spartan::partition_dataset(experiment_results, parameters, measures, percent_train, percent_test,
                                         percent_validation, normalise=normalise_set, sample_mins = apply(experiment_results,2,min), sample_maxes = apply(experiment_results,2,max))

      record_partitioned_data_parameters_to_db(dblink, partitioned_data, parameters, new_experiment_id)
      record_partitioned_data_measures_to_db(dblink, partitioned_data, measures, new_experiment_id)

      # Now we can build the emulators using this data
      built_emulators<-build_emulators_from_db_data(emulator_list, partitioned_data, parameters, measures, normalise_set)

      add_emulator_performance_stats_to_db(dblink, built_emulators, measures, new_experiment_id)

      message("Spartan Emulator Object returned as object. This can be used to make predictions or generate an ensemble, as shown in the spartan package vignettes")

      return(built_emulators)
    }
  }

}

#' Takes the partitioned parameter values data and stores this in the database, so emulation creation can be replicated if necessary
#'
#' In doing this, whether the row of the data is training, testing, or validation is stored in the paramOfInterest column
#'
#' @param dblink A link to the database in which this table is being created
#' @param partitioned_data Partitioned dataset being used in emulator generation, to add to DB
#' @param parameters Simulation parameters being examined
#' @param new_experiment_id ID for this experiment in the database
#'
record_partitioned_data_parameters_to_db <- function(dblink, partitioned_data, parameters, new_experiment_id)
{
  # So we can replicate this again in the future, we store this split in the database, using the paramOfInterest column to denote whether this is training, testing, or validation
  # Note the rounding we have used throughout spartanDB, to ensure we can retrieve by parameter value later
  training_db_set<-cbind(round(partitioned_data$training[parameters],digits=12),rep("Training",nrow(partitioned_data$training)))
  colnames(training_db_set)<-c(parameters,"paramOfInterest")
  testing_db_set<-cbind(round(partitioned_data$testing[parameters],digits=12),rep("Testing",nrow(partitioned_data$testing)))
  colnames(testing_db_set)<-c(parameters,"paramOfInterest")
  validation_db_set<-cbind(round(partitioned_data$validation[parameters],digits=12),rep("Validation",nrow(partitioned_data$validation)))
  colnames(validation_db_set)<-c(parameters,"paramOfInterest")

  success <- add_parameter_set_to_database(dblink, rbind(training_db_set,testing_db_set, validation_db_set), new_experiment_id, experiment_type="LHC")

  if(!success)
    stop("Error in Adding Parameter Values for Emulation Training and Test Sets to Database")
  else
    message(paste("Partitioned Parameter Value Sets Added to Database, with Experiment ID ",new_experiment_id,sep=""))
}

#' Takes the partitioned output measures values data and stores this in the database, so emulation creation can be replicated if necessary
#'
#' In doing this, whether the row of the data is training, testing, or validation is stored in the paramOfInterest column
#'
#' @param dblink A link to the database in which this table is being created
#' @param partitioned_data Partitioned dataset being used in emulator generation, to add to DB
#' @param measures Simulation output responses
#' @param new_experiment_id ID for this experiment in the database
#'
record_partitioned_data_measures_to_db <- function(dblink, partitioned_data, measures, new_experiment_id)
{
  # We need to add the results for that set too, in the analysed data table
  training_results_set<-cbind(round(partitioned_data$training[measures],digits=12),rep("Training",nrow(partitioned_data$training)))
  colnames(training_results_set)<-c(measures,"paramOfInterest")
  testing_results_set<-cbind(round(partitioned_data$testing[measures],digits=12),rep("Testing",nrow(partitioned_data$testing)))
  colnames(testing_results_set)<-c(measures,"paramOfInterest")
  validation_results_set<-cbind(round(partitioned_data$validation[measures],digits=12),rep("Validation",nrow(partitioned_data$validation)))
  colnames(validation_results_set)<-c(measures,"paramOfInterest")
  set_results<-rbind(training_results_set,testing_results_set,validation_results_set)

  # Get the parameter IDs from the database that match the parameters table
  param_ids<-DBI::dbGetQuery(dblink,paste("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",new_experiment_id,";",sep=""))

  block_to_add_to_db<-cbind(set_results,param_ids$parameter_set_id,rep(new_experiment_id,nrow(set_results)))

  # Now we can add these summarys to the analysed results table
  colnames(block_to_add_to_db)<-c(measures,"paramOfInterest","summarising_parameter_set_id","experiment_set_id")
  success<-RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_db[stats::complete.cases(block_to_add_to_db), ]),row.names=FALSE,name="spartan_analysed_results", append=TRUE)

  if(!success)
    stop("Error in Adding Responses for Emulation Training and Test Sets to Database")
  else
    message(paste("Partitioned Simulation Responses Set Added to Database, with Experiment ID ",new_experiment_id,sep=""))
}

#' Takes a partitioned data set and uses spartan to generate the requested emulations from that data
#'
#' @param parameters The parameters of the simulation that are being analysed
#' @param partitioned_data Partitioned data object returned from spartan
#' @param measures The measures of the simulation that are being assessed
#' @param emulator_list Vector of the types of emulation model to create.
#' Accepted abbreviations are: SVM (Support-Vector Machine), GP (Gaussian
#' Process Model), NNET (Neural Network), RF (Random Forest), GLM (General
#' Linear Model)
#' @param normalise_set Whether the data has been normalised
#' @return Object containing the built emulators
build_emulators_from_db_data<-function(emulator_list, partitioned_data, parameters, measures, normalise_set)
{
  # Now we can build the emulators using this data
  built_emulators <- spartan::generate_requested_emulations(emulator_list, partitioned_data, parameters, measures, normalised=normalise_set)
  message(paste0("Emulators Built and Saved to ",file.path(getwd(), paste0("Built_Emulation_",paste(emulator_list, collapse = "_"),".Rda"))))
  return(built_emulators)
}

#' Adds emulator performance stats to the database for retrieval later
#'
#' @param dblink A link to the database in which this table is being created
#' @param built_emulators Object containing the emulators built by spartan
#' @param measures Simulation output responses
#' @param new_experiment_id ID of the emulator generation experiment in the database
add_emulator_performance_stats_to_db<-function(dblink, built_emulators, measures, new_experiment_id)
{
  # Now we can add the stats to the database for training performance
  stats_for_db<-NULL
  for(m in 1:length(measures))
  {
    stats_for_db<-rbind(stats_for_db,cbind(built_emulators$statistics[,"Technique"],
                                           rep(measures[m],nrow(built_emulators$statistics)),
                                           built_emulators$statistics[,paste0(measures[m],"_MSE")],
                                           built_emulators$statistics[,paste0(measures[m],"_R2")],
                                           toString(built_emulators$pre_normed_mins),
                                           toString(built_emulators$pre_normed_maxes),
                                           rep(new_experiment_id,nrow(built_emulators$statistics))))
  }

  # Colnames to match database, and add
  colnames(stats_for_db)<-c("parameter","measure","statistic_1","statistic_2","statistic_3","statistic_4","experiment_set_id")
  a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(stats_for_db),row.names=FALSE,name="spartan_generated_stats", append=TRUE)
  message(paste0("Emulator Performance Statistics Added to Database, for Experiment ",new_experiment_id))
}

#' Takes a previous emulator generation experiment stored in the database and regenerates the emulators. Ensures emulators can be regenerated from the same data
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param emulator_list Vector of the types of emulation model to create.
#' Accepted abbreviations are: SVM (Support-Vector Machine), GP (Gaussian
#' Process Model), NNET (Neural Network), RF (Random Forest), GLM (General
#' Linear Model)
#' @param normalise_set Whether data retrieved from the database should be normalised
#' @param experiment_id List of experiment IDs in the database that form the test, training, and validation sets
#' @param experiment_description If not searching for previous result by ID, can use experiment description and date instead
#' @param experiment_date If not searching for previous result by ID, can use experiment description and date instead
#' @export
regenerate_emulators_from_db_data<-function(dblink, parameters, measures, emulator_list, normalise_set=FALSE, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL)
{
  experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

  # if experiment ID is ok:
  if(experiment_id != -1)
  {
    # We can recover the partitioned data from the database:
    experiment_results<-DBI::dbGetQuery(dblink,paste0("SELECT ",toString(parameters),",",toString(measures),",spartan_parameters.paramOfInterest FROM spartan_analysed_results, spartan_parameters WHERE spartan_analysed_results.experiment_set_id IN (",toString(experiment_id),") AND spartan_parameters.experiment_id IN (",toString(experiment_id),") AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;"))

    partitioned_data<-list("training"=sapply(subset(experiment_results,experiment_results$paramOfInterest=="Training",select=c(parameters,measures)),as.numeric),
                           "testing"=sapply(subset(experiment_results,experiment_results$paramOfInterest=="Testing",select=c(parameters,measures)),as.numeric),
                           "validation"=sapply(subset(experiment_results,experiment_results$paramOfInterest=="Validation",select=c(parameters,measures)),as.numeric),
                           "parameters"=parameters, "measures"=measures,"pre_normed_mins"=NULL,"pre_normed_maxes"=NULL)

    # Make results numeric
    experiment_results<-sapply(experiment_results[c(parameters,measures)],as.numeric)

    # Need to add the mins and maxes for the range to this too, which is in the stats part of the database
    mins_maxes<-DBI::dbGetQuery(dblink,paste0("SELECT DISTINCT statistic_3, statistic_4 FROM spartan_generated_stats WHERE experiment_set_id=",toString(experiment_id),";"))

    partitioned_data$pre_normed_mins<-utils::read.table(text=mins_maxes$statistic_3,sep=",",colClasses="numeric",col.names = c(parameters,measures))
    partitioned_data$pre_normed_maxes<-utils::read.table(text=mins_maxes$statistic_4,sep=",",colClasses="numeric",col.names = c(parameters,measures))

    # Now we have the data set for building the emulations
    built_emulators<-build_emulators_from_db_data(emulator_list, partitioned_data, parameters, measures, normalise_set)
    message("Spartan Emulator Object returned as object. This can be used to make predictions or generate an ensemble, as shown in the spartan package vignettes")

    return(built_emulators)
  }
}

#' To demonstrate use of emulator, we return validation set from the database to show how the emulator can make predictions on output
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id ID of the experiment for which the validation set is being returned
#' @return Set of parameter values that can be used to make predictions
#'
retrieve_validation_set_from_db_for_emulator<-function(dblink, parameters, measures, experiment_id)
{
  validation_results<-sapply(DBI::dbGetQuery(dblink, paste0("SELECT ",toString(parameters)," FROM spartan_parameters WHERE experiment_id=",experiment_id," AND paramOfInterest='Validation';")),as.numeric)
  if(nrow(validation_results)>0)
  {
    return(validation_results)
  }
  else
  {
    message("No validation set contained in the database for experiment ID ",experiment_id)
    stop()
  }
}

#' Takes a set of parameters and uses a set of emulators to generate predictions of simulator output, then adds these to the database
#'
#' @param dblink A link to the database in which this table is being created
#' @param sim_emulators Emulation object created by spartan
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param data_to_predict Sets of parameters for which simulation output measures should be predicted
#' @param normalise Whether the data_to_predict should be normalised prior to making predictions. Defaults to FALSE
#' @param normalise_result Whether the predictions generated should be normalised using the scale set in the emulator. Defaults to FALSE
#' @param experiment_description Description of this experiment, to be added to the database
#' @param experiment_date Date this experiment was performed. Defaults to that day's date
#' @export
use_emulators_to_make_and_store_predictions<-function(dblink, sim_emulators, parameters, measures, data_to_predict, normalise=FALSE, normalise_result=FALSE, experiment_description, experiment_date=Sys.Date())
{
  tryCatch({
    # Set up an experiment in the database for storing this data
    experiment_id<-setup_experiment(dblink,"Emulator Predictions",experiment_date, experiment_description)
    message(paste0("New Emulator Prediction Experiment Added to Database with Experiment ID ",experiment_id))

    # Generate predictions
    predictions <- spartan::emulator_predictions(sim_emulators, parameters, measures,  data.frame(data_to_predict), normalise, normalise_result)

    # Firstly, store the parameter set used in this experiment in the database
    params_to_add <- cbind(data_to_predict,rep(experiment_id,nrow(data_to_predict)))
    colnames(params_to_add)<-c(colnames(data_to_predict),"experiment_id")
    params<-RMySQL::dbWriteTable(dblink, value = as.data.frame(params_to_add),row.names=FALSE,name="spartan_parameters", append=TRUE)

    # For storing the results alongside these parameters, we need the database IDs in which these were stored
    param_ids<-DBI::dbGetQuery(dblink,paste0("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",experiment_id))

    # Now we can process the data and add to the database


    predictions_frame_to_add_to_db<-NULL

    # If we have more than one emulator in the sim_emulators object, we will need to store the predictions made by each emulator
    for(i in 1:length(sim_emulators$emulators))
    {
      # Retrieve the predictions for this emulator, bind with the param_ids seen earlier, experiment ID, and use paramOfInterest column to store emulator type
      emulator_predictions<-cbind(predictions[,paste0(sim_emulators$emulators[[i]]$type,"_",measures)], rep(sim_emulators$emulators[[i]]$type,nrow(predictions)),
                                  param_ids,rep(experiment_id,nrow(predictions)))
      # Set column name to match the measures
      colnames(emulator_predictions)<-c(measures,"paramOfInterest","summarising_parameter_set_id","experiment_set_id")
      predictions_frame_to_add_to_db<-rbind(predictions_frame_to_add_to_db,emulator_predictions)
    }

    # Now we can add this set to the DB
    a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(predictions_frame_to_add_to_db),row.names=FALSE,name="spartan_analysed_results", append=TRUE)
    message("Experiment Data Added to Database")
  }, error = function(e)
  {
    message(paste("Error in Using Emulators to Generate Predictions and Add that Experiment to the Database.Error Message Generated: \n",e,sep=""))
    remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}
