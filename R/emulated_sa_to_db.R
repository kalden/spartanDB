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
  tryCatch({
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

  }, error = function(e)
  {
    message("Problem Adding Emulated LHC results to Database.")
    remove_errored_new_experiment_from_db(dblink, new_experiment_id)
  })

}

#' Take a set of emulated lhc experiments, add to database, and run required LHC analysis
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_value_set Set of parameters used in the experiment. Expects an R object generated by spartan
#' @param emulator Emulator or ensemble object to use to generate predictions in place of original simulation
#' generated by spartan method emulate_efast_sampled_parameters
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param measure_scale The scale in which each of the outputs is measured. Used for plotting. Can be NULL if
#' no plot is required
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#' @param graph_results Whether analysis should be plotted once complete
#' @param output_directory If graph_results is TRUE, where analysis should be plotted to on the file system
#' @param normalise_sample Whether the generated sample being used to make predictions should be normalised prior to input into simulator
analyse_and_add_emulated_lhc_to_db<-function(dblink, parameter_value_set, emulator, parameters, measures, measure_scale=NULL, experiment_id=NULL, experiment_description=NULL,
                                             experiment_date = Sys.Date(), graph_results=FALSE, output_directory=NULL, normalise_sample=FALSE)
{
  tryCatch({

    # Now to run the predictions
    prccs<-spartan::emulate_lhc_sampled_parameters(output_directory, emulator, parameters, measures, measure_scale, dataset = data.frame(parameter_value_set),
                                                   normalise = normalise_sample, write_csv_files = FALSE, graph_results=graph_results)

    # Now to add all this to the database
    add_emulated_lhc_to_db(dblink,prccs$predictions[parameters], prccs$predictions[measures], prccs$prccs, experiment_description=experiment_description, experiment_id=experiment_id,
                           experiment_date=experiment_date)

  }, error = function(e)
    {
    message(paste0("Error in Analyse and Add Emulated LHC to Database. Error Message Produced: \n", e, "\n Emulated Analysis Terminated"))
  })

}

#' Take a set of emulated efast experiments, add to database, and run eFAST analysis
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_value_set Set of parameters used in the experiment. Expects an R object generated by spartan
#' @param emulator Emulator or ensemble object to use to generate predictions in place of original simulation
#' generated by spartan method emulate_efast_sampled_parameters
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#' @param graph_results Whether analysis should be plotted once complete
#' @param output_directory If graph_results is TRUE, where analysis should be plotted to on the file system
#' @param normalise_sample Whether the generated sample being used to make predictions should be normalised prior to input into simulator
#' @param normalise_result Whether the result that comes out of the emulator needs to be rescaled (using the scales held in the emulator)
analyse_and_add_emulated_efast_to_db<-function(dblink, parameter_value_set, emulator, parameters, measures, experiment_id=NULL, experiment_description=NULL,
                                               experiment_date = Sys.Date(), graph_results=FALSE, output_directory=NULL, normalise_sample=FALSE, normalise_result=FALSE)
{

  tryCatch({

    # Check that the Dummy has been declared in the parameters
    if(!"Dummy" %in% parameters & !"dummy" %in% parameters)
      parameters<-c(parameters,"Dummy")
    else if("dummy" %in% parameters) # Just make it upper case to match rest of code
      parameters[match("dummy",parameters)]<-"Dummy"

    # Firstly run the emulator to make the predictions - can get number of curves from the dimensions of the parameter set
    emulator_predictions<-spartan::emulate_efast_sampled_parameters(NULL, emulator, parameters, measures, dim(parameter_value_set)[4], normalise = normalise_sample, csv_file_input=FALSE,
                                                        spartan_sample_obj=parameter_value_set, write_csv_file_out=FALSE, normalise_result=normalise_result)

    # Add Parameter set to DB - can get number of curves from the length of the emulation predictions
    new_experiment_id<-add_existing_efast_sample_to_database(dblink, parameters, length(emulator_predictions), parameters_r_object=parameter_value_set, experiment_id=experiment_id,
                                                             experiment_description=experiment_description, experiment_date=experiment_date, return_experiment_id=TRUE)

    # If returned as -1 an error has occurred, or experiment may already exist in database. Error message will have been printed
    if(new_experiment_id != -1)
    {
      # Add the predictions
      # This is a bit more tricky for eFAST, as what we have are curve summary files, that will need to be post processed
      # Get the parameter ID;s (again use length or predictions to get number of curves)
      for(c in 1:length(emulator_predictions))
      {
        for(p in 1:length(parameters))
        {
          # Extract the results for this parameter
          param_results<-emulator_predictions[[c]][,paste0(parameters[p],"_",measures)]
          # Get the param IDs
          param_ids<-DBI::dbGetQuery(dblink, paste0("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",new_experiment_id," AND paramOfInterest='",parameters[p],"' AND curve=",c,";"))
          # Now make up the results
          results_to_add <- cbind(param_results,rep(parameters[p],nrow(param_results)),rep(c,nrow(param_results)),param_ids,rep(new_experiment_id,nrow(param_results)))
          colnames(results_to_add)<-c(measures,"paramOfInterest","curve","summarising_parameter_set_id","experiment_set_id")
          a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(results_to_add),row.names=FALSE,name="spartan_analysed_results", append=TRUE)

          message(paste0("Emulated eFAST Data Added for Curve ",c," Parameter ",parameters[p]," with Experiment ID ",new_experiment_id))
        }
      }

      # Now to run the analysis and add the stats
     generate_efast_analysis(dblink, parameters, measures, experiment_id=new_experiment_id, graph_results=graph_results, output_directory=output_directory)
    }
  }, error = function(e)
  {
    message(paste0("Error in Emulating eFAST Analysis. R Error Produced: \n",e,"\n Analysis Terminated"))
    if(exists("new_experiment_id"))
    {
      remove_errored_new_experiment_from_db(dblink, new_experiment_id)
    }
  })
}
