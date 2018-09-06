#' Using results stored in the database, calculates partial rank correlation coefficients for
#' all measure-parameter pairs in an LHC experiment
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters Simulation parameters being examined
#' @param measures Simulation output responses
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
generate_lhc_analysis<-function(dblink, parameters, measures, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL)
{
  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the analysed results for this experiment
      results<-DBI::dbGetQuery(dblink,paste("SELECT ",toString(parameters),",",toString(measures)," FROM spartan_parameters,spartan_analysed_results WHERE spartan_analysed_results.experiment_set_id=",
                            experiment_id," AND spartan_parameters.experiment_id=",experiment_id,
                            " AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;", sep=""))

      if(nrow(results > 0))
      {
        coeffs<-spartan::lhc_generatePRCoEffs_db_link(results, parameters, measures)

        block_to_add_to_db<-matrix(nrow=length(parameters)*length(measures),ncol=5)
        row_ref<-1

        # Now to put this in the DB. In this case statistic_1 is PRCC, statistic_2 is P-Value
        for(r in 1:nrow(coeffs))
        {
          col_offset<-0

          for(m in 1:length(measures))
          {
            block_to_add_to_db[row_ref,1]<-row.names(coeffs)[r]
            # Need to get the measure name - assuming in same order as provided measures
            block_to_add_to_db[row_ref,2]<-measures[m]
            block_to_add_to_db[row_ref,3]<-coeffs[r,(col_offset+1)]
            block_to_add_to_db[row_ref,4]<-coeffs[r,(col_offset+2)]
            block_to_add_to_db[row_ref,5]<-experiment_id
            row_ref<-row_ref+1
            col_offset<-col_offset+2
          }
        }

        # Write this set to the DB
        colnames(block_to_add_to_db)<-c("parameter","measure","statistic_1","statistic_2","experiment_set_id")
        a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_db),row.names=FALSE,name="spartan_generated_stats", append=TRUE)
      }
      else
      {
        message("No results in the database for the specified experiment")
      }
    }
    # Error messages output by the chech experiment method
  },error = function(e)
  {
    message(paste("Error in analysing LHC results in database. Error message generated:\n",e))
  })
}

#' Take LHC experiment from database and produce plots of the results
#'
#' @param dblink A link to the database in which the results are stored
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param measure_scale The scale in which each response is measured. Included on the output plot
#' @param output_directory Where graphs should be output to
#' @param experiment_id Experiment ID for the experiment being plotted. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#' @param output_type Type of graph to produce. Can be PDF, TIFF, PNG, BMP
#'
graph_lhc_analysis<-function(dblink, parameters, measures, measure_scale, output_directory, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL, output_type=c("PDF"))
{
  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the analysed results for this experiment
      results<-DBI::dbGetQuery(dblink,paste("SELECT ",toString(parameters),",",toString(measures)," FROM spartan_parameters,spartan_analysed_results WHERE spartan_analysed_results.experiment_set_id=",
                                            experiment_id," AND spartan_parameters.experiment_id=",experiment_id,
                                            " AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;", sep=""))

      if(nrow(results) > 0)
      {
        # We need to get the coefficients, so we can plot these in the headers
        coeffs<-DBI::dbGetQuery(dblink,paste("SELECT * FROM spartan_generated_stats WHERE experiment_set_id=",experiment_id,";",sep=""))

        spartan::lhc_graphMeasuresForParameterChange_from_db(results, coeffs, parameters, measures, measure_scale, output_directory,
                                                             output_type = c("PDF"))
      }
      else
      {
        message("No results in the database for the specified experiment")
      }
    }
    # Error messages output by the chech experiment method
  },error = function(e)
  {
    message(paste("Error in analysing LHC results in database. Error message generated:\n",e))
  })
}
