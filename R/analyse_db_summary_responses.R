#' Using results stored in the database, calculates A-Test scores for
#' all measure-parameter pairs in a robustness analysis experiment
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters Simulation parameters being examined
#' @param measures Simulation output responses
#' @param baseline Baseline/Calibrated value for each of the parameters
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
generate_robustness_analysis<-function(dblink, parameters, measures, baseline, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL)
{
  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the analysed results for this experiment
      results<-DBI::dbGetQuery(dblink,paste("SELECT spartan_parameters.paramOfInterest,",toString(parameters),",",toString(measures)," FROM spartan_parameters,spartan_results WHERE spartan_results.experiment_set_id=",
                                            experiment_id," AND spartan_parameters.experiment_id=",experiment_id,
                                            " AND spartan_results.parameter_set_id=spartan_parameters.parameter_set_id;", sep=""))

      if(nrow(results)>0)
      {
        # Get the parameter IDs, parameter of interest, and values for the parameters in this experiment
        param_ids<-DBI::dbGetQuery(dblink,paste("SELECT parameter_set_id,paramOfInterest,",toString(parameters), " FROM spartan_parameters WHERE experiment_id=",experiment_id,";",sep=""))

        # Work out the min, max, and inc values used in sampling
        sampling_settings_info<-construct_range_info_vectors(parameters, param_ids)

        # Calculate the A-Test scores for all the results in the database
        a_test_scores <- spartan::oat_csv_result_file_analysis_from_DB(results, parameters, baseline, measures, sampling_settings_info$min_vals,sampling_settings_info$max_vals,
                                                                       sampling_settings_info$inc_vals)


        # Now to construct the result block for the database
        block_to_add_to_database<-matrix(nrow=(nrow(param_ids)*length(measures)),ncol=6)
        row_ref<-1

        for (r in 1:nrow(a_test_scores))
        {
          # We need to find the ID of the parameter set, for adding to the database, so subset the parameter ID results
          param_set<-param_ids
          for(p in 1:length(parameters))
            param_set <- subset(param_set, param_set[,parameters[p]]
                         == a_test_scores[r,parameters[p]])

          # If the baseline is found, there may be more than one ID in the param_set. We're going to store the results of the same parameter values for these different IDs
          # such that all parameter set ID's have a result
          for(id in 1:nrow(param_set))
          {
            col_offset<-3
            for(m in 1:length(measures))
            {
              block_to_add_to_database[row_ref,1]<-param_set[id,2]
              block_to_add_to_database[row_ref,2]<-measures[m]
              block_to_add_to_database[row_ref,3]<-a_test_scores[r,col_offset]
              block_to_add_to_database[row_ref,4]<-a_test_scores[r,(col_offset+1)]
              block_to_add_to_database[row_ref,5]<-experiment_id
              block_to_add_to_database[row_ref,6]<-param_set[id,1]
              row_ref<-row_ref+1
              col_offset<-col_offset+2
            }
          }
        }

        # Name the columns to match the database structure
        colnames(block_to_add_to_database)<-c("parameter","measure","statistic_1","statistic_2","experiment_set_id","parameter_set_id")
        a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_database),row.names=FALSE,name="spartan_generated_stats", append=TRUE)

        message("A-Test Scores Produced for all Parameter-Measure Pairs and Stored in the Database")
      }
      else
      {
        message("No results in the database for the specified experiment")
      }
    }
    # Error messages for experiment ID are within the experiment check function
  },error = function(e)
  {
    message(paste("Error in analysing Robustness results in database. Error message generated:\n",e))
  })
}

#' To save having to input min, max, and inc values for the analysis, this works them out from sampling
#'
#' @param parameters Simulation parameters being examined
#' @param db_result results of the DB query for parameter sets
#' @return List containing vector for min, max, and inc values used in sampling
construct_range_info_vectors<-function(parameters, db_result)
{
  min_vals=c()
  max_vals=c()
  inc_vals=c()
  for(p in parameters)
  {
    a<-subset(db_result,db_result$paramOfInterest==p,select=c(p))
    min_vals<-c(min_vals,min(a[,1]))
    max_vals<-c(max_vals,max(a[,1]))

    # We can also use this to work out the increment
    # Assumes all will be the same throughout the set
    inc_vals<-c(inc_vals, as.numeric(a[2,1])-as.numeric(a[1,1]))
  }

  return(list("min_vals"=min_vals,"max_vals"=max_vals,"inc_vals"=inc_vals))

}

#' Function to retrieve robustness results from the database and produce plots
#'
#' @param dblink A link to the database in which this table is being created
#' @param output_directory Where the graphs should be stored
#' @param parameters Simulation parameters being examined
#' @param measures Simulation output responses
#' @param ATESTSIGLEVEL Level above 0.5 at which a difference is deemed a large difference
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
graph_robustness_analysis<-function(dblink, output_directory, parameters, measures, ATESTSIGLEVEL=0.23, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL)
{
  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the analysed results for this experiment
      results<-DBI::dbGetQuery(dblink,paste("SELECT parameter, measure, statistic_1, statistic_2, ",toString(parameters)," FROM spartan_parameters,spartan_generated_stats WHERE spartan_generated_stats.experiment_set_id=",
                                        experiment_id," AND spartan_parameters.experiment_id=",experiment_id,
                                        " AND spartan_generated_stats.parameter_set_id=spartan_parameters.parameter_set_id;", sep=""))

      if(nrow(results)>0)
      {
        for (p in 1:length(parameters)) {
          message(paste("Creating graph for Parameter ", parameters[p],
                        sep = ""))

          graph_file <- file.path(output_directory,
                                   paste(parameters[p], "pdf",sep="."))
          graph_title <- paste("A-Test Scores when adjusting parameter \n",
                              parameters[p], sep = "")

          grDevices::pdf(graph_file, width = 12, height = 7)
          graphics::par(xpd = NA, mar = c(4, 4, 4, 17))

          # Plot the first measure
          to_graph<-subset(results,results$parameter==parameters[p] & results$measure==measures[1])
          to_graph<-to_graph[order(to_graph[,parameters[p]]),]


          graphics::plot(to_graph[,parameters[p]], to_graph$statistic_1,
               type = "o", main = graph_title,
               lty = 1, ylim = c(0, 1), pch = 1, xlab = "Parameter Value",
               ylab = "A Test Score", xaxt = "n")

          # Now add any others
          if (length(measures) > 1) {
            # NOW ADD THE REST OF THE MEASURES
            for (l in 2:length(measures)) {
              to_graph<-subset(results,results$parameter==parameters[p] & results$measure==measures[l])
              to_graph<-to_graph[order(to_graph[,parameters[p]]),]

              graphics::lines(to_graph[,parameters[p]],
                    to_graph$statistic_1,
                    type = "o", lty = 5, pch = l)
            }
          }

          graphics::axis(1, to_graph[,parameters[p]])
          graphics::legend(graphics::par("usr")[2], graphics::par("usr")[4], title = "Measures", measures,
                 pch = 1:length(measures), cex = 0.7, ncol = 1)
          graphics::par(xpd = FALSE)

          graphics::abline(a = 0.5, b = 0, lty = 4)
          text_pos <- (max(as.numeric(to_graph[,parameters[p]])) + min(as.numeric(to_graph[,parameters[p]]))) / 2
          graphics::text(text_pos, 0.52, "no difference",
               col = "blue")
          a_abline <- 0.5 + ATESTSIGLEVEL
          graphics::abline(a = a_abline, b = 0, lty = 4)
          graphics::text(text_pos, (0.5 + ATESTSIGLEVEL
                          + 0.02),
               "large difference", col = "blue")
          a_abline <- 0.5 - ATESTSIGLEVEL
          graphics::abline(a = a_abline, b = 0, lty = 4)
          graphics::text(text_pos, (0.5 - ATESTSIGLEVEL
                          - 0.02),
               "large difference", col = "blue")


          grDevices::dev.off()
        }
      }
      else
      {
        message("No results in the database for the specified experiment")
      }
    }
  },error=function(e)
  {
    message(paste("Error in analysing LHC results in database. Error message generated:\n",e))
  })
}

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

      if(nrow(results) > 0)
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

#' From the results stored in a mysql database, produce summary statistics using eFAST technique
#'
#' @param dblink A link to the database in which the results are stored
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the experiment being plotted. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
generate_efast_analysis<-function(dblink, parameters, measures, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL)
{
  # For eFAST, there is a Dummy parameter. This should exist in the database incase eFAST is used
  # So we add it, if the user has not specified it in their parameters
  if(!"Dummy" %in% parameters & !"dummy" %in% parameters)
    parameters<-c(parameters,"Dummy")
  else if("dummy" %in% parameters) # Just make it upper case to match rest of code.
    parameters[match("dummy",parameters)]<-"Dummy"

  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Get the analysed results for this experiment
      results<-DBI::dbGetQuery(dblink,paste("SELECT ",toString(measures),",spartan_analysed_results.paramOfInterest,spartan_analysed_results.curve FROM spartan_parameters,spartan_analysed_results WHERE spartan_analysed_results.experiment_set_id=",
                                            experiment_id," AND spartan_parameters.experiment_id=",experiment_id,
                                            " AND spartan_analysed_results.summarising_parameter_set_id=spartan_parameters.parameter_set_id;", sep=""))

      # Calculate the number of samples
      number_samples<-nrow(DBI::dbGetQuery(dblink,paste("SELECT parameter_set_id FROM spartan_parameters WHERE experiment_id=",experiment_id," AND paramOfInterest='",parameters[1],"' AND curve=1;",sep="")))

      if(nrow(results) > 0)
      {
        # We're going to construct the summary file that spartan uses to do the analysis here, one for each curve
        all_curve_results<-NULL
        for(c in 1:max(results$curve))
        {
          column_names<-NULL
          summary_table<-NULL
            col_ref<-1

          for(p in 1:length(parameters))
          {
            for(m in 1:length(measures))
            {
              summary_table<-cbind(summary_table,subset(results,results$paramOfInterest==parameters[p] & results$curve==c,select=measures[m])[,1])
              column_names<-c(column_names,paste(parameters[p],"_Median",measures[m],sep=""))
            }
          }
            # Make summary table numeric
          summary_table<-apply(summary_table, 2,as.numeric)
          colnames(summary_table) <- column_names

          # Bind to results of all curves
          all_curve_results<-cbind(all_curve_results,summary_table)

        }

        # Get the results in the final structure the eFAST analysis methods requires
        all_curve_results <- as.matrix(all_curve_results)
        final_structure<- array(all_curve_results, dim = c(number_samples, (length(parameters) * length(measures)),
                                                           max(results$curve)))

        # Now we can use the spartan methods to analyse the data
        efast_results<-spartan::efast_run_Analysis_from_DB(final_structure, number_samples, parameters,
                                                           max(results$curve), measures)


        # Now to put this in the DB. In eFAST's case there are 9 stats for each measure
        # Si, Si PVal, STi, STi PVal, SCi, Si Coefficient, STi Coefficient, Si Error bar, STi Error Bar. These are
        # stored in stat 1-9 in the database, and these are translated as required when graphing
        block_to_add_to_db<-matrix(nrow=length(parameters)*length(measures),ncol=12)
        row_ref<-1

        for(r in 1:nrow(efast_results))
        {
          col_offset<-0

          for(m in 1:length(measures))
          {
            block_to_add_to_db[row_ref,1]<-row.names(efast_results)[r]
            # Need to get the measure name - assuming in same order as provided measures
            block_to_add_to_db[row_ref,2]<-measures[m]
            block_to_add_to_db[row_ref,3]<-efast_results[r,(col_offset+1)]
            block_to_add_to_db[row_ref,4]<-efast_results[r,(col_offset+2)]
            block_to_add_to_db[row_ref,5]<-efast_results[r,(col_offset+3)]
            block_to_add_to_db[row_ref,6]<-efast_results[r,(col_offset+4)]
            block_to_add_to_db[row_ref,7]<-efast_results[r,(col_offset+5)]
            block_to_add_to_db[row_ref,8]<-efast_results[r,(col_offset+6)]
            block_to_add_to_db[row_ref,9]<-efast_results[r,(col_offset+7)]
            block_to_add_to_db[row_ref,10]<-efast_results[r,(col_offset+8)]
            block_to_add_to_db[row_ref,11]<-efast_results[r,(col_offset+9)]
            block_to_add_to_db[row_ref,12]<-experiment_id
            row_ref<-row_ref+1
            col_offset<-col_offset+9
          }
        }
        # Write this set to the DB
        colnames(block_to_add_to_db)<-c("parameter","measure","statistic_1","statistic_2","statistic_3","statistic_4","statistic_5","statistic_6","statistic_7","statistic_8","statistic_9","experiment_set_id")
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
    message(paste("Error in analysing eFAST results in database. Error message generated:\n",e))
  })
}

#' Mine the DB for an eFAST experiment and plot the results
#'
#' @param dblink A link to the database in which the results are stored
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param output_directory Where graphs should be output to
#' @param experiment_id Experiment ID for the experiment being plotted. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
graph_efast_analysis<-function(dblink, parameters, measures, output_directory, experiment_id=NULL, experiment_date=NULL, experiment_description=NULL)
{
  # For eFAST, there is a Dummy parameter. This should exist in the database incase eFAST is used
  # So we add it, if the user has not specified it in their parameters
  if(!"Dummy" %in% parameters & !"dummy" %in% parameters)
    parameters<-c(parameters,"Dummy")
  else if("dummy" %in% parameters) # Just make it upper case to match rest of code.
    parameters[match("dummy",parameters)]<-"Dummy"

  tryCatch({
    # First check the experiment exists
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      results<- DBI::dbGetQuery(dblink,paste("SELECT * FROM spartan_generated_stats WHERE experiment_set_id=",experiment_id,";",sep=""))

      if(nrow(results)>0)
      {
          colors <- c("black", "grey50")

          for (m in seq(length(measures)))
          {
            ### The database output is not in the same format as is used for spartan graphing
            ### Getting the data into that format would have taken as much code as tweaking the graph code to
            ## fit the data structure the database produces
            ### So the latter has been done, and the spartan function is not called

            GRAPHFILE <- file.path(output_directory, paste(measures[m], ".pdf",sep=""))
            GRAPHTITLE <- paste("Partitioning of Variance in Simulation Results
                                  Measure: ", measures[m],
                                sep = "")

            grDevices::pdf(GRAPHFILE)
            labelspacing <- seq(2, (length(parameters) * 3), 3)

            # DATA TO GRAPH RETRIEVES THE PARAMETERS,
            # si AND sti TO BE GRAPHED FROM THE MAIN RESULT SET
            analysis_result<-subset(results,results$measure==measures[m],select=c(results$statistic_1,results$statistic_3, results$statistic_8, results$statistic_9))
            data_to_graph <- data.frame(as.numeric(analysis_result[,1]), as.numeric(analysis_result[,2]),check.names = FALSE)


            # CONSTRUCT THE ERROR BAR
            high_si <- as.numeric(data_to_graph[, 1]) + as.numeric(analysis_result[,3])
            high_sti <- as.numeric(data_to_graph[, 2]) + as.numeric(analysis_result[,4])

            # COMBINE
            errors_high <- cbind(high_si, high_sti)

            colnames(data_to_graph) <- c("Si", "STi")
            graphics::par(mar = c(9, 4, 4, 2) + 0.1)
            gplots::barplot2(t(data_to_graph), names.arg = parameters, beside = TRUE,
                             main = GRAPHTITLE,
                             ylim = c(0, 1.0),
                             ylab = "eFAST Sensitivity", col = colors, xaxt = "n",
                             plot.ci = TRUE, ci.u = t(errors_high),
                             ci.l = t(data_to_graph))

            # TEXT SIZE CONTROLLED BY CEX.AXIS
            graphics::axis(1, at = labelspacing, labels = parameters, las = 2, cex.axis = 0.6)
            graphics::legend("topleft", title = NULL, c("Si", "STi"), fill = colors)

            grDevices::dev.off()
          }
          message(paste("Graphs Output to ", output_directory, sep = ""))
      }

      else
      {
        message(paste("No results in the database for experiment ID ",experiment_id,sep=""))
      }
    }
    # Error messages taken care of by experiment ID check
  },error = function(e)
  {
    message(paste("Error in plotting eFAST results from database. Error message generated:\n",e))
  })
}
