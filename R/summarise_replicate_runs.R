#' Summarise replicate runs of robustness parameter sets into summary results table of database
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the results being summarised. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
#' @export
summarise_replicate_robustness_runs<-function(dblink,parameters,measures,experiment_id=NULL,experiment_description=NULL,experiment_date=NULL)
{
  tryCatch({
    # Check the experiment and parameters exist
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Check whether there are results to process for this experiment
      num_results<-DBI::dbGetQuery(dblink,paste("SELECT COUNT(result_set_id) AS number_results FROM spartan_results WHERE experiment_set_id=",experiment_id,";",sep=""))

      if(num_results > 0)
      {
        # Get the number of parameter sets in the experiment, to set up the matrix
        number_samples<-DBI::dbGetQuery(dblink,paste("SELECT COUNT(parameter_set_id) AS number_samples FROM spartan_parameters WHERE experiment_id=",experiment_id,";",sep=""))
        results_summary<-matrix(nrow=as.numeric(number_samples),ncol=length(measures)+3)
        row_ref<-1

        # Now process each parameter in turn
        for(p in 1:length(parameters))
        {
          # Need to get the parameter set ID range for the value sets for this parameter
          min_id<-as.numeric(DBI::dbGetQuery(dblink,paste("SELECT MIN(parameter_set_id) FROM spartan_results WHERE experiment_set_id=",experiment_id," AND paramOfInterest='",parameters[p],"';",sep="")))
          max_id<-as.numeric(DBI::dbGetQuery(dblink,paste("SELECT MAX(parameter_set_id) FROM spartan_results WHERE experiment_set_id=",experiment_id," AND paramOfInterest='",parameters[p],"';",sep="")))

          # Now process each of these samples
          for(s in min_id:max_id)
          {
            #print(s)
            # Get the results for this parameter set
            set_results <- DBI::dbGetQuery(dblink,paste("SELECT ",toString(measures)," FROM spartan_results WHERE experiment_set_id=",experiment_id," AND paramOfInterest='",parameters[p],
                                                        "' AND parameter_set_id=",s,";",sep=""))

            #print(paste(s, " ",nrow(set_results),sep=""))

            if(nrow(set_results)>0)
            {
              # Take the median of both columns and store in the matrix
              for(m in 1:length(measures))
              {
                results_summary[row_ref,m]<-stats::median(as.numeric(set_results[,m]))
              }
              # Param of interest
              results_summary[row_ref,length(measures)+1]<-parameters[p]
              # Add parameter set ID
              results_summary[row_ref,length(measures)+2]<-s
              # Add experiment ID
              results_summary[row_ref, length(measures)+3]<-experiment_id
            }

            # Still increase the row reference, we'll remove the NA's when adding to the database
            row_ref<-row_ref+1

          }
        }
        # Now we can add these summarys to the analysed results table
        colnames(results_summary)<-c(measures,"paramOfInterest","summarising_parameter_set_id","experiment_set_id")
        RMySQL::dbWriteTable(dblink, value = as.data.frame(results_summary[stats::complete.cases(results_summary), ]),row.names=FALSE,name="spartan_analysed_results", append=TRUE)
      }
      else
      {
        message("Error in summarising replicate experiments. No results in the DB for this experiment.")
      }
    }
  }, error = function(e)
  {
    message(paste("Error in summarising replicate experimental results in database. Error message generated:\n",e,sep=""))
  })
}


#' Summarise replicate runs of LHC parameter sets into summary results table of database
#'
#' @param dblink A link to the database in which this table is being created
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the results being summarised. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
#' @export
summarise_replicate_lhc_runs<-function(dblink, measures, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL)
{
  tryCatch({
    # Check the experiment and parameters exist
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Check whether there are results to process for this experiment
      num_results<-DBI::dbGetQuery(dblink,paste("SELECT COUNT(result_set_id) AS number_results FROM spartan_results WHERE experiment_set_id=",experiment_id,";",sep=""))

      if(num_results > 0)
      {
        # Get the number of parameter sets for this experiment
        number_samples <- as.numeric(DBI::dbGetQuery(dblink,paste("SELECT COUNT(parameter_set_id) AS number_samples FROM spartan_parameters WHERE experiment_id=",experiment_id,";",sep="")))

        # Declare a matrix for storing the results
        results_summary<-matrix(nrow=number_samples,ncol=length(measures)+2)

        for(k in 1:number_samples)
        {
          # Get the results from the database for this set
          set_results <- DBI::dbGetQuery(dblink,paste("SELECT ",toString(measures)," FROM spartan_results WHERE experiment_set_id=",experiment_id," AND parameter_set_id=",k,";",sep=""))

          # Take the median of both columns and store in the matrix
          for(m in 1:length(measures))
          {
            results_summary[k,m]<-stats::median(as.numeric(set_results[,m]))
          }
          # Add parameter set ID
          results_summary[k,length(measures)+1]<-k
          # Add experiment ID
          results_summary[k, length(measures)+2]<-experiment_id
        }

        # Now we can add these summarys to the analysed results table
        colnames(results_summary)<-c(measures,"summarising_parameter_set_id","experiment_set_id")
        RMySQL::dbWriteTable(dblink, value = as.data.frame(results_summary),row.names=FALSE,name="spartan_analysed_results", append=TRUE)
      }
      else
      {
        message("Error in summarising replicate experiments. No results in the DB for this experiment.")
      }
    }
    # No else here as the error messages will be generated by the function called in the IF statement
  }, error = function(e)
  {
    message(paste("Error in summarising replicate experimental results in database. Error message generated:\n",e,sep=""))
  })
}

#' Summarise replicate runs of eFAST parameter sets into summary results table of database
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the results being summarised. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
#' @export
summarise_replicate_efast_runs<-function(dblink, parameters, measures, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL)
{
  # For eFAST, there is a Dummy parameter. This should exist in the database incase eFAST is used
  # So we add it, if the user has not specified it in their parameters
  if(!"Dummy" %in% parameters & !"dummy" %in% parameters)
    parameters<-c(parameters,"Dummy")
  else if("dummy" %in% parameters) # Just make it upper case to match rest of code.
    parameters[match("dummy",parameters)]<-"Dummy"

  tryCatch({

    # Check the experiment and parameters exist
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)

    # if experiment ID is ok:
    if(experiment_id != -1)
    {
      # Check there are results for this experiment:


      num_results<-DBI::dbGetQuery(dblink,paste("SELECT COUNT(result_set_id) AS number_results FROM spartan_results WHERE experiment_set_id=",experiment_id,";",sep=""))
      if(num_results>0)
      {
        # eFAST a bit more complex, we have a number of curves and a number of samples - we can use the DB to recover both
        num_curves<-max(DBI::dbGetQuery(dblink, paste("SELECT DISTINCT curve FROM spartan_parameters WHERE experiment_id=",experiment_id,";",sep="")))
        # Now can get the number of samples for the first curve, for the first parameter
        number_samples <- as.numeric(DBI::dbGetQuery(dblink,paste("SELECT COUNT(parameter_set_id) AS number_samples FROM spartan_parameters WHERE experiment_id=",experiment_id," AND curve=1 AND paramOfInterest='",parameters[1],"';"
    ,sep="")))

        results_summary<-matrix(nrow=number_samples*num_curves*length(parameters),ncol=length(measures)+4)

        row_ref<-1
        # Now we can process each curve and parameter set
        for(c in 1:num_curves)
        {
          for(p in 1:length(parameters))
          {
            message(paste("Processing Curve ",c, " Parameter ",parameters[p],sep=""))

            #set_results <- DBI::dbGetQuery(dblink,paste("SELECT ",toString(measures)," FROM spartan_results WHERE experiment_set_id=",experiment_id," AND curve=",c," AND paramOfInterest=",p,";",sep=""))

            # Issue now is that the parameter_set_id's do not reset for each curve/parameter pair, so we need to work out the min and max of this addition and then process the id's inbetween
            param_range<-DBI::dbGetQuery(dblink,paste("SELECT parameter_set_id,",toString(measures)," FROM spartan_results WHERE experiment_set_id=",experiment_id," AND curve=",c," AND paramOfInterest='",parameters[p],"';",sep=""))
            min_id<-min(param_range[,1])
            max_id<-max(param_range[,1])

            # Now process each of these samples
            for(s in min_id:max_id)
            {
              # Subset the results by parameter set id
              set_results<-subset(param_range,param_range$parameter_set_id==s,select=measures)

              #print(s)
              # Get the results for this parameter set
              #set_results <- DBI::dbGetQuery(dblink,paste("SELECT ",toString(measures)," FROM spartan_results WHERE experiment_set_id=",experiment_id," AND curve=",c," AND paramOfInterest='",parameters[p],
              #      "' AND parameter_set_id=",s,";",sep=""))

              # Take the median of both columns and store in the matrix
              for(m in 1:length(measures))
              {
                results_summary[row_ref,m]<-stats::median(as.numeric(set_results[,m]))
              }
              # Param of interest
              results_summary[row_ref,length(measures)+1]<-parameters[p]
              # Curve:
              results_summary[row_ref,length(measures)+2]<-c
              # Add parameter set ID
              results_summary[row_ref,length(measures)+3]<-s
              # Add experiment ID
              results_summary[row_ref, length(measures)+4]<-experiment_id

              row_ref<-row_ref+1

            }
          }
        }

        # Now we can add this to the database
        colnames(results_summary)<-c(measures,"paramOfInterest","curve","summarising_parameter_set_id","experiment_set_id")
        # Note the removal of any NA rows here - this should be checked
        RMySQL::dbWriteTable(dblink, value = as.data.frame(results_summary[stats::complete.cases(results_summary), ]),row.names=FALSE,name="spartan_analysed_results", append=TRUE)
      }
    }
  }, error = function(e)
  {
    message(paste("Error in summarising replicate experimental results in database. Error message generated:\n",e,sep=""))
  })
}
