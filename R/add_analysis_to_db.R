#' Adds a set of results to the database for a given LHC or robustness experiment
#'
#' @param dblink A link to the database in which this table is being created
#' @param results_csv The path to the CSV file being added to the database
#' @param results_obj R object containing results being added to the database
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
#' @export
add_lhc_and_robustness_sim_results <- function(dblink, parameters, measures, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL, results_csv=NULL, results_obj=NULL)
{
  tryCatch({

    # Firstly we need to check the experiment exists, and has associated parameter sets in the database
    # Though experiment_id may have been specified, it may not have been if searching DB by description and date. Thus the check function
    # returns the experiment_id once this is retrieved (or if check ok where experiment_id specified, just returns the same ID)
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)
    if(experiment_id != -1)
    {
      # Need to check whether results already exist for this experiment
      pre_existing_results<-DBI::dbGetQuery(dblink,paste0("SELECT COUNT(result_set_id) FROM spartan_results WHERE experiment_set_id=",experiment_id))

      if(pre_existing_results==0)
      {
        if(!is.null(results_csv))
        {
          results <- round(utils::read.csv(results_csv, header = TRUE, check.names = FALSE),digits=12)
        }
        else if(!is.null(results_obj))
        {
          results <- round(results_obj,digits=12)
        }

        # Providing this is read in correctly, we can then process these into the DB
        if(nrow(results)>0)
        {
          experiment_type <- DBI::dbGetQuery(dblink, paste0("SELECT experiment_type FROM spartan_experiment WHERE experiment_id=",experiment_id,";"))
          success <- add_replicate_runs_to_database(dblink, parameters, measures, results, experiment_id, experiment_type)
          if(!success)
            stop("Error in Adding LHC/robustness results from CSV file")
          else
            message("Simulation Results added to results database")
        }
        else
        {
          message("No results in specified CSV file. No results added to DB")
        }
      }
      else
      {
        message(paste0("Results already exist for this experiment ID (",experiment_id,") in the database. No processing performed."))
      }
    }
    # No else here as the error messages will be generated by the function called in the IF statement
  },error = function(e)
  {
    message(paste("Error in adding LHC or robustness simulation results to database from CSV file. Generated error message:\n",e,sep=""))
  })
}

#' Adds a set of results to the database for a given eFAST experiment, from a set of CSV files
#'
#' @param dblink A link to the database in which this table is being created
#' @param results_folder_path The path to folder containing the CSV files being added to the database
#' @param parameters The parameters of the simulation that are being analysed
#' @param measures The measures of the simulation that are being assessed
#' @param num_curves  Number of resample curves employed in sampling
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#'
#' @export
add_efast_sim_results_from_csv_files <- function(dblink, results_folder_path, parameters, measures, num_curves, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL)
{

  # For eFAST, there is a Dummy parameter. This should exist in the database incase eFAST is used
  # So we add it, if the user has not specified it in their parameters
  if(!"Dummy" %in% parameters & !"dummy" %in% parameters)
    parameters<-c(parameters,"Dummy")
  else if("dummy" %in% parameters) # Just make it upper case to match rest of code.
    parameters[match("dummy",parameters)]<-"Dummy"

  tryCatch({
    # Firstly we need to check the experiment exists, and has associated parameter sets in the database
    # Though experiment_id may have been specified, it may not have been if searching DB by description and date. Thus the check function
    # returns the experiment_id once this is retrieved (or if check ok where experiment_id specified, just returns the same ID)
    experiment_id <- check_experiment_and_parameters_exist_for_adding_results(dblink, experiment_id, experiment_description, experiment_date)
    if(experiment_id != -1)
    {
      # Check that there are no results for this experiment
      pre_existing_results<-DBI::dbGetQuery(dblink,paste0("SELECT COUNT(result_set_id) FROM spartan_results WHERE experiment_set_id=",experiment_id))

      if(pre_existing_results==0)
      {

        # Rather than read in one file here, we will have to read in a number - one results file per parameter-curve pair
        for(c in 1:num_curves)
        {
          # At the moment this method assumes files are named in a set way - Curve[curve number]_Parameter[Parameter Number]_Results.csv
          # Now to iterate through all parameters
          for(p in 1:length(parameters))
          {
            message(paste("Adding Results for Curve ",c," Parameter ",p,sep=""))
            results <- utils::read.csv(file.path(results_folder_path,paste("Curve",c,"_Parameter",p,"_Results.csv",sep="")), header = TRUE, check.names = FALSE)
            # Providing this is read in correctly, we can then process these into the DB
            if(nrow(results)>0)
            {
              success <- add_replicate_runs_to_database(dblink, parameters, measures, results, experiment_id,experiment_type="eFAST",curve=c,param_of_interest = parameters[p])
              if(!success)
                stop("Error in Adding LHC results from CSV file")
              else
                message(paste(nrow(results)," added to results database",sep=""))
            }
          }
        }
        message("Addition of eFAST results to database complete")
      }
      else
      {
        message(paste0("Results already exist for this experiment ID (",experiment_id,") in the database. No processing performed."))
      }

    }
    # No else here as the error messages will be generated by the function called in the IF statement
  },error = function(e)
  {
    message(paste("Error in adding eFAST simulation results to database from CSV file. Generated error message:\n",e,sep=""))
  })
}

#' When adding results to the database, checks that an experiment exists in the database with associated parameter value sets
#'
#' @param dblink A link to the database in which this table is being created
#' @param experiment_id Experiment ID for the results being added. May be NULL if description and date specified
#' @param experiment_date Date experiment created. May be NULL if adding by experiment ID
#' @param experiment_description A description of this experiment. May be NULL if adding by experiment ID
#' @return Either the ID of the experiment or a -1 if either the experiment does not exist in the DB, or the experiment has no associated parameters
#'
check_experiment_and_parameters_exist_for_adding_results<-function(dblink, experiment_id=NULL, experiment_description=NULL, experiment_date=NULL)
{
  tryCatch({

    # User can search by experimentID or by experiment description and date, so need to check which they have done here
    if(!is.null(experiment_id))
    {
      # Check the experiment is there, and that parameter sets have been declared
      id<-DBI::dbGetQuery(dblink,paste("SELECT experiment_description, experiment_date FROM spartan_experiment WHERE experiment_id=",experiment_id,";",sep=""))

      if(nrow(id)>0)
      {
        if(check_parameter_sets_exist_for_given_experiment_id(dblink, experiment_id))
          return(experiment_id)
        else
        {
          message(paste("Parameter sets do not exist for experiment ID ",experiment_id,". Results not added to the Database",sep=""))
          return(-1)
        }
      }
      else
      {
        message(paste("Specified experiment ID (",experiment_id,") does not exist in the database. No results added to the database",sep=""))
      }
    }
    else
    {
      # Experiment ID was null, can we find an experiment with the description and date specified

      if(!is.null(experiment_description) && !is.null(experiment_date))
      {
        # Check the experiment is there, and that parameter sets have been declared
        id<-DBI::dbGetQuery(dblink,paste("SELECT experiment_id FROM spartan_experiment WHERE experiment_description='",experiment_description,"' AND experiment_date='",experiment_date,"';",sep=""))

        if(nrow(id)>0)
        {
          if(check_parameter_sets_exist_for_given_experiment_id(dblink, as.numeric(id)))
            return(as.numeric(id))
          else
          {
            message(paste("Parameter sets do not exist for experiment ID ",as.numeric(id),". Results not added to the Database",sep=""))
            return(-1)
          }
        }
      }
      else
      {
        message("Cannot add or find results from robustness or latin-hypercube experiment to the database.\n You need to enter a valid experiment ID that is in the database, or a experiment description and date that is in the database")
      }
    }
  }, error = function(e)
  {
    message(paste("Error in adding LHC results to the database. Error message generated:\n",e,sep=""))
  })
}

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
#'@param experiment_type Type of experiment for which these runs were performed
#'@param curve For eFAST, which resample curve this result is for
#'@param param_of_interest For eFAST, the parameter for which this set is of interest
#'@return Boolean showing success in adding to DB or failure
add_replicate_runs_to_database<-function(dblink, parameters, measures, all_results, experiment_id, experiment_type, curve=NULL, param_of_interest=NULL)
{
  tryCatch({

    block_to_add_to_database<-NULL

    # For robustness analysis, there is potential for the baseline to be evaluated numerous times,
    # with the parameter sample included in the database more than once. To detect this when multiple
    # parameter set IDs are returned from the database, this reference ensures the results are stored under
    # the correct parameter set ID (i.e. if there is more than one baseline result, they are each stored under
    # their own parameter set ID that are in the DB)
    baseline_db_refs<-1


    # Work on the top row initially, then we can do all the others separately
    # This ensures we don't have to query the database for a parameter set ID if the parameters are the same
    current_parameter_set<-all_results[1,1:length(parameters)]
    parameter_set_id<-get_parameter_set_id(dblink,current_parameter_set,experiment_id,baseline_db_refs)
    if(nrow(parameter_set_id)>1)
    {
      parameter_set_id<-parameter_set_id[,baseline_db_refs]
      baseline_db_refs<-baseline_db_refs+1
    }

    #print(current_parameter_set)

    if(experiment_type=="LHC") {
      f<-cbind(t(as.numeric(all_results[1,measures])),parameter_set_id,experiment_id)
      colnames(f)<-c(measures,"parameter_set_id","experiment_set_id")
    } else if(experiment_type=="eFAST") {
      f<-cbind(t(as.numeric(all_results[1,measures])),parameter_set_id,experiment_id,param_of_interest,curve)
    } else if(experiment_type=="Robustness") {
      # For a robustness analysis, we need to retrieve the parameter of interest for this parameter set from the database
      param_of_interest<-retrieve_parameter_of_interest_using_param_id(dblink,parameter_set_id)
      #param_of_interest <- retrieve_parameter_of_interest(dblink, current_parameter_set, experiment_id)
      #f<-cbind(t(as.numeric(all_results[1,measures])),parameter_set_id,experiment_id,param_of_interest[baseline_db_refs,])
      f<-cbind(t(as.numeric(all_results[1,measures])),parameter_set_id,experiment_id,param_of_interest)
      # If more than one param_of_interest was returned, increase the reference so for the next baseline set, we use the next parameter reference
      #if(nrow(param_of_interest)>1) {
      #  baseline_db_refs<-baseline_db_refs+1
      #}
    }

    block_to_add_to_database<-rbind(block_to_add_to_database,f)

    #for(result in 1488:1737)
    for(result in 2:nrow(all_results))
    {
      #print(result)
      # Get the parameters
      parameter_values = all_results[result,1:length(parameters)]
      # See if we need to get the ID for this parameter set from the database, or whether it is equal to the last set
      if(!all(current_parameter_set==parameter_values))
      {
        # We need to commit the results seen so far for the last parameter set to the database
        if(experiment_type=="LHC")
          colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id")
        else if(experiment_type=="eFAST")
          colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id","paramOfInterest","curve")
        else if(experiment_type=="Robustness")
          colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id","paramOfInterest")

        #print(current_parameter_set)
        #print(paste("Parameter Set: ",parameter_set_id," Writing ",nrow(block_to_add_to_database)," rows",sep=""))
        a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_database),row.names=FALSE,name="spartan_results", append=TRUE)
        #print(a)
        # Reset the block to empty
        block_to_add_to_database<-NULL

        # Now We need to query the value for the next set of parameters
        current_parameter_set = parameter_values
        parameter_set_id<-get_parameter_set_id(dblink, current_parameter_set,experiment_id, baseline_db_refs)
        #if(nrow(parameter_set_id)>1)
        #{
        #  parameter_set_id<-parameter_set_id[,baseline_db_refs]
        #  baseline_db_refs<-baseline_db_refs+1
        #}

        # For a robustness analysis, we need to check the parameter of interest for this new set
        if(experiment_type=="Robustness")
        {
          param_of_interest <- retrieve_parameter_of_interest(dblink, current_parameter_set, experiment_id)
          #RMySQL::dbFetch(RMySQL::dbSendQuery(dblink,paste("SELECT paramOfInterest FROM spartan_parameters WHERE parameter_set_id=",parameter_set_id,";",sep="")))

         # param_of_interest<-retrieve_parameter_of_interest_using_param_id(dblink,parameter_set_id)
          # If more than one param_of_interest was returned, increase the reference so for the next baseline set, we use the next parameter reference
          #if(nrow(param_of_interest)>1)
          #{
            #print(result)
           # baseline_db_refs<-baseline_db_refs+1
          #}
        }
      }

      # Add the result to the block to be written to the database
      if(experiment_type=="LHC")
      {
        f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id)
        colnames(f)<-c(measures,"parameter_set_id","experiment_set_id")
      } else if(experiment_type=="eFAST") {
        f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id,param_of_interest,curve)
      } else if(experiment_type=="Robustness") {

       # if(nrow(param_of_interest)>1)
      #  {
          # Assumption made here that the sample exists more than once in the parameter table, yet the results are only in the CSV file once
          # So we're going to add them to the database twice, once for each of the parameters of interest
          f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id,param_of_interest)
       # }

       # if(nrow(param_of_interest)>1)
      #    f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id,param_of_interest[baseline_db_refs,])
      #  else
      #    f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id,param_of_interest[1,])
        #f<-cbind(t(as.numeric(all_results[result,measures])),parameter_set_id,experiment_id,param_of_interest)
      }

      block_to_add_to_database<-rbind(block_to_add_to_database,f)
    }

    # Now there may still be the final set to add to the database, so write any left over results
    if(nrow(block_to_add_to_database)>0)
    {
      if(experiment_type=="LHC")
        colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id")
      else if(experiment_type=="eFAST")
        colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id","paramOfInterest","curve")
      else if(experiment_type=="Robustness")
        colnames(block_to_add_to_database)<-c(measures,"parameter_set_id","experiment_set_id","paramOfInterest")

      #print(current_parameter_set)
      #print(paste("Parameter Set: ",parameter_set_id," Writing ",nrow(block_to_add_to_database)," rows",sep=""))
      a<-RMySQL::dbWriteTable(dblink, value = as.data.frame(block_to_add_to_database),row.names=FALSE,name="spartan_results", append=TRUE)
    }

    return(TRUE)
  }, error = function(e)
  {
    message(paste("Error encountered in adding results to database. Error message generated:\n",e,sep=""))
    return(FALSE)
  })
}
