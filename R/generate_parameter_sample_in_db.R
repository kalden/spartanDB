#' Generate robustness analysis sample and store in database
#'
#' Generates a set of parameters for execution in simulation to permit
#' a robustness analysis. These parameters are stored in the database,
#' either for a given experiment ID created previously, or a new experiment
#' is created in the database
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#' @param baseline The calibrated value of each parameter being analysed
#' @param minvals The minimum value of the range to explore for each parameter
#' @param maxvals The maximum value of the range to explore for each parameter
#' @param incvals The sampling increment value to apply to each parameter between
#' the minimum and maximum limits
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
generate_robustness_set_in_db <- function(dblink,parameters,baseline, minvals, maxvals, incvals, experiment_id=NULL, experiment_description=NULL, experiment_date=Sys.Date())
{
  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({
    # In this case, one set of parameter values is generated per parameter. The DB will need to know the parameter of interest for the set. This will
    # be done by adding a field - parameterOfInterest, to the parameter table - the analysis can then use this to work out which parameter sets
    # belong where
    experiment_id <- check_experiment_id(dblink, experiment_id, "Robustness", experiment_date, experiment_description)

    if(experiment_id !=-1)
    {
      # Firstly generate all the samples using spartan
      sample <- spartan::oat_parameter_sampling(FILEPATH=NULL, parameters, baseline,minvals,maxvals,incvals, write_csv=FALSE,return_sample=TRUE)
      # Now go through each, append the parameter of interest to the set as a column, then submit for inclusion in the database
      for(param in 1:length(sample))
      {
        # Append the parameter of interest as a new column
        param_set<-cbind(sample[[param]],rep(parameters[param],nrow(sample[[param]])))
        colnames(param_set)<-c(colnames(param_set)[1:ncol(param_set)-1],"paramOfInterest")
        add_parameter_set_to_database(dblink, param_set, experiment_id, experiment_type="Robustness")
      }

      message(paste("Robustness Parameter Set Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
     message(paste("Error in Generating Robustness Analysis Sample and Storing in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Generate eFAST analysis sample and store in database
#'
#' Generates a set of parameters for execution in simulation to permit
#' an eFAST analyis. These parameters are stored in the database,
#' either for a given experiment ID created previously, or a new experiment
#' is created in the database
#' @inheritParams generate_robustness_set_in_db
#' @param num_samples The number of parameter subsets to generate - should be at
#' least 65 for eFAST
#' @param num_curves The number of 'resamples' to perform (see eFAST
#' documentation) - recommend using at least 3
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
generate_efast_set_in_db <- function(dblink, parameters, num_samples, minvals, maxvals, num_curves, experiment_id=NULL, experiment_description=NULL, experiment_date=Sys.Date())
{
  # In this case, one set of parameter values is generated per parameter. The DB will need to know the parameter of interest for the set. This will
  # be done by adding a field - parameterOfInterest, to the parameter table - the analysis can then use this to work out which parameter sets
  # belong where. However, a number of resample curves may also be applied, and the algorithm needs to know these two. These are kept in the field
  # curve, which is again not used in lhc or robustness analysis

  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({

    experiment_id <- check_experiment_id(dblink, experiment_id, "eFAST", experiment_date, experiment_description)

    if(experiment_id != -1)
    {
      # Generate the eFAST sample - note the user should have specified a dummy parameter in doing this
      sample<-spartan::efast_generate_sample(FILEPATH=NULL, num_curves, num_samples, parameters, minvals, maxvals, write_csv=FALSE,return_sample=TRUE)

      # Now to output each set into the database
      for(c in 1:num_curves) {
        for(param in 1:length(parameters)) {
          # Now add to the database - curve and parameter of interest are added in the called function
          output_params<-sample[, , param, c]
          colnames(output_params)<-c(parameters)
          add_parameter_set_to_database(dblink, output_params, experiment_id, experiment_type="eFAST", curve=c,param_of_interest = parameters[param])
        }
      }
      message(paste("eFAST Parameter Set Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
    message(paste("Error in Generating eFAST Sample and Storing in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Generate LHC analysis sample and store in database
#'
#' Generates a set of parameters for execution in simulation to permit
#' an LHC/PRCC analysis as described in spartan. These parameters are stored in the database,
#' either for a given experiment ID created previously, or a new experiment
#' is created in the database
#' @inheritParams generate_robustness_set_in_db
#' @param num_samples The number of samples to take from the hypercube
#' @param algorithm Choice of algorithm to use to generate the hypercube.
#' Can be set to either 'normal' or 'optimum'. Beware optimum can take a
#' long time to generate an optimised parameter set (more than 24 hours
#' in some circumstances)
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
generate_lhc_set_in_db <- function(dblink, parameters, num_samples, minvals, maxvals, algorithm, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({

    experiment_id <- check_experiment_id(dblink, experiment_id, "LHC", experiment_date, experiment_description)

    if(experiment_id != -1)
    {
      sample<-spartan::lhc_generate_lhc_sample(FILEPATH=NULL, parameters, num_samples, minvals, maxvals, "normal", write_csv=FALSE)
      add_parameter_set_to_database(dblink, sample, experiment_id, experiment_type="LHC")
      message(paste("Parameter Set Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
    message(paste("Error in Generating LHC Sample and Storing in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Adds a previously generated LHC sample to the database, either creating an experiment record for this in doing so or checking a specified experiment ID
#'
#' If an experiment ID is specified, the existance of this record is checked, as well as the existance of any current parameter sets for that experiment
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set to add to the database
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
add_existing_lhc_sample_to_database<-function(dblink, parameter_set, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({
    experiment_id <- check_experiment_id(dblink, experiment_id, "LHC", experiment_date, experiment_description)

    # Check above was successful (returned a value that isn't -1)
    if(experiment_id != -1)
    {
      # If no experiment ID is specified, generate a new experiment
      success <- add_parameter_set_to_database(dblink, parameter_set, experiment_id, experiment_type="LHC")
      if(!success)
        stop("Error in Adding Parameter Set to Database")
      else
        message(paste("Parameter Set Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
    message(paste("Error in Storing Pre-Existing LHC Sample in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Adds a previously generated eFAST sample to the database, either creating an experiment record for this in doing so or checking a specified experiment ID
#'
#' If an experiment ID is specified, the existance of this record is checked, as well as the existance of any current parameter sets for that experiment. In
#' this case, as spartan creates an eFAST sample over several files, the user should provide the folder containing each of these files
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set_path Path to the parameter sets to add to the database
#' @param parameters Simulation parameters being analysed
#' @param num_curves Number of resample curves employed in sampling
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
add_existing_efast_sample_to_database<-function(dblink, parameter_set_path, parameters, num_curves, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({
    experiment_id <- check_experiment_id(dblink, experiment_id, "eFAST", experiment_date, experiment_description)

    # Check above was successful (returned a value that isn't -1)
    if(experiment_id != -1)
    {
      # At the moment it is assumed that the Dummy is specified in the parameter list

      # Now cycle through all parameters and all curves
      for(c in 1:num_curves)
      {
        for(p in 1:length(parameters))
        {
          # Read in the CSV file containing the parameter sets
          params<-utils::read.csv(file.path(parameter_set_path,paste("Curve",c,"_Parameter",p,"_Parameters.csv",sep="")),header=T)
          success <- add_parameter_set_to_database(dblink, params, experiment_id, experiment_type="eFAST",curve=c,param_of_interest=parameters[p])

          if(!success)
            stop("Error in Adding eFAST Parameter Set to Database")
        }
      }

      message(paste("Parameter Set Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
    message(paste("Error in Storing Pre-Existing eFAST Sample in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Adds a previously generated robustness sample to the database, either creating an experiment record for this in doing so or checking a specified experiment ID
#'
#' If an experiment ID is specified, the existance of this record is checked, as well as the existance of any current parameter sets for that experiment. In
#' this case, as spartan creates a robustness sample over several files, the user should provide the folder containing each of these files
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set_path Path to the parameter sets to add to the database
#' @param parameters Simulation parameters being analysed
#' @param experiment_id The ID of the experiment in the database, if not a new
#' experiment. If NULL an experiment ID will be created
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @param experiment_date Date experiment created. Defaults to today's date
#' if not entered
#'
#' @export
add_existing_robustness_sample_to_database<-function(dblink, parameter_set_path, parameters, experiment_id=NULL, experiment_description=NULL, experiment_date = Sys.Date())
{
  # Flag to store if a new experiment is created, in case rollback is required on sample generation error
  new_experiment_flag <- set_new_experiment_flag(experiment_id)

  tryCatch({
    experiment_id <- check_experiment_id(dblink, experiment_id, "Robustness", experiment_date, experiment_description)

    # Check above was successful (returned a value that isn't -1)
    if(experiment_id != -1)
    {
      # There is one CSV sheet per parameter, so we iterate through these
      for(p in 1:length(parameters))
      {
        # Read in the CSV file containing the parameter sets
        params<-utils::read.csv(file.path(parameter_set_path,paste(parameters[p],"_OAT_Values.csv",sep="")),header=T)
        success <- add_parameter_set_to_database(dblink, params, experiment_id, experiment_type="Robustness",param_of_interest=parameters[p])

        if(!success)
          stop("Error in Adding eFAST Parameter Set to Database")
      }
      message(paste("Parameter Sets Added to Database, with Experiment ID ",experiment_id,sep=""))
    }
  }, error = function(e)
  {
    message(paste("Error in Storing Pre-Existing Robustness Sample in Database. Error Message Generated: \n",e,sep=""))
    if(new_experiment_flag)
      remove_errored_new_experiment_from_db(dblink, experiment_id)
  })
}

#' Add a generated parameter set to the database
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set to add to the database
#' @param experiment_id Database experiment ID to which this parameter set belongs
#' @param experiment_type Type of experiment being designed
#' @param curve For eFAST, which curve this parameter belongs to
#' @param param_of_interest For eFAST and Robustness, which parameter this set is specific to
#' @return Boolean showing success of writing to the table or not
#'
add_parameter_set_to_database<-function(dblink, parameter_set,experiment_id, experiment_type, curve=NULL, param_of_interest=NULL)
{
  tryCatch({
    # Parameter set needs additional link to the experiment id to be added to dataset
    if(experiment_type=="LHC")
    {
      r<-cbind(parameter_set,rep(experiment_id,nrow(parameter_set)))
      colnames(r)<-c(colnames(r)[1:(ncol(r)-1)],"experiment_id")
    }
    else if(experiment_type=="eFAST")
    {
      r<-cbind(parameter_set,rep(experiment_id,nrow(parameter_set)),rep(param_of_interest,nrow(parameter_set)),rep(curve,nrow(parameter_set)))
      colnames(r)<-c(colnames(r)[1:(ncol(r)-3)],"experiment_id","paramOfInterest","curve")
    }
    else if(experiment_type=="Robustness")
    {
      r<-cbind(parameter_set,rep(experiment_id,nrow(parameter_set)),rep(param_of_interest,nrow(parameter_set)))
      colnames(r)<-c(colnames(r)[1:(ncol(r)-2)],"experiment_id","paramOfInterest")
    }

    RMySQL::dbWriteTable(dblink, value = as.data.frame(r), row.names = FALSE, name = "spartan_parameters", append = TRUE )
    return(TRUE)
  }, error = function(e)
  {
    message(paste("Error in Adding Existing Parameter Set to Database. Error Message Generated:\n",e,sep=""))
    return(FALSE)
  })
}

#' Creates a new experiment ID if one not specified, and checks ID is in database with no parameter sets if one is specified
#'
#' @param dblink A link to the database in which this table is being created
#' @param experiment_id Experiment ID for parameter set being generated. May be NULL to create one, or specified
#' @param experiment_type Type of spartan sample being generated
#' @param experiment_date Date experiment created.
#' @param experiment_description A description of this experiment, if a new
#' experiment is being created
#' @return Either a new experiment ID, the specified experiment ID if no error, or -1 if error encountered
check_experiment_id<-function(dblink, experiment_id, experiment_type, experiment_date, experiment_description)
{
  tryCatch({
    # If experiment ID is NULL, we generate one
    if(is.null(experiment_id))
    {
      return(setup_experiment(dblink,experiment_type,experiment_date, experiment_description))
    }
    else
    {
      # An ID has been specified, we need to check it exists, and whether parameter sets already exist for that experiment
      # Does the experiment ID exist:
      id<-DBI::dbGetQuery(dblink,paste("SELECT experiment_description, experiment_date FROM spartan_experiment WHERE experiment_id=",experiment_id,";",sep=""))
      if(nrow(id)>0)
      {
        # Exists. Now need to check whether there are parameter values already there for this experiment, and fail this addition if there are
        if(check_parameter_sets_exist_for_given_experiment_id(dblink, experiment_id))
        {
          message(paste("Parameter values already exist for experiment ID ",experiment_id,". Set not added to the Database",sep=""))
          # Return -1 to indicate error in addition of this set
          return(-1)
        }
        else
        {
          # Ok. Return experiment ID
          return(experiment_id)
        }
      }
      else
      {
        message(paste("Specified experiment ID ",experiment_id," is not in the database. Set not added to the Database",sep=""))
        # Return -1 to indicate error in addition of this set
        return(-1)
      }
    }
  }, error = function(e)
  {
    message(paste("Error in Checking Experiment ID for adding parameters to Database. Error message generated:\n",e,sep=""))
  })
}

#' Sets the flag used to denote whether a new experiment was created in sampling. Used to roll DB back on error
#' @param experiment_id Specified experiment ID to function. If NULL, a new experiment was created
#' @return Boolean flag showing whether a new experiment ID is needed (TRUE) or one was specified (FALSE)
set_new_experiment_flag <- function(experiment_id)
{
  if(is.null(experiment_id))
    return(TRUE)
  else
    return(FALSE)
}

#' Used when creating a new experiment for a new sample, yet an error is encountered in sampling. Removes generated experiment
#'
#' This ensures no new experiments exist in the database where an error has been encountered in sampling. If the user specified
#' an experiment ID to use when generating a sample, that experiment ID is not removed
#'
#' @param dblink A link to the database in which this table is being created
#' @param experiment_id Id of the experiment created that needs to be deleted
remove_errored_new_experiment_from_db <- function(dblink, experiment_id)
{
  tryCatch({
    id<-DBI::dbGetQuery(dblink,paste("DELETE FROM spartan_experiment WHERE experiment_id=",experiment_id,";",sep=""))
    message(paste("Experiment ID ",experiment_id,", created in failed attempt at sample generation, now deleted from DB",sep=""))
  }, error = function(e)
  {
    message(paste("Error in rolling back experiment database to delete experiment ID ",experiment_id," created in failed sample generation",sep=""))
  })
}




#' Setup a new experiment in the database, returning the generated ID of the experiment
#'
#' @param dblink A link to the database in which this entry is being added
#' @param experiment_type Whether the experiment was a consistency, robustness, LHC, or eFAST analysis
#' @param experiment_date Date the experiment was performed
#' @param experiment_description Description of the experiment being performed
#' @return Generated ID for this experiment (the database table primary key)
#'
#' @export
setup_experiment <- function(dblink,experiment_type,experiment_date, experiment_description)
{
  tryCatch({
    # Check no match in database already
    id<-DBI::dbGetQuery(dblink,paste("SELECT experiment_id FROM spartan_experiment WHERE experiment_type='",experiment_type,"' AND experiment_description='",experiment_description,
                                         "' AND experiment_date='",experiment_date,"';",sep=""))
    if(nrow(id)==0)
    {
      query<-paste("INSERT INTO spartan_experiment (experiment_type, experiment_date, experiment_description) VALUES('",experiment_type,"','",experiment_date,"','",
                   experiment_description,"');",sep="")
      out<-DBI::dbGetQuery(dblink, query)
      return(get_experiment_id(dblink,experiment_type,experiment_date, experiment_description))
    }
    else
    {
      message("Experiment description and date already in the database")
      return(-1)
    }
  }, error = function(e)
  {
    message(paste("Error in adding an experiment to the database. Error Message generated:\n",e,sep=""))
  })
}
