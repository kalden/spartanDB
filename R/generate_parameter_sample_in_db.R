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
#'
#' @export
generate_robustness_set_in_db <- function(dblink,parameters,baseline, minvals, maxvals, incvals, experiment_id=NULL, experiment_description=NULL)
{
  # In this case, one set of parameter values is generated per parameter. The DB will need to know the parameter of interest for the set. This will
  # be done by adding a field - parameterOfInterest, to the parameter table - the analysis can then use this to work out which parameter sets
  # belong where
  if(is.null(experiment_id))
  {
    experiment_id <- setup_experiment(dblink,"Robustness",Sys.Date(), experiment_description)
  }

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
      add_parameter_set_to_database(dblink, param_set, experiment_id)
    }
  }
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
#'
#' @export
generate_efast_set_in_db <- function(dblink, parameters, num_samples, minvals, maxvals, num_curves, experiment_id=NULL, experiment_description=NULL)
{
  # In this case, one set of parameter values is generated per parameter. The DB will need to know the parameter of interest for the set. This will
  # be done by adding a field - parameterOfInterest, to the parameter table - the analysis can then use this to work out which parameter sets
  # belong where. However, a number of resample curves may also be applied, and the algorithm needs to know these two. These are kept in the field
  # curve, which is again not used in lhc or robustness analysis

  if(is.null(experiment_id))
  {
    experiment_id <- setup_experiment(dblink,"eFAST",Sys.Date(), experiment_description)
  }

  # CHeck a valid experiment ID was found
  if(experiment_id != -1)
  {
    # Generate the eFAST sample
    sample<-spartan::efast_generate_sample(FILEPATH=NULL, num_curves, num_samples, parameters, minvals, maxvals, write_csv=FALSE,return_sample=TRUE)

    # Now to output each set into the database
    for(curve in 1:num_curves) {
      for(param in 1:length(parameters)) {
        # Get the sample, and append the parameter of interest and the resample curve
        output_params <- cbind(sample[, , param, curve], rep(parameters[param],nrow(sample[, , param, curve])),rep(curve,nrow(sample[, , param, curve])))
        # Get the headers to match the database
        colnames(output_params)<-c(parameters,"paramOfInterest","curve")
        # Now add to the database
        add_parameter_set_to_database(dblink, output_params, experiment_id)

      }
    }
  }
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
#'
#' @export
generate_lhc_set_in_db <- function(dblink, parameters, num_samples, minvals, maxvals, algorithm, experiment_id=NULL, experiment_description=NULL)
{
  if(is.null(experiment_id))
  {
    experiment_id <- setup_experiment(dblink,"LHC",Sys.Date(), experiment_description)
  }

  # Check above was successful (returned a value that isn't -1)
  if(experiment_id != -1)
  {
    # If no experiment ID is specified, generate a new experiment
    sample<-spartan::lhc_generate_lhc_sample(FILEPATH=NULL, parameters, num_samples, minvals, maxvals, "normal", write_csv=FALSE)
    add_parameter_set_to_database(dblink, sample, experiment_id)
  }
}

#' Add a generated parameter set to the database
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameter_set Parameter set to add to the database
#' @param experiment_id Database experiment ID to which this parameter set belongs
#'
add_parameter_set_to_database<-function(dblink, parameter_set,experiment_id)
{
  # Parameter set needs additional link to the experiment id to be added to dataset
  #print(head(parameter_set))
  r<-cbind(parameter_set,rep(experiment_id,nrow(parameter_set)))
  #print(head(r))
  colnames(r)<-c(colnames(r)[1:ncol(r)-1],"experiment_id")
  RMySQL::dbWriteTable(dblink, value = as.data.frame(r), row.names = FALSE, name = "spartan_parameters", append = TRUE )
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
}
