#' Creates database table that stores link to each experiment
#'
#' Creates a table that gives each experiment its' own ID, stores the type
#' of analysis performed (LHC, Robustness, eFAST), the date entered and a
#' text description of that study.
#' @param dblink A link to the database in which this table is being created
#'
#' @export
create_experiments_table<-function(dblink)
{
  query<-"CREATE TABLE spartan_experiment (
  experiment_id INT NOT NULL AUTO_INCREMENT,
  experiment_type VARCHAR(45) NOT NULL,
  experiment_date DATE NOT NULL,
  experiment_description VARCHAR(90),
  PRIMARY KEY (experiment_id),
  UNIQUE INDEX experiment_id_UNIQUE (experiment_id ASC));"

  RMySQL::dbSendQuery(dblink, query)
}

#' Creates the mySQL string to create columns for each parameter, or measure, required in the table
#' @param field_list List of columns required in the database. Could be parameters or simulation
#' response measures.
#' @return String to add to a mySQL query when creating a table
create_field_string <- function(field_list)
{
  field_string<-""
  for(entry in field_list)
  {
    field_string <- paste(field_string, entry, " VARCHAR(45) NOT NULL,",sep="")
  }

  # Add on two others, one for parameter of interest and one for curve
  # These are used by robustness and efast sampling, but should remain as empty for lhc
  field_string <- paste(field_string, "paramOfInterest VARCHAR(45),",sep="")
  field_string <- paste(field_string, "curve INT,",sep="")

  return(field_string)
}

#' Creates database table that stores all generated parameter sets for each experiment
#'
#' Creates a table that gives each parameter set its' own ID, stores the values for each
#' of the parameters, and the ID of the experiment to which this set belongs. To ensure
#' all sampling analyses in spartan can be stored in this database, additional columns for
#' parameter of interest and curve are added for robustness and eFAST sampling techniques,
#' yet these remain empty for lhc
#'
#' @param dblink A link to the database in which this table is being created
#' @param parameters The parameters of the simulation that are being analysed
#'
#' @export
create_parameter_values_table<-function(dblink, parameters)
{
  field_string <- create_field_string(parameters)

  query<-paste("CREATE TABLE spartan_parameters (parameter_set_id INT NOT NULL AUTO_INCREMENT,",
               field_string,
               "experiment_id INT NULL,
               PRIMARY KEY (parameter_set_id),
               INDEX experiment_id_idx (experiment_id ASC),
               CONSTRAINT experiment_id
               FOREIGN KEY (experiment_id)
               REFERENCES spartan_experiment (experiment_id)
               ON DELETE NO ACTION
               ON UPDATE NO ACTION);",sep="")

  RMySQL::dbSendQuery(dblink, query)
}

#' Creates database table that stores results for all parameter sets for each experiment
#'
#' Creates a table that gives each result set its' own ID, stores the outputs obtained in
#' simulation for those values, and the parameter and experiment ID to which these
#' results belong. There may be multiple results for each parameter set in this table,
#' to account for execution of replicates required to reduce aleatory uncertainty (as
#' explained in the spartan package)
#'
#' @param dblink A link to the database in which this table is being created
#' @param measures The measures of the simulation that are being assessed
#'
#' @export
create_simulation_results_table <- function(dblink, measures)
{
  field_string <- create_field_string(measures)

  query<-paste("CREATE TABLE spartan_results (result_set_id INT NOT NULL AUTO_INCREMENT,",
               field_string, "parameter_set_id INT NOT NULL,experiment_set_id INT NOT NULL,PRIMARY KEY (result_set_id),
               INDEX parameter_set_id_idx (parameter_set_id ASC),
               INDEX experiment_set_id_idx (experiment_set_id ASC),
               CONSTRAINT parameter_set_id
               FOREIGN KEY (parameter_set_id)
               REFERENCES spartan_parameters (parameter_set_id)
               ON DELETE NO ACTION
               ON UPDATE NO ACTION,
               CONSTRAINT experiment_set_id
               FOREIGN KEY (experiment_set_id)
               REFERENCES spartan_experiment (experiment_id)
               ON DELETE NO ACTION
               ON UPDATE NO ACTION);",sep="")

  RMySQL::dbSendQuery(dblink, query)
}

#' Creates database table that stores analysed results for all parameter sets for each experiment
#'
#' The results table consists of results of replicate executions of the simulation under the same
#' parameter set. This table contains analysed results, or summarises, derived from these replicates.
#' So for example, in an LHC, each parameter set is executed a number of times, and all responses
#' stored in the results table. These are then summarised to calculate a median response for those
#' distributions, and those summary statistics stored in this table.
#'
#' @param dblink A link to the database in which this table is being created
#' @param measures The measures of the simulation that are being assessed
#'
#' @export
create_analysed_results_table <- function(dblink, measures)
{
  field_string <- create_field_string(measures)

  query<-paste("CREATE TABLE spartan_analysed_results (analysed_set_id INT NOT NULL AUTO_INCREMENT,",
               field_string,
               "summarising_parameter_set_id INT NOT NULL,
    PRIMARY KEY (analysed_set_id),
    UNIQUE INDEX analysed_set_id_UNIQUE (analysed_set_id ASC),
    INDEX summarising_parameter_set_id_idx (summarising_parameter_set_id ASC),
    CONSTRAINT summarising_parameter_set_id
    FOREIGN KEY (summarising_parameter_set_id)
    REFERENCES spartan_parameters (parameter_set_id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);",sep="")

  RMySQL::dbSendQuery(dblink, query)
}
