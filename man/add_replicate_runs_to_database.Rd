% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_analysis_to_db.R
\name{add_replicate_runs_to_database}
\alias{add_replicate_runs_to_database}
\title{Adds replicate runs to the database for a given experiment ID}
\usage{
add_replicate_runs_to_database(dblink, parameters, measures, all_results,
  experiment_id, experiment_type, curve = NULL,
  param_of_interest = NULL)
}
\arguments{
\item{dblink}{A link to the database in which this table is being created}

\item{parameters}{The parameters of the simulation that are being analysed}

\item{measures}{The measures of the simulation that are being assessed}

\item{all_results}{Set of results to add to the database}

\item{experiment_id}{ID of this experiment in the database}

\item{experiment_type}{Type of experiment for which these runs were performed}

\item{curve}{For eFAST, which resample curve this result is for}

\item{param_of_interest}{For eFAST, the parameter for which this set is of interest}
}
\value{
Boolean showing success in adding to DB or failure
}
\description{
Takes a set of results and adds these to the database, ensuring
these are linked to the correct parameter ID and experiment ID. As
spartan outputs results in groups of the same parameter values, this
method takes advantage of this by collecting results into one set
before adding these to the database, to ensure better performance
and less connections required to the database.
}
