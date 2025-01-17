% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_replicate_runs.R
\name{summarise_replicate_efast_runs}
\alias{summarise_replicate_efast_runs}
\title{Summarise replicate runs of eFAST parameter sets into summary results table of database}
\usage{
summarise_replicate_efast_runs(dblink, parameters, measures,
  experiment_id = NULL, experiment_description = NULL,
  experiment_date = NULL)
}
\arguments{
\item{dblink}{A link to the database in which this table is being created}

\item{parameters}{The parameters of the simulation that are being analysed}

\item{measures}{The measures of the simulation that are being assessed}

\item{experiment_id}{Experiment ID for the results being summarised. May be NULL if description and date specified}

\item{experiment_description}{A description of this experiment. May be NULL if adding by experiment ID}

\item{experiment_date}{Date experiment created. May be NULL if adding by experiment ID}
}
\description{
Summarise replicate runs of eFAST parameter sets into summary results table of database
}
