% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emulated_sa_to_db.R
\name{analyse_and_add_emulated_efast_to_db}
\alias{analyse_and_add_emulated_efast_to_db}
\title{Take a set of emulated efast experiments, add to database, and run eFAST analysis}
\usage{
analyse_and_add_emulated_efast_to_db(dblink, parameter_value_set, emulator,
  parameters, measures, experiment_id = NULL,
  experiment_description = NULL, experiment_date = Sys.Date(),
  graph_results = FALSE, output_directory = NULL,
  normalise_sample = FALSE, normalise_result = FALSE)
}
\arguments{
\item{dblink}{A link to the database in which this table is being created}

\item{parameter_value_set}{Set of parameters used in the experiment. Expects an R object generated by spartan}

\item{emulator}{Emulator or ensemble object to use to generate predictions in place of original simulation
generated by spartan method emulate_efast_sampled_parameters}

\item{parameters}{The parameters of the simulation that are being analysed}

\item{measures}{The measures of the simulation that are being assessed}

\item{experiment_id}{Experiment ID for the results being added. May be NULL if description and date specified}

\item{experiment_description}{A description of this experiment. May be NULL if adding by experiment ID}

\item{experiment_date}{Date experiment created. May be NULL if adding by experiment ID}

\item{graph_results}{Whether analysis should be plotted once complete}

\item{output_directory}{If graph_results is TRUE, where analysis should be plotted to on the file system}

\item{normalise_sample}{Whether the generated sample being used to make predictions should be normalised prior to input into simulator}

\item{normalise_result}{Whether the result that comes out of the emulator needs to be rescaled (using the scales held in the emulator)}
}
\description{
Take a set of emulated efast experiments, add to database, and run eFAST analysis
}
