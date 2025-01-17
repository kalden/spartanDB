% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_table_setup.R
\name{create_parameter_field_string}
\alias{create_parameter_field_string}
\title{Creates the mySQL string to find a record where parameters have a given value}
\usage{
create_parameter_field_string(parameters, values)
}
\arguments{
\item{parameters}{List of parameters to build into the query}

\item{values}{Values for each of those parameters}
}
\value{
String to add to a mySQL query
}
\description{
Creates the mySQL string to find a record where parameters have a given value
}
