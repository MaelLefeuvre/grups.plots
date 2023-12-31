% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sim_deviation.R
\name{get_sim_deviation_label}
\alias{get_sim_deviation_label}
\title{Compute the average simulated deviation for a given reference relationship}
\usage{
get_sim_deviation_label(sims_dataframe, simlabel, obs_pwd, ...)
}
\arguments{
\item{sims_dataframe}{a pair-specific .sims dataframe.
Most likely the output of grups.plots::load_simfile()}

\item{obs_pwd}{a pair-specific observed pairwise mismatch rate}
}
\value{
a deviation ratio
}
\description{
Compute the average simulated deviation for a given reference relationship
}
