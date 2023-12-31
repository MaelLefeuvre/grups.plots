% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_svmop_probs.R
\name{get_svmop_probs}
\alias{get_svmop_probs}
\title{Compute per-class SVM probabilities of belonging to a given relatedness
for every pairwise comparison, using the 'e1071' R package.}
\usage{
get_svmop_probs(results_file, sim_files, threads = 1, progressor = NULL)
}
\arguments{
\item{results_file}{A'.result' dataframe containing the results of the
'pedigree-sims' module of GRUPS-rs}

\item{sim_files}{a list of '.sims' files. One for every pairwise comparison}

\item{threads}{number of additional worker threads}

\item{progressor}{optional progressr::progressor to update a progress bar.}
}
\value{
a dataframe containing SVM probabilities for every pairwise
comparison.
}
\description{
Compute per-class SVM probabilities of belonging to a given relatedness
for every pairwise comparison, using the 'e1071' R package.
}
