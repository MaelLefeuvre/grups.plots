#' Compute the average simulated deviation between the observed PMR, and
#' the closest pedigree simulationdistribution.
#' @param sims_dataframe a pair-specific .sims dataframe.
#'        Most likely the output of grups.plots::load_simfile()
#' @param obs_pwd a pair-specific observed pairwise mismatch rate
#' @return a deviation ratio
#' @export
get_sim_deviation_closest <- function(sims_dataframe, obs_pwd, ...) {
  sim_distrib_avg <- aggregate(avg ~ label, sims_dataframe, FUN = mean)$avg
  closest_rel     <- which.min(abs(sim_distrib_avg - obs_pwd))
  1 - (obs_pwd / sim_distrib_avg[closest_rel])
}

#' Compute the average simulated deviation for a given reference relationship
#' @param sims_dataframe a pair-specific .sims dataframe.
#'        Most likely the output of grups.plots::load_simfile()
#' @param obs_pwd a pair-specific observed pairwise mismatch rate
#' @return a deviation ratio
#' @export
get_sim_deviation_label <- function(sims_dataframe, simlabel, obs_pwd, ...) {
  sim_distrib_avg <- aggregate(avg ~ label, sims_dataframe, FUN = mean)
  label_avg <- sim_distrib_avg$avg[sim_distrib_avg$label == simlabel]
  1 - (obs_pwd / label_avg)
}

#' Compute the average simulated deviation for a fiven pair of individual
#' @import progressr
#' @import stringr
#' @export
compute_sim_deviation <- function(
  sim_files,
  results_df,
  sample_regex,
  label_request,
  percent       = TRUE,
  simlabel      = NULL,
  metric        = NULL,
  subset        = NULL,
  progressor    = NULL
  ) {
  # ---- fetch the appropriate summary statistic function
  metric_func <- grups.plots::get_norm_metric(metric)

  # ---- Filter self- or pairwise comparisons, if requested.
  results_df$Self <- stringr::str_detect(
    results_df$Pair_name,
    paste0("^(", sample_regex, ")-(\\1)$")
  )

  results_df <- switch(subset,
    "Pairwise comparisons" = results_df[which(!results_df$Self), ],
    "Self comparisons"     = results_df[which(results_df$Self), ],
    "All comparisons"      = results_df
  )

  # Choose the appropriate function, depending on the user's request
  deviation_func <- ifelse(
    label_request,
    get_sim_deviation_label,
    get_sim_deviation_closest
  )

  n <- NROW(sim_files)
  i <- 0
  deviations <- sapply(results_df$Pair_name, function(x) {
    if (!is.null(progressor)) {
      i   <<- i +1
      progressor(sprintf("Processing [%d/%d]: %s", i, n, x))
    }
    deviation_func(
      sims_dataframe = grups.plots::load_simfile(sim_files[x, 'path']),
      obs_pwd        = with(results_df, Corr.Avg.PWD[Pair_name == x]),
      simlabel       = simlabel
    )
  })
  
  output <- metric_func(deviations)
  if (percent) {
    output <- output * 100
    return(paste0("Deviation: ", output, " %"))
  } else {
    return(paste0("Deviation: ", output))
  }
}
