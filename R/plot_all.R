

#' Save a plotly HTML widget
#' @import htmlwidgets
save_plotly <- function(fig, directory, file, selfcontained = TRUE) {
  htmlwidgets::saveWidget(
    widget        = fig,
    file          = file.path(directory, file),
    selfcontained = selfcontained,
    libdir        = file.path("lib"),
  )
}


#' @export
plot_all <- function(
  data_dir            = "./grups_output",
  sample_regex        = "[A-Za-z0-9]+(?:[-0-9]+){0,1}",
  threads             = 1,
  recompute_svm_probs = FALSE,
  optargs             = list(
    pwd_plot = list(
      hide_self_comparisons = FALSE,
      norm_avg_type = "Corr",
      min_overlap   = 1500,
      norm_request  = FALSE,
      norm_metric   = "Median",
      norm_method   = "Pairwise",
      norm_value    = NA
    ),
    sliding_window = list(
      block_width = 20,
      block_step  = 1
    ),
    matrix = list(
        dimensions = c(1200, 1200)
    )
  )
) {

  # -------- Fetch required input files
  message("Fetching required files...")
  pair_regex <- paste0("(?<=-)(", sample_regex, "-", sample_regex, ")")
  grups_rs_files <- grups.plots::fetch_grups_rs_files(data_dir, pair_regex)
  res_files    <- grups_rs_files[["results"]]
  pwd_files    <- grups_rs_files[["pwd"]]
  blk_files    <- grups_rs_files[["blk"]]
  sim_files    <- grups_rs_files[["sims"]]

  # ------- Parse and create output directory
  plot_dir <- paste(data_dir, "plots", sep="/")
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }

  # -------- Load files
  results_df <- grups.plots::load_res_file(res_files[1])

  # ---- Plot pairwise differences barplot
  message("Plotting barplot of pairwise differences...")
  pwd_df     <- grups.plots::load_pairwise_file(
    path          = pwd_files[1],
    res_data      = results_df,
    sample_regex  = sample_regex,
    norm_avg_type = optargs$pwd_plot$norm_avg_type,
    min_overlap   = optargs$pwd_plot$min_overlap,
    norm_request  = optargs$pwd_plot$norm_request,
    norm_method   = optargs$pwd_plot$norm_method,
    norm_metric   = optargs$pwd_plot$norm_metric,
    norm_value    = optargs$pwd_plot$norm_value
  )

  pwd_plot <- grups.plots::plot_pairwise_diff(
    data = pwd_df,
    hide_self_comparisons = optargs$pwd_plot$hide_self_comparisons,
    norm_method = optargs$pwd_plot$norm_method,
    norm_metric = optargs$pwd_plot$norm_metric
  )
  save_plotly(pwd_plot, plot_dir, "pwd-plot.html")

  # ----- Plot Sliding window scatterplot

  message("Printing sliding windows scatterplot files")
  for (pair in rownames(blk_files)) {
    block_df <- grups.plots::load_blockfile( 
      path  = blk_files[pair, ],
      width = optargs$sliding_window$block_width,
      step  = optargs$sliding_window$block_step
    )

    blk_plot_dir <- file.path(plot_dir, "sliding-windows", pair)
    if (!dir.exists(blk_plot_dir)) {
      dir.create(blk_plot_dir, recursive = TRUE)
    }

    blk_plot <- grups.plots::plot_sliding_window(
      block_dataframe = block_df,
      pair            = pair
    )
    save_plotly(
      blk_plot,
      blk_plot_dir,
      paste0("sliding-windows-", pair, ".html")
    )
  }

  # ----- Plot kinship matrix
  message("Plotting kinship matrix...")
  kinship_matrix_plot <- grups.plots::plot_kinship_matrix(
    kinship_matrix = tryCatch(
      {
        grups.plots::get_kinship_matrix(
          results_df,
          sample_regex
        )
      },
      error = grups.plots::extract_pair_names
    ),
    dimensions    = optargs$matrix$dimensions
  )
  save_plotly(kinship_matrix_plot, plot_dir, "kinship-matrix.html")

  message("Plotting pairwise simulation plots...")
  for (pair in rownames(sim_files)) {
    # ---- Plot simulation violin plots
    sims_df <- grups.plots::load_simfile(path = sim_files[pair, ])
    simulations_plot <- grups.plots::plot_pedigree_sims(
      pair           = pair,
      sims_dataframe = sims_df,
      results_data   = results_df,
      labels_to_plot = levels(sims_df$label)
    )
    violin_plot_dir <- file.path(plot_dir, "simulations-plots")
    if (!dir.exists(violin_plot_dir)) {
      dir.create(violin_plot_dir, recursive = TRUE)
    }
    save_plotly(
      simulations_plot,
      violin_plot_dir,
      paste0("simulations-plot-", pair, ".html")
    )

    # ---- Plot Matrix of Bhattacharya distances
    bc_plot <- grups.plots::plot_bc_matrix(
      bc_matrix = grups.plots::get_bc_matrix(
        sims_data = sims_df,
        labels_to_keep = levels(sims_df$label)
      ),
      plot_title = "<b>Matrix of Bhattacharya coefficients</b>",
      cutoff_values = c(-Inf, 0.01, 0.05, Inf),
      cutoff_labels = c("< 0.01", "< 0.05", ">= 0.05"),
      absolute_values = FALSE,
      marker_text = "<b>BC coefficient:</b>"
    )
    bc_plot_dir <- file.path(plot_dir, "BC-matrices")
    if (!dir.exists(bc_plot_dir)) {
      dir.create(bc_plot_dir, recursive = TRUE)
    }
    save_plotly(bc_plot, bc_plot_dir, paste0("BC-matrix-", pair, ".html"))

    # ---- Plot OR matrix
    or_plot <- grups.plots::plot_bc_matrix(
      bc_matrix = grups.plots::get_odds_matrix(
        sims_data        = sims_df,
        observed_results = results_df,
        pair             = pair,
        labels_to_keep   = levels(sims_df$label)
      ),
      plot_title      = "<b>Matrix of log(Odds Ratio)</b>",
      marker_text     = NULL,
      cutoff_values   = c(-Inf, log(1), log(100), Inf),
      cutoff_labels   = c("<= 1/1", "<= 100/1 ", "> 100/1"),
      cutoff_colors   = c("#CC6677", "#DDCC77", "#44AA99"),
      right_align     = TRUE,
      absolute_values = TRUE
    )

    # Dirty trick to obtain a proper marker text annotation.
    or_plot[["x"]][["attrs"]][[1]][["text"]] <- ~paste(
      "<b>Odds Ratio:</b>", exp(value), "<br>",
      "<b>Log(OR):</b>", value
    )

    or_plot_dir <- file.path(plot_dir, "OR-matrices")
    if (!dir.exists(or_plot_dir)) {
      dir.create(or_plot_dir, recursive = TRUE)
    }
    save_plotly(or_plot, or_plot_dir, paste0("OR-matrix-", pair, ".html"))

    conf_plot <- grups.plots::plot_or_confidence(
      or_matrix = grups.plots::get_odds_matrix(
        sims_data        = sims_df,
        observed_results = results_df,
        pair             = pair,
        labels_to_keep   = levels(sims_df$label)
      ),
      predictor = results_df[which(results_df[, 1] == pair), 2]
    )

    conf_plot_dir <- file.path(plot_dir, "OR-confidences")
    if (!dir.exists(conf_plot_dir)) {
      dir.create(conf_plot_dir, recursive = TRUE)
    }
    save_plotly(conf_plot, conf_plot_dir, paste0("OR-confidence-", pair, ".html"))
  }
}