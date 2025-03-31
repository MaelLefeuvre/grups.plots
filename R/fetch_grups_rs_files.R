# ---- Default error function when failing to find rownames
#' @export
extract_pair_names <- function(cond, pair_regex) {
  msg <- base::paste0(
    "Failed to retrieve some files using the following regular expression: ",
    "'", pair_regex, "'\n\n",
    "This may be due to an invalid sample_regex, given your sample names. ",
    "Try running grups.plots::app() with an alternate sample_regex.\n\n",
    "Original error message:\n", cond
  )
  stop(msg)
}

#' @import stringr
#' @export
fetch_grups_rs_files <- function(data_dir = "grups_output", pair_regex) {
  # ---- Default error function when failing to find rownames
  extract_pair_names <- function(cond) {
    msg <- base::paste0(
      "Failed to retrieve some files using the following regular expression: ",
      "'", pair_regex, "'\n\n",
      "This may be due to an invalid sample_regex, given your sample names. ",
      "Try running grups.plots::app() with an alternate sample_regex.\n\n",
      "Original error message:\n", cond
    )
    stop(msg)
  }

  # ---- 1. Search for .result file(s)
  res_files <- list.files(
    path = data_dir,
    full.names = TRUE,
    pattern = "\\.result$"
  )

  # ---- Search for .prob file.
  prob_file <- list.files(
    path = data_dir,
    full.names = TRUE,
    pattern = "\\.probs$"
  )

  # ---- 2. Search for .pwd file(s)
  pwd_files <- list.files(
    path = data_dir,
    full.names = TRUE,
    pattern = "\\.pwd$"
  )

  # ---- 3a. Search for blk_file(s)
  blk_files <- list.files(
    path = paste(data_dir, "blocks", sep = "/"),
    full.names = TRUE,
    pattern = "\\.blk$"
  )

  # ---- 3b. Extract block pair names, parse all that data into a df.
  blk_files <- tryCatch({
      data.frame(
        path      = blk_files,
        row.names = stringr::str_extract(  # Extract pair names
          blk_files,
          paste0(pair_regex, "(?=.blk$)")
        ),
        stringsAsFactors = FALSE
      )
    }, error = extract_pair_names
  )

  # ---- 4a. Search for simulation files                              [A FUNC]
  sim_files <- list.files(
    path = paste(data_dir, "simulations", sep = "/"),
    full.names = TRUE,
    pattern = "\\.sims$"
  )

  # ---- 4b. Extract simulations, pair names, parse them into a df. [B FUNC]
  sim_files <- tryCatch({
      data.frame(
        path = sim_files,
        row.names = stringr::str_extract( # Extract pair names
          sim_files,
          paste0(pair_regex, "(?=.sims$)")
        ),
        stringsAsFactors = FALSE
      )
    }, error = extract_pair_names
  )

  # ---- 5a. Search for .yaml config files
  config_files <- list.files(
    path = data_dir,
    full.names = TRUE,
    pattern = "\\.yaml$"
  )

  # ---- 5b. order them in decreasing order. -> last yaml becomes the first.
  config_files <- config_files[order(config_files, decreasing = TRUE)]

  # ---- Sanity checks
  shiny::validate(
    shiny::need(
      length(pwd_files) == 1,
      "[ERROR]: Exactly one `.pwd` file must exist within `data_dir`. \
       Exiting."
    ),

    shiny::need(
      length(res_files) == 1,
      "[ERROR]: Exactly one `.result` file must exist within `data_dir`. \
       Exiting."
    ),

    shiny::need(
      length(prob_file) <= 1,
      "[ERROR]: Only one `.probs` file must exist within `data_dir`. Exiting."
    ),

    shiny::need(
      length(config_files) >= 1,
      "[ERROR]: At least one `.yaml` configuration file must exist within \
       `data_dir`. Exiting"
    )
  )

  list(
    "results" = res_files,
    "probs"   = prob_file,
    "pwd"     = pwd_files,
    "blk"     = blk_files,
    "sims"    = sim_files,
    "configs" = config_files
  )
}