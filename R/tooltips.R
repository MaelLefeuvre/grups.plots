
#' (private) Wrapper which outputs a dataframe containing tooltips
#' for every Shiny widget.
#' @export
#' @return a dataframe of one row, where each column corresponds to
#'         a static shiny inputId, and values correspond to the text
#'         contents of a tooltip
tooltips <- function() {
  data.frame(
    norm_request          = "Display normalized values of pairwise mismatch rates on the y-axis.",
    hide_self_comparisons = "Hide self-comparisons from the figure.\nSelf-comparisons are detected whenever the label of the left-individual matches that of the right-individual",
    norm_method           = "Choose which subset of comparisons should be used to compute a normalization value:\n
    - Pairwise comparisons (2Ms): Use pairwise comparisons to normalize results, by applying a normalization metric on their pairwise mismatch rate values.\n
    - Selc-comparisons (Ms): Use self-comparison values to normalize results, by applying a normalization metric on their pairwise mismatch rate values.\n
    - Value: Apply a predefined normalization value.",
    norm_metric   = "Choose which metric to apply to normalize pairwise mismatch rates.",
    min_overlap   = "Exclude individuals, whose overlap is found lower than the specified threshold.\nThese individuals will be ignored when computing the normalization value.\nNote that individuals below the threshold will also be excluded from the dataframe found within the 'Summary' tab.",
    norm_avg_type = "Select whether to use raw observed average PWD value from the 'pwd-from-stin' module,\nor corrected average PWD values from the 'pedigree-sims' module ",
    block_width   = "Select the size of window (in Megabase).",
    block_step    = "Select the sliding step of each window (in Megabase).",
    kinship_matrix_ordered_labels = "Reorder labels within the kinship matrix heatmap.\n(Drag labels to change the order)",
    ks_alpha      = "Set the alpha level for all Kolmogorov-Smirnov tests of normality\n for the simulated distributions."
  )
}