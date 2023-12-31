#' Main grups.plots shiny dashboard application
#' @export
#' @import shiny
#' @import plotly
#' @import stringr
#' @import shinycssloaders
#' @import bslib
#' @import shinyBS
#' @import prompter
#' @import progressr
#' @import thematic
#' @import shinyFeedback
#' @importFrom dplyr %>%
#' @param data_dir (string) Path to a directory containing the results of a
#'        GRUPS-rs pedigree-sims kinship analysis.
#' @param sample_regex Specify the regular expression used to parse sample
#'        names from the 'Pair_Name' column (PCRE).
#' @param threads Specify additional worker threads to run this application
#' @param recompute_svm_probs Specify whether grups.plots should recompute
#'        SVM probabilities, using the 'e1071' package.
#' @param
#' @param ... additional arguments for shiny::serverApp
#' @return A shiny::shinyApp() object
app <- function(
  data_dir            = "./grups_output",
  sample_regex        = "[A-Za-z0-9]+(?:[-0-9]+){0,1}",
  threads             = 1,
  recompute_svm_probs = FALSE,
  ...
) {

  tooltips <- grups.plots::tooltips()

  progressr::handlers(global = TRUE)

  # ----- Format a file pair regular expression
  pair_regex <- paste0("(?<=-)(", sample_regex, "-", sample_regex, ")")
  # ---- 0a. Configure loading spinner animation
  options(spinner.type = 8, spinner.color = "#0dc5c1")

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

  # ---- 3a. Search for blk_file(s)                                   [A FUNC]
  blk_files <- list.files(
    path = paste(data_dir, "blocks", sep = "/"),
    full.names = TRUE,
    pattern = "\\.blk$"
  )

  # ---- 3b. Extract block pair names, parse all that data into a df. [B FUNC]
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

  thematic::thematic_shiny()
  ui <- shiny::fluidPage(

    prompter::use_prompt(),

  # ---- Dimensions of the current window
    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '),
      tags$style(HTML("
        [class*=hint--][aria-label]:after {
          white-space: pre;
        }
      "))
    ),


    theme = bslib::bs_theme(bootswatch = "darkly", version = 5) %>%
      bs_add_rules( # --- Force black text color for dropdown items.
        ".dropdown-item { color: #000000 }"
      ),
    shiny::navbarPage("GRUPS-plots",
      # ---- 1. Render summary table.
      shiny::tabPanel("Summary",
        DT::dataTableOutput("simulation_results_df") %>%
          shinycssloaders::withSpinner()
      ),
      # ---- 1. Render pwd barplots tab.
      shiny::tabPanel("Raw Pairwise differences.",
        shiny::fluidPage(
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::fluidRow(
                shiny::column(6,
                  shiny::checkboxInput("norm_request",
                    label = "Normalize"
                  )
                ) %>% prompter::add_prompt(
                  message = tooltips[["norm_request"]],
                  position = "right",
                  rounded  = TRUE,
                  bounce   = TRUE,
                  type     = "info"
                ),
                shiny::column(6,
                  shiny::checkboxInput("hide_self_comparisons",
                    label = "Hide self-comparisons"
                  ) %>% prompter::add_prompt(
                  message = tooltips[["hide_self_comparisons"]],
                  position = "right",
                  rounded  = TRUE,
                  bounce   = TRUE,
                  type     = "info"
                )
                )
              ),

              shiny::fluidRow(
                shiny::column(6,
                  shiny::radioButtons("norm_method",
                    "Normalization subset",
                    choiceNames  = c("Pairwise-comparisons (2Ms)",
                                     "Self-comparisons (Ms)",
                                     "Value (2Ms)"
                                    ),
                    choiceValues = c("Pairwise", "Self", "Value"),
                    selected = "Pairwise",
                    width = "100%"
                  ) %>% prompter::add_prompt(
                    message  = tooltips[["norm_method"]],
                    position = "right",
                    rounded  = TRUE,
                    bounce   = TRUE,
                    type     = "info"
                  )
                ),
                shiny::column(6,
                  shiny::conditionalPanel(
                    condition = "input.norm_method != 'Value'",
                    shiny::radioButtons("norm_metric",
                      label    = "Normalization metric",
                      choices  = c("Median", "Mean", "Min", "Max"),
                      selected = "Median"
                    ) %>% prompter::add_prompt(
                      message = tooltips[["norm_metric"]],
                      position = "right",
                      rounded  = TRUE,
                      bounce   = TRUE,
                      type     = "info"
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = "input.norm_method == 'Value'",
                    shiny::numericInput("norm_value",
                      label = "Normalization value",
                      value = 0.25,
                      min   = 0.01,
                      max   = 2,
                      step  = 0.01
                    )
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::numericInput("min_overlap",
                    label = "Minimum SNP overlap threshold",
                    value = 0,
                    min   = 0,
                    max   = .Machine$integer.max,
                    step  = 10000
                  ) %>% prompter::add_prompt(
                    message = tooltips[["min_overlap"]],
                    position = "right",
                    rounded  = TRUE,
                    bounce   = TRUE,
                    type     = "info"
                  )
                ),
                shiny::column(6,
                  shiny::radioButtons("norm_avg_type",
                    label = "Type",
                    choiceNames = c("Raw Average PWD", "Corrected Average PWD"),
                    choiceValues = c("Raw", "Corr"),
                  ) %>% prompter::add_prompt(
                    message = tooltips[["norm_avg_type"]],
                    position = "right",
                    rounded  = TRUE,
                    bounce   = TRUE,
                    type     = "info"
                  )
                )
              )
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Plot",
                  plotly::plotlyOutput("pwd_barplot") %>%
                    shinycssloaders::withSpinner(),
                ),
                shiny::tabPanel("Raw data",
                   DT::dataTableOutput("pwd_dataframe") %>%
                    shinycssloaders::withSpinner()
                )
              )
            )
          )
        )
      ),

      # ---- 2. Render block scatterplots tab
      shiny::tabPanel("Sliding mismatch rate",
        shiny::fluidPage(
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::selectInput("block_pair",
                "Pair name",
                rownames(blk_files)
              ),
              shiny::sliderInput("block_width",
                "Sliding window width (Megabases)",
                value = 20,
                min   = 2,
                max   = 50
              ), #%>% prompter::add_prompt(
                #message = tooltips[["block_width"]],
                #position = "right",
                #rounded  = TRUE,
                #bounce   = TRUE,
                #type     = "info"
              #),
              shiny::hr(),
              shiny::sliderInput("block_step",
                "Sliding window step (Megabases)",
                value = 1,
                min = 1,
                max = 50
              ), #%>% prompter::add_prompt(
                #message = tooltips[["min_overlap"]],
                #position = "right",
                #rounded  = TRUE,
                #bounce   = TRUE,
                #type     = "info"
              #),
              shiny::hr(),
              shiny::uiOutput("chromosome_subset_checkboxgroup") %>%
                shinycssloaders::withSpinner()
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Plot",
                  plotly::plotlyOutput("block_scatterplot") %>%
                    shinycssloaders::withSpinner()
                ),
                shiny::tabPanel("Raw data",
                  shiny::tableOutput("block_dataframe") %>%
                    shinycssloaders::withSpinner()
                )
              )
            )
          )
        )
      ),

      # ----- Render kinship matrix
      shiny::tabPanel("Kinship Matrix",
        shiny::fluidPage(
          shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::uiOutput("kinship_matrix_order") %>%
                  shinycssloaders::withSpinner()
            ) %>% prompter::add_prompt(
              message = tooltips[["kinship_matrix_ordered_labels"]],
              position = "right",
              rounded  = TRUE,
              bounce   = TRUE,
              type     = "info"
            ),
            shiny::mainPanel(
              plotly::plotlyOutput("kinship_matrix") %>%
                shinycssloaders::withSpinner(),
            )
          ),
        )
      ),

      # ---- 3. Render violin plots
      shiny::tabPanel("Simulations plot",
        shiny::fluidPage(
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::selectInput("sim_pair",
                label   = "Pair name",
                choices = rownames(sim_files)
              ),
              shiny::uiOutput("simulation_labels_checkboxgroup") %>%
                shinycssloaders::withSpinner(),
              shiny::numericInput("ks_alpha",
                label = "Kolmogorov-Smirnov alpha treshold",
                min   = 0,
                max   = 1,
                step  = 0.01,
                value = 0.05
              ) %>% prompter::add_prompt(
                message = tooltips[["ks_alpha"]],
                position = "right",
                rounded  = TRUE,
                bounce   = TRUE,
                type     = "info"
              )
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Violin Plots",
                  plotly::plotlyOutput("sims_violinplot") %>%
                    shinycssloaders::withSpinner(),
                  shiny::hr(),
                  shiny::h5(strong("Assigned SVM probabilities")),
                  DT::dataTableOutput("assigned_svm_probabilities") %>%
                    shinycssloaders::withSpinner(),
                  shiny::hr(),
                  shiny::h5(strong("Normality of simulated distributions (Kolmogorov-Smirnov test)")),
                  DT::dataTableOutput("ks_normality_test") %>%
                    shinycssloaders::withSpinner(),
                  shiny::hr(),
                  shiny::fluidRow(
                    shiny::column(6,
                      shiny::h5(strong("Matrix of Bhattacharya coefficients")),
                      plotly::plotlyOutput("bc_matrix_plot") %>%
                        shinycssloaders::withSpinner()
                    ),
                    shiny::column(6,
                      shiny::h5(strong("Matrix of Odds Ratio ")),
                      plotly::plotlyOutput("plot_or_matrix") %>%
                        shinycssloaders::withSpinner()
                    )
                  ),
                  shiny::hr(),
                  shiny::h5(strong("Log(Odds ratio) of the most likely relationship")),
                  plotly::plotlyOutput("OR_confidence")
                ),
                shiny::tabPanel("Raw data",
                  shiny::tableOutput("sims_dataframe") %>%
                    shinycssloaders::withSpinner()
                )
              )
            )
          )
        )
      ),

      # ---- TEST. Render SVM results table.
      shiny::tabPanel("SVM probabilities",
        DT::dataTableOutput("SVM_results_df") %>%
          shinycssloaders::withSpinner()
      ),

      # ---- Stats
      shiny::tabPanel("Stats",
        shiny::fluidPage(
          shiny::h5("Deviation calculator"),
          shiny::p("Estimate the overall deviation between the observed PWD values and pedigree simulation distributions."),
          shiny::fluidRow(
            shiny::column(2,
              shiny::selectInput("stats_samples_select_input_list",
                label   = "Samples subset",
                choices = c(
                  "Pairwise comparisons",
                  "Self comparisons",
                  "All comparisons"
                )
              )
            ),
            shiny::column(2,
              shiny::radioButtons("stats_simlabel_radio_button",
                label    = "Use as reference...",
                choices  = c("A specific distribution", "The closest distribution"),
                inline    = TRUE
              )
            ),
            shiny::column(2,
              shiny::conditionalPanel(
                condition = "input.stats_simlabel_radio_button == 'A specific distribution'",
                shiny::uiOutput("stats_simlabel_input_list")
              )
            ),
            shiny::column(2,
              shiny::selectInput("stats_metric_select_input_list",
               label = "Summary statistic",
               choices = c("Median", "Mean", "Min", "Max")
              )
            ),
            shiny::column(2,
              shinyFeedback::loadingButton("stats_compute_deviation_button",
                label = "Compute",
                style = "margin-top: 25px;"
              )
            ),
            shiny::column(2,
              shiny::textOutput("stats_compute_deviation_text_result"),
              style = "margin-top: 25px;"
            )
          )
        ),
      ),

      # ---- 4. Render yaml configuration file:
      shiny::tabPanel("Configuration",
        shiny::verbatimTextOutput("config_file") %>%
          shinycssloaders::withSpinner()
      ),
    ),
  )

  server <- function(input, output, session) {
    # 0 ---- Update block slider inputs
    shiny::observe({
      max_blockstep_value <- input$block_width - 1
      new_step_value <- ifelse(
        input$block_step < max_blockstep_value,
        input$block_step,
        max_blockstep_value
      )

      shiny::updateSliderInput(
        session,
        "block_step",
        value = new_step_value,
        min = 1,
        max = max_blockstep_value
      )
    })

    # 0 ---- Fit SVMOPs and compute probabilities.
    prog_msg <-  "Fitting SVM against simulations. This may take a while..."
    load_svm_probs <- shiny::reactive({
      shiny::validate(shiny::need(
        length(prob_file) > 0 || recompute_svm_probs,
        stringr::str_squish("\
          Cannot find any '.probs' file within the directory. You may \
          recompute these probabilities within grups.plots, using the \
          'e1071' package, by running the app with \
          'recompute_svm_probs = TRUE'"
        )
      ))

      if (length(prob_file) == 1 && !recompute_svm_probs) {
        grups.plots::load_svmop_probs(prob_file[1])
      } else if (recompute_svm_probs) {
        progressr::withProgressShiny(message = prog_msg, value = 0, {
          progressor <- progressr::progressor(along = seq_along(sim_files$path))
          grups.plots::get_svmop_probs(
            results_file = load_results_file(),
            sim_files    = sim_files,
            progressor   = progressor,
            threads      = threads
          )
        })
      }
  })

    output$SVM_results_df <- DT::renderDataTable({
      probs <- load_svm_probs()
      DT::datatable(
        probs,
        style   = "bootstrap5",
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50 , 100, -1),
            c("10", "25", "50", "100", "All")
          ),
          ordering = TRUE,
          scrollX = FALSE,
          autoWidth = TRUE,
          searching = TRUE,
          paging = TRUE,
          dom = paste0(
            "<'row'<'col-sm-4 col-md'l><'col-sm-8 col-md-auto'f><'col-sm-4 col-md-auto'B>>",
            "<'row'<'col-sm-12'tr>>",
            "<'row'<'col-sm-12 col-md-5'i><'col-sm-12 col-md-7'p>>"
          ),
          buttons = list(
              list(extend = "copy", text = "Copy to clipboard"),
              list(
                extend = "collection",
                className = 'btn-xs btn-warning dropdown-toggle text-primary',
                buttons = list(
                  list(extend = "csv", className = "btn-xs"),
                  "excel",
                  "pdf"
                ),
                text = "Download"
              )
          )
        ),
        filter  = "top",
      ) %>% DT::formatRound(
        columns = colnames(probs)[3:length(colnames(probs))],
        digits = 5
      )
    })

    load_results_file <- shiny::reactive({
      grups.plots::load_res_file(res_files[1])
    })

    # ---- 1a. Load results  summary dataframe
    output$simulation_results_df <- DT::renderDataTable({
      DT::datatable(
        grups.plots::merge_pwd_results(
          pwd_df = load_pairwise_dataframe(),
          res_df = load_results_file()
        ),
        style = "bootstrap5",
        options = list(ordering = TRUE, scrollX = FALSE),
        class = "table-condensed",
        filter = "top",
      )
  })

    # ---- 1b. Load / Update pairwise dataframe.
    load_pairwise_dataframe <- shiny::reactive(
      grups.plots::load_pairwise_file(
        path          = pwd_files[1],
        res_data      = load_results_file(),
        norm_avg_type = input$norm_avg_type,
        sample_regex  = sample_regex,
        min_overlap   = input$min_overlap,
        norm_request  = input$norm_request,
        norm_method   = input$norm_method,
        norm_metric   = input$norm_metric,
        norm_value    = input$norm_value
      )
    )

    # ---- Test: Get relatednes
    output$kinship_matrix_order <- shiny::renderUI({
      shinyjqui::orderInput(
        inputId = "kinship_matrix_ordered_labels",
        label = "Re-order labels (click and drag to change)",
        items = levels(load_sims_dataframe()$label)
      )
    })


    # ---- 2a. Render pairwise difference barplot.
    output$pwd_barplot <- plotly::renderPlotly({
      options(warn = -1)
      grups.plots::plot_pairwise_diff(
        data                  = load_pairwise_dataframe(),
        hide_self_comparisons = input$hide_self_comparisons,
        norm_method           = input$norm_method,
        norm_metric           = input$norm_metric,
      )}
    )

    # ---- 2b.Render kinship matrix
    shiny::observeEvent(input$dimension, {
      options(warn = -1)
      output$kinship_matrix <- plotly::renderPlotly(
        grups.plots::plot_kinship_matrix(
          kinship_matrix = tryCatch(
            {
              grups.plots::get_kinship_matrix(
                load_results_file(),
                sample_regex
              )
            },
            error = extract_pair_names
          ),
          dimensions = input$dimension,
          order      = input$kinship_matrix_ordered_labels
        )
      )
    })

    # ---- 2c. Render pairwise dataframe
    output$pwd_dataframe <- DT::renderDataTable(
      DT::datatable(
        load_pairwise_dataframe()$data,
        style = "bootstrap5",
        options = list(ordering = TRUE, scrollX = TRUE),
        class = "table-condensed",
        filter = "top",
      )

    )

    # ---- 3a. Load / Update block dataframe
    load_block_dataframe <- shiny::reactive({
      shiny::validate(shiny::need(
        NROW(blk_files) > 0,
        "Failed to discover any '.blk' file. Is the 'blocks' directory missing?"
      ))
      grups.plots::load_blockfile(
        path  = blk_files[input$block_pair, ],
        width = input$block_width,
        step  = input$block_step
      )
    })

    # ---- 3b. Output chromosome subset checkbox group
    output$chromosome_subset_checkboxgroup <- shiny::renderUI({
      chromosomes <- levels(as.factor(load_block_dataframe()$chr))
      grups.plots::shiny_reactive_checkbox(
        values  = chromosomes,
        title   = "Display chromosome(s):",
        input_id = "chromosome_labels",
        ncol    = 6
      )
    })

    # ---- 3c. Render block-scatterplot
    output$block_scatterplot <- plotly::renderPlotly({
      grups.plots::plot_sliding_window(
        load_block_dataframe(),
        pair            = input$block_pair
      )
    })

    # ---- 3d. Filter out chromosome which were not requested by the user.
    shiny::observeEvent(input$chromosome_labels, {
      chr_to_hide <- unique(
        load_block_dataframe()$chr[
          !(load_block_dataframe()$chr %in% input$chromosome_labels)
        ] - 1
      )
      chr_to_keep <- unique(
        load_block_dataframe()$chr[
          (load_block_dataframe()$chr %in% input$chromosome_labels)
        ] - 1
      )
      plotly::plotlyProxy("block_scatterplot",
        session
      ) %>%
      plotly::plotlyProxyInvoke("restyle",
        list(visible = FALSE),
        chr_to_hide
      ) %>%
      plotly::plotlyProxyInvoke("restyle",
        list(visible = TRUE),
        chr_to_keep
      )

    })

    # ---- 3e. Reset user-selected chromosome if he used select/deselect All.
    shiny::observeEvent(input$chromosome_labels_select, {
      chromosomes <- levels(as.factor(load_block_dataframe()$chr))
      shiny::updateCheckboxGroupInput(
        session  = session,
        inputId  = "chromosome_labels",
        choices  = chromosomes,
        selected = chromosomes
      )
    })
    shiny::observeEvent(input$chromosome_labels_deselect, {
      shiny::updateCheckboxGroupInput(
        session  = session,
        inputId  = "chromosome_labels",
        choices  = levels(as.factor(load_block_dataframe()$chr)),
        selected = c()
      )
    })

    # ---- 3f. Render block dataframe
    output$block_dataframe <- shiny::renderTable(
      load_block_dataframe()
    )

    # ---- 4a. Load / Update simulations dataframe
    load_sims_dataframe <- shiny::reactive({
      shiny::validate(shiny::need(
        !is.null(sim_files[input$sim_pair, ]),
        "[Error]: Directory does not appear to contain any '.sims' \
         file for this pairwise comparison"
      ))
      grups.plots::load_simfile(
        path = sim_files[input$sim_pair, ]
      )
    })

    # ---- 4b. Output simulation labels checkbox group.
    output$simulation_labels_checkboxgroup <- shiny::renderUI({
      sim_labels <- levels(load_sims_dataframe()$label)
      grups.plots::shiny_reactive_checkbox(
        values   = sim_labels,
        title    = "Display Relationship Label(s):",
        input_id = "violin_labels",
        ncol     = 2
      )
    })

    output$stats_simlabel_input_list <- shiny::renderUI({
      sim_labels <- levels(load_sims_dataframe()$label)
      shiny::selectInput("stats_simlabel_input_list",
        label    = "Pedigree comparison label",
        choices  = rev(sim_labels)
      )
    })

    # ---- 4c. Render simulations violin plot
    output$sims_violinplot <- plotly::renderPlotly({
      grups.plots::plot_pedigree_sims(
        sims_dataframe = load_sims_dataframe(),
        results_data   = load_results_file(),
        pair           = input$sim_pair,
        labels_to_plot = input$violin_labels
      )
    })

    # ---- 4f. Reset User-selected chromosome if he used select/deselect All.
    shiny::observeEvent(input$violin_labels_select, {
      shiny::updateCheckboxGroupInput(
        session  = session,
        inputId  = "violin_labels",
        choices  = levels(load_sims_dataframe()$label),
        selected = levels(load_sims_dataframe()$label)
      )
    })

    shiny::observeEvent(input$violin_labels_deselect, {
      shiny::updateCheckboxGroupInput(
        session  = session,
        inputId  = "violin_labels",
        choices  = levels(load_sims_dataframe()$label),
        selected = c()
      )
    })

    # ---- 4g. Render simulations dataframe
    output$sims_dataframe <- shiny::renderTable(
      load_sims_dataframe()
    )

    # ---- Load svm probabilities
    shiny::observeEvent(input$sim_pair, {
      output$assigned_svm_probabilities <- DT::renderDataTable({
        probs <- load_svm_probs()
        DT::datatable(
          probs[which(probs$Pair_name == input$sim_pair), -1],
          style    = "bootstrap5",
          rownames = FALSE,
          options  = list(
              dom = "t",
              ordering = TRUE,
              scrollX = TRUE,
              width = "100%"
            )
        ) %>% DT::formatRound(
          columns = colnames(probs)[3:length(colnames(probs))],
          digits = 5
        )
      })
    })
    # ---- 4h. Render KS normality test
    output$ks_normality_test <- DT::renderDataTable(
      grups.plots::test_normality(
        sims_file = load_sims_dataframe(),
        alpha     = input$ks_alpha
      ) %>%
      DT::datatable(
        style   = "bootstrap5",
        options = list(dom      = "t",
                       ordering = FALSE,
                       scrollX  = TRUE,
                       width    = "100%"
                      )
      ) %>%
      DT::formatStyle(
        1:999,
        rows  = "p.val",
        color = DT::JS(
          paste("value  < ", input$ks_alpha, " ? 'red' : ''")
        )
      ),
      rownames = TRUE
    )

    # ---- 4i. Render BC matrix plot
    output$bc_matrix_plot <- plotly::renderPlotly(
      grups.plots::plot_bc_matrix(
        bc_matrix = grups.plots::get_bc_matrix(
          sims_data = load_sims_dataframe(),
          labels_to_keep = input$violin_labels
        ),
        plot_title = "<b>Matrix of Bhattacharya coefficients</b>",
        cutoff_values = c(-Inf, 0.01, 0.05, Inf),
        cutoff_labels = c("< 0.01", "< 0.05", ">= 0.05"),
        absolute_values = FALSE,
        marker_text = "<b>BC coefficient:</b>"
      )
    )

    # ---- 4j. Render  OR matrix
    output$plot_or_matrix <- plotly::renderPlotly({
      fig <- grups.plots::plot_bc_matrix(
        bc_matrix = grups.plots::get_odds_matrix(
                      sims_data        = load_sims_dataframe(),
                      observed_results = load_results_file(),
                      pair             = input$sim_pair,
                      labels_to_keep   = input$violin_labels
                    ),
        plot_title = "<b>Matrix of log(Odds Ratio)</b>",
        marker_text = NULL,
        cutoff_values = c(-Inf, log(1), log(100), Inf),
        cutoff_labels = c("<= 1/1", "<= 100/1 ", "> 100/1"),
        cutoff_colors = c("#CC6677", "#DDCC77", "#44AA99"),
        right_align = TRUE,
        absolute_values = TRUE
      )

      # Dirty trick to obtain a proper marker text annotation.
      fig[["x"]][["attrs"]][[1]][["text"]] <- ~paste(
        "<b>Odds Ratio:</b>", exp(value), "<br>",
        "<b>Log(OR):</b>", value
      )
      fig
    })

    # ---- 4k. Render OR confidence
    output$OR_confidence <- plotly::renderPlotly(
      grups.plots::plot_or_confidence(
        or_matrix = grups.plots::get_odds_matrix(
                      sims_data        = load_sims_dataframe(),
                      observed_results = load_results_file(),
                      pair             = input$sim_pair,
                      labels_to_keep = input$violin_labels
        ),
        predictor = load_results_file()[
          which(load_results_file()[, 1] == input$sim_pair), 2
        ]
      )
    )


    # ---- Stats
    stats_compute_deviation_event <- eventReactive(input$stats_compute_deviation_button, {
      prog_msg <- "Estimating average deviation from pedigree simulations. This may take a while..."
      progressr::withProgressShiny(message = prog_msg, value = 0, {
        progressor <- progressr::progressor(along = seq_along(sim_files$path))
        output <- grups.plots::compute_sim_deviation(
          sim_files     = sim_files,
          results_df    = load_results_file(),
          sample_regex  = sample_regex,
          label_request = (input$stats_simlabel_radio_button == 'A specific distribution'),
          metric        = input$stats_metric_select_input_list,
          subset        = input$stats_samples_select_input_list,
          simlabel      = input$stats_simlabel_input_list,
          progressor    = progressor
        )
        shinyFeedback::resetLoadingButton("stats_compute_deviation_button")
        output
      })
    })

    output$stats_compute_deviation_text_result <- renderText({
      stats_compute_deviation_event()
    })

    # ---- 5a. Render yaml config file
    output$config_file <- shiny::renderText(
      readChar(config_files[1], file.info(config_files[1])$size)
    )
  }

  options(shiny.autoreload = TRUE)

  shiny::shinyApp(ui, server, ...)
}
