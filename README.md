# grups.plots

`grups.plots` is a companion R package providing with various interactive visualizations, utilities and summary statistics to analyse the results of [`GRUPS-rs`](https://github.com/MaelLefeuvre/grups-rs)

## Installation

```bash
R --slave -e 'devtools::install_github("https://github.com:MaelLefeuvre/grups.plots", upgrade="always")'
```

## Usage

### Running the `grups.plots` shiny application

Simply use the `grups.plots::app()` function from an interactive session, by targeting any directory containing the output results of `grups-rs pedigree-sims`
```r
library(grups.plots)
grups.plots::app(data_dir = "./grups-output")
```

or directly launch the app from the terminal, by running R in `--slave` mode
```bash
R --slave -e 'grups.plots::app(data_dir="./grups-output")'
```



### Getting help
Just like any other R package:
- Typing `?grups.plots::app` within an interactive R session should provide documentation for the main shinyApp of `grups.plots`
- Typing `help(package = grups.plots)` within an interactive R session should output a documented list of every public function of `grups.plots` (Note that every exported function of `grups.plots` is documented, and may be printed out using the `help()` function, or the `?` documentation shortcut, as in `?grups.plots::<function-name>`)

