#' Create a static report from cancer RADAR output file
#'
#' @param filename.out file path, the path to a cancer RADAR output file
#'
#' This function will create a html report that could be useful to check the data
#' that will be transmitted to IARC.
#'
#' @return nothing is returned, but a html file created with some summary statistics and graphs
#' out of the file that should be transmitted with IARC
#'
#'
#' @export
#'
create_static_report <-
  function(filename.out = '') {
    if (!file.exists(filename.out)) {
      stop(paste0(filename.out, ' does not exists'))
    }
    ## ensure we are using full path to the file
    filename.out <- normalizePath(filename.out)
    rmarkdown::render(
      system.file('extdata/static_report.Rmd', package = "cancerradarr"),
      output_file = filename.out |> stringr::str_replace('.xlsx$', '.html'),
      params = list(
        filename.out = filename.out
      ),
      output_dir = dirname(filename.out),
      knit_root_dir = dirname(filename.out),
      intermediates_dir = dirname(filename.out)
    )
  }
