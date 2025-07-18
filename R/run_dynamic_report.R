
#' Create a dynamic report from cancer RADAR output file
#'
#' @param filename.out file path, the path to a cancer RADAR output file
#'
#' This function will open a shiny app where cancer registries can visually check the data
#' they will be transmitted to IARC.
#'
#' @return nothing is returned
#'
#' @concept main
#'
#' @export
#'
run_dynamic_report <-
  function(filename.out = ''){
    ## ensure we are using full path to the file
    filename.out <- normalizePath(filename.out)
    if(!file.exists(filename.out)) stop(paste0(filename.out, ' does not exists'))
    rmarkdown::run(
      system.file('extdata/dynamic_report.Rmd', package = "cancerradarr"),
      render_args =
        list(
          params = list(
            filename.out = filename.out
          )
        )
    )
  }


