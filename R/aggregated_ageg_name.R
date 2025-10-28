#' Compute the aggregated age group names from a vector of
#' more detailed age groups
#'
#' @param selected.ageg character, the fine grain age group vector
#' @param ageg.sep character, the ageg group separator character
#'
#' @return character, the name of the aggregated age group
#' @export
#'
#' @examples
#' ageg.in <- c('15_19', '20_24', '25_29')
#' aggregated_ageg_name(ageg.in)
#'
aggregated_ageg_name <-
  function(selected.ageg, ageg.sep = '_') {
    a.start <- selected.ageg |>
      utils::head(1) |>
      stringr::str_remove(paste0(ageg.sep, '.*'))
    a.stop <- selected.ageg |>
      utils::tail(1) |>
      stringr::str_remove(paste0('.*', ageg.sep))
    if (a.start != a.stop) {
      paste0(a.start, '_', a.stop)
    } else {
      a.start
    }
  }
