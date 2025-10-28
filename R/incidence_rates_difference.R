#' Compute incidence rates difference
#'
#' @param ncan integer, number of cancers in the population of interest
#' @param py integer, person-year of the the population of interest
#' @param ncanref integer, number of cancers in the reference population
#' @param pyref integer, person-year of the the reference population
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#' @param py.min integer, minimum person-year required not to mask the CI's out
#'
#' Incidence rates differences and associated 95% confidence interval are computing assuming normal distribution of the differences..
#'
#' @return a 3 column data.frame containing the incidence rates difference (est) and associated 95% CI (lci, uci)
#' @export
#'
#' @concept summary-stat
#'
#' @examples
#' ncan <- 1:10
#' py <- 101:110
#' ncanref <- 41:50
#' pyref <- 251:260
#' ncan.min <- 5
#'
#' incidence_rates_difference(ncan, py, ncanref, pyref, ncan.min)
incidence_rates_difference <-
  function(ncan, py, ncanref, pyref, ncan.min = 5, py.min = 0) {
    est <- lci <- uci <- NA
    if (sum(ncan, na.rm = TRUE) >= ncan.min) {
      conf.level <- .95
      norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)

      est <- (ncan / py) - (ncanref / pyref)
      var.est <- ncan / (py^2) + ncanref / (pyref^2)
      lci <- est - norm.pp * sqrt(var.est)
      uci <- est + norm.pp * sqrt(var.est)
    }
    tibble(est = est, lci = lci, uci = uci) |>
      mutate(
        lci = replace(.data$lci, ncan < ncan.min | py < py.min, NA),
        uci = replace(.data$uci, ncan < ncan.min | py < py.min, NA)
      )
  }
