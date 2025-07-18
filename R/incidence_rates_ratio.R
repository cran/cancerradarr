#' Compute incidence rates ratio
#'
#' @param ncan integer, number of cancers in the population of interest
#' @param py integer, person-year of the the population of interest
#' @param ncanref integer, number of cancers in the reference population
#' @param pyref integer, person-year of the the reference population
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Incidence rates ratio and associated 95% confidence interval are computing assuming normal distribution of the ratios on the log scale.
#'
#' @return a 3 column data.frame containing the incidence rates ratio (est) and associated 95% CI (lci, uci)
#' @export
#'
#' @references  Boyle P, Parkin DM. Cancer registration: principles and methods. Statistical methods for registries. IARC Sci Publ. 1991;(95):126-58. PMID: 1894318.
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
#' incidence_rates_ratio(ncan, py, ncanref, pyref, ncan.min)
incidence_rates_ratio <-
  function(ncan, py, ncanref, pyref, ncan.min = 5){
    est <- lci <- uci <- NA
    if(sum(ncan, na.rm = TRUE) >= ncan.min){
      conf.level <- .95
      norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)

      est <- (ncan / py) / (ncanref / pyref)
      var.est <- 1 / ncan + 1 / ncanref
      lci <- est * exp(- norm.pp * sqrt(var.est))
      uci <- est * exp(norm.pp * sqrt(var.est))
    }
    tibble(est = est, lci = lci, uci = uci) %>%
      mutate(
        lci = replace(.data$lci, ncan < ncan.min, NA),
        uci = replace(.data$uci, ncan < ncan.min, NA)
      )
  }
