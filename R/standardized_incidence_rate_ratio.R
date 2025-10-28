#' Age-standardized incidence rates ratio (asirr)
#'
#' @param ncan integer, (age-specific) number of cancers in the population of interest
#' @param py integer, (age-specific) person-year in the the population of interest
#' @param ncanref integer, (age-specific) number of cancers in the reference population
#' @param pyref integer, (age-specific) person-year in the the reference population
#' @param pystd numeric, (age-specific) standard population person-years (e.g. standard world population)
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#' @param py.min integer, minimum person-year required not to mask the CI's out
#'
#' Age-standardized incidence rate ratio (asirr) and associated 95% confidence interval are computing Armitage and Berry (1987) formula.
#' asird is a summary statistics that should be computed per group of individuals providing age specific counts.
#'
#' @return a 1 line and 3 column data.frame containing the pir (est) and associated 95% CI (lci, uci)
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
#' pystd <- 10:1
#' ncan.min <- 5
#' standardized_incidence_rate_ratio(ncan, py, ncanref, pyref, pystd, ncan.min)
#' standardized_incidence_rate_ratio(ncan, py, ncanref, pyref, pystd, sum(ncan) + 1)
standardized_incidence_rate_ratio <-
  function(ncan, py, ncanref, pyref, pystd, ncan.min = 5, py.min = 0) {
    est <- lci <- uci <- numeric()

    if (length(ncan)) {
      asr <- sum(ncan / py * pystd) / sum(pystd)
      var.asr <- sum(ncan / py * (pystd^2) * (1 - ncan / py) / py) /
        (sum(pystd)^2)

      asrref <- sum(ncanref / pyref * pystd) / sum(pystd)
      var.asrref <- sum(
        ncanref / pyref * (pystd^2) * (1 - ncanref / pyref) / pyref
      ) /
        (sum(pystd)^2)

      if (!is.na(asrref)) {
        if (asrref > 0) {
          est <- asr / asrref
          if (
            (sum(ncan, na.rm = TRUE) >= ncan.min) &
              (sum(py, na.rm = TRUE) >= py.min)
          ) {
            conf.level <- .95
            norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)
            var.est <- var.asr + var.asrref
            x <- (asr - asrref) / (sqrt(var.est))
            lci <- est^(1 - norm.pp / x)
            uci <- est^(1 + norm.pp / x)
          }
        }
      }
    }

    tibble(est = est, lci = lci, uci = uci)
  }
