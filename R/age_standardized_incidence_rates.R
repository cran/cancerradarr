#' Age-standardized incidence rate (asir)
#'
#' @param ncan integer, (age-specific) number of cancers in the population of interest
#' @param py integer, (age-specific) person-year in the the population of interest
#' @param pystd numeric, (age-specific) standard population person-years (e.g. standard world population)
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#' @param py.min integer, minimum person-years required not to mask the CI's out
#'
#' Age-standardized incidence rate (asir) and associated 95% confidence interval are computing assuming normal distribution of the asir.
#' asir is a summary statistics that should be computed per group of individuals providing age specific counts.
#'
#' @return a 1 line and 3 column data.frame containing the asir (est) and associated 95% CI (lci, uci)
#' @export
#'
#' @references  Boyle P, Parkin DM. Cancer registration: principles and methods. Statistical methods for registries. IARC Sci Publ. 1991;(95):126-58. PMID: 1894318.
#'
#' @concept summary-stat
#'
#' @examples
#' ncan <- 1:10
#' py <- 101:110
#' pystd <- 10:1
#' ncan.min <- 5
#' age_standardized_incidence_rates(ncan, py, pystd, ncan.min)
#' age_standardized_incidence_rates(ncan, py, pystd, sum(ncan) + 1)
age_standardized_incidence_rates <-
  function(ncan, py, pystd, ncan.min = 5, py.min = 0) {
    # ## Fay 1998 version with exact confidence interval calculation (based on gamma distribution)
    # epitools::ageadjust.direct(ncan, py, stdpop = pystd) |>
    #   t() |>
    #   as_tibble() |>
    #   select(est = adj.rate, lci = lci, uci = uci)

    est <- lci <- uci <- NA
    if (sum(pystd, na.rm = TRUE) > 0) {
      est <- sum(ncan / py * pystd, na.rm = TRUE) / sum(pystd, na.rm = TRUE)
      if (
        (sum(ncan, na.rm = TRUE) >= ncan.min) &
          (sum(py, na.rm = TRUE) >= py.min)
      ) {
        conf.level <- .95
        norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)
        var.est <- sum(ncan / py * (pystd^2) * (1 - ncan / py) / py) /
          (sum(pystd)^2)
        lci <- pmax(est - norm.pp * sqrt(var.est), 0)
        uci <- pmax(est + norm.pp * sqrt(var.est), 0)
      }
    }
    tibble(est = est, lci = lci, uci = uci)
  }
