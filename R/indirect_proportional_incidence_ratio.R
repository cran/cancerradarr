#' Compute the indirect proportional incidence ratio (pir)
#'
#' @param ncan integer, (age-specific) number of cancers in the population of interest
#' @param ntot integer, (age-specific) total number of cancer the the population of interest
#' @param ncanref integer, (age-specific) number of cancers in the reference population
#' @param ntotref integer, (age-specific) total number of cancer the the reference of interest
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Indirect proportional incidence ratio and associated 95% confidence interval are computing assuming normal distribution of the pir on the log scale.
#' pir is a summary statistics that should be computed per group of individuals providing age specific counts.
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
#' ntot <- 11:20
#' ncanref <- 41:50
#' ntotref <- 251:260
#' ncan.min <- 5
#'
#' indirect_proportional_incidence_ratio(ncan, ntot, ncanref, ntotref, ncan.min)
#' indirect_proportional_incidence_ratio(ncan, ntot, ncanref, ntotref, sum(ncan) + 1)
indirect_proportional_incidence_ratio <-
  function(ncan, ntot, ncanref, ntotref, ncan.min = 5) {
    est <- lci <- uci <- NA
    n.obs <- sum(ncan, na.rm = TRUE)
    n.exp <- sum(ntot * ncanref / ntotref, na.rm = TRUE)
    if (n.exp > 0) {
      est <- n.obs / n.exp
      if ((n.obs >= ncan.min) & all(ntot >= ncan)) {
        conf.level <- .95
        norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)
        se.log.est <- sqrt(sum(ncan * (ntot - ncan) / ntot, na.rm = TRUE)) /
          n.obs
        lci <- exp(log(est) - norm.pp * se.log.est)
        uci <- exp(log(est) + norm.pp * se.log.est)
      }
    }
    tibble(est = est, lci = lci, uci = uci)
  }
