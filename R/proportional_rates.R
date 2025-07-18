#' Compute proportional rates
#'
#' @param ncan integer, number of cancer of interest
#' @param ntot integer, overal number of cancer
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Proportional incidence rates and associated 95% confidence interval are computing assuming a Binomial distribution and the Clopper and Pearson (1934) procedure.
#'
#' @return a 3 column data.frame containing the proportional incidence rate estimate (est) and associated 95% CI (lci, uci)
#' @export
#'
#' @references C. J. CLOPPER, B.Sc., E. S. PEARSON, D.Sc., THE USE OF CONFIDENCE OR FIDUCIAL LIMITS ILLUSTRATED IN THE CASE OF THE BINOMIAL, Biometrika, Volume 26, Issue 4, December 1934, Pages 404–413, https://doi.org/10.1093/biomet/26.4.404
#' @references  Boyle P, Parkin DM. Cancer registration: principles and methods. Statistical methods for registries. IARC Sci Publ. 1991;(95):126-58. PMID: 1894318.
#'
#' @seealso [stats::binom.test()]
#'
#' @concept summary-stat
#'
#' @examples
#' ncan <- c(1, 10, 100)
#' ntot <- c(10, 100, 1000)
#' proportional_rates(ncan, ntot, 5)
proportional_rates <-
  function(ncan, ntot, ncan.min = 5){
    conf.level <- 0.95
    xnc <- cbind(ncan, ntot, conf.level)
    est <- lci <- uci <- rep(NA, nrow(xnc))

    for (i in 1:nrow(xnc)) {
      if((!is.na(xnc[i, 1])) & (!is.na(xnc[i, 2]))){
        if((xnc[i, 2] > 0) & (xnc[i, 1] <= xnc[i, 2])) {
          ci <- stats::binom.test(x = xnc[i, 1], n = xnc[i, 2], conf.level = xnc[i, 3])$conf.int
          est[i] <- xnc[i, 1] / xnc[i, 2]
          lci[i] <- ci[1]
          uci[i] <- ci[2]
        }
      }
    }

    tibble(est = est, lci = lci, uci = uci) %>%
      mutate(
        lci = replace(.data$lci, ncan < ncan.min, NA),
        uci = replace(.data$uci, ncan < ncan.min, NA)
      )
  }
