#' Compute crude incidence rates
#'
#' @param ncan integer, number of cancer
#' @param py integer, number of person-year
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Crude incidence rates and associated 95% confidence interval are computing assuming a Poisson distribution and the exact method.
#'
#' @seealso [epitools::pois.exact()]
#' @return a 3 column data.frame containing the crude incidence rate estimate (est) and associated 95% CI (lci, uci)
#' @export
#'
#' @references  Boyle P, Parkin DM. Cancer registration: principles and methods. Statistical methods for registries. IARC Sci Publ. 1991;(95):126-58. PMID: 1894318.
#'
#' @concept summary-stat
#'
#' @examples
#' ncan <- c(1, 10, 100)
#' py <- c(10, 100, 1000)
#' incidence_rates(ncan, py, 5)
incidence_rates <-
  function(ncan, py, ncan.min = 5){
    if(length(ncan)){
      epitools::pois.exact(ncan, py) %>%
        as_tibble() %>%
        select('est' = 'rate', 'lci' = 'lower', 'uci' = 'upper') %>%
        mutate(
          lci = replace(.data$lci, ncan < ncan.min, NA),
          uci = replace(.data$uci, ncan < ncan.min, NA)
        )
    } else {
      tibble(est = numeric(), lci = numeric(), uci = numeric(), .rows = 0)
    }
  }

