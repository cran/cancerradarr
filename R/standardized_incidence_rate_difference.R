#' Age-standardized incidence rates differences (asird)
#'
#' @param ncan integer, (age-specific) number of cancers in the population of interest
#' @param py integer, (age-specific) person-year in the the population of interest
#' @param ncanref integer, (age-specific) number of cancers in the reference population
#' @param pyref integer, (age-specific) person-year in the the reference population
#' @param pystd numeric, (age-specific) standard population person-years (e.g. standard world population)
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Age-standardized incidence rate difference (asird) is computed without confidence interval estimation for now.
#' asird is a summary statistics that should be computed per group of individuals providing age specific counts.
#'
#' @references https://www.hsph.harvard.edu/thegeocodingproject/analytic-methods/
#' @return a 1 line and 3 column data.frame containing the pir (est) and associated 95% CI (lci, uci)
#' @export
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
#' standardized_incidence_rate_difference(ncan, py, ncanref, pyref, pystd, ncan.min)
#' standardized_incidence_rate_difference(ncan, py, ncanref, pyref, pystd, sum(ncan) + 1)
standardized_incidence_rate_difference <-
  function(ncan, py, ncanref, pyref, pystd, ncan.min = 5){
    est <- lci <- uci <- NA
    
    w <- pystd / sum(pystd, na.rm = TRUE)
    
    asr <- sum(ncan / py * w) 
    var.asr <- sum(ncan / (py ^ 2) * (w ^ 2)) / (sum(w) ^ 2)
    
    asrref <- sum(ncanref / pyref * w) 
    var.asrref <- sum(ncanref / (pyref ^ 2) * (w ^ 2)) / (sum(w) ^ 2)

    est <- asr - asrref
    var.est <- var.asr + var.asrref

    if(sum(ncan, na.rm = TRUE) >= ncan.min){
      conf.level <- .95
      norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)
      lci <- est - norm.pp * sqrt(var.est)
      uci <- est + norm.pp * sqrt(var.est)
    }
    tibble(est = est, lci = lci, uci = uci)
  }
