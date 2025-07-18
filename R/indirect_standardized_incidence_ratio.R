#' Compute indirect standardized incidence ratio (sir)
#'
#' @param ncan integer, (age-specific) number of cancers in the population of interest
#' @param py integer, (age-specific) person-year of the the population of interest
#' @param ncanref integer, (age-specific) number of cancers in the reference population
#' @param pyref integer, (age-specific) person-year of the the reference population
#' @param ncan.min integer, minimum number of observation required not to mask the CI's out
#'
#' Standardized incidence ratio (sir) and associated 95% confidence interval are computing assuming normal distribution of the pir on the log scale.
#' sir is a summary statistics that should be computed per group of individuals providing age specific counts.
#'
#' @return a 1 line and 3 column data.frame containing the sir (est) and associated 95% CI (lci, uci)
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
#' indirect_standardized_incidence_ratio(ncan, py, ncanref, pyref, ncan.min)
#' indirect_standardized_incidence_ratio(ncan, py, ncanref, pyref, sum(ncan) + 1)
indirect_standardized_incidence_ratio <-
  function(ncan, py, ncanref, pyref, ncan.min = 5){
    # ## Fay 1998 version with exact confidence interval calculation (based on gamma distribution)
    # epitools::ageadjust.indirect(ncan, py, ncanref, pyref) %>%
    #   extract2('sir') %>%
    #   t() %>%
    #   as_tibble() %>%
    #   select(est = sir, lci = lci, uci = uci)
    est <- lci <- uci <- NA

    n.obs <- sum(ncan, na.rm = TRUE)
    n.exp <- sum(py * ncanref / pyref)

    if(!is.na(n.exp)){
      if(n.exp > 0){
        est <- n.obs / n.exp
        if(n.obs >= ncan.min){
          conf.level <- .95
          norm.pp <- stats::qnorm(1 - (1 - conf.level) / 2)
          var.est <- n.obs / (n.exp ^ 2)
          lci <- est - norm.pp * sqrt(var.est)
          uci <- est + norm.pp * sqrt(var.est)
        }
      }
    }

    tibble(est = est, lci = lci, uci = uci)
  }
