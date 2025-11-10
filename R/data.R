#' Countries label and countries codes
#'
#' A 2 column dataset containing all the countries of birth (with associated countries codes) included
#' in Cancer RADAR project
#'
#' @concept data
#' @format
#' A data frame with 251 rows and 3 columns:
#' \describe{
#'   \item{cob_label}{Country name}
#'   \item{cob_code}{Country code}
#'   \item{cob_iso3}{Country ISO3 code (used as unique id)}
#'   ...
#' }
"dat.cob"

#' Geographical aggregation used for cancerradarr
#'
#' In order to prevent loose of data in case of too low effective, several geographical aggregation
#' can be considered. In this table are stored the different level of aggregation and the aggregation
#' correspondence table considered.
#'
#' A multi-columns dataset containing all the countries of birth (as ISO3 code) and other geographical
#' aggregation rules
#'
#' @concept data
#' @format
#' A data frame with 250 rows and 5 columns:
#' \describe{
#'   \item{cob_iso3}{Country ISO3 code}
#'   \item{un_region}{UN region}
#'   \item{un_subregion}{UN subregion}
#'   \item{hdi_cat}{HDI 2023 category}
#'   \item{any_migr}{any migration background}
#'   ...
#' }
"dat.aggr"

#' Burden of cancer aggregation category used for cancerradarr
#'
#'
#' A multi-columns dataset containing for all the countries of birth (as ISO3 code), sex and cancer type
#' combinations the quartiles of cancer burden in country of origin burden. The quartiles (`0%-24%`,
#' `25%-49%`, `50%-74%` and `75-100%`) are based on the ASIR from GLOBOCAN 2022.
#'
#' @concept data
#' @format
#' A data frame with 2,220 rows and 5 columns:
#' \describe{
#'   \item{cob_iso3}{Country ISO3 code}
#'   \item{sex}{targeted sex}
#'   \item{can}{the cancer type}
#'   \item{asr}{GLOBOCAN 2022 age-standardized cancer incidence rate}
#'   \item{asr_rank_cat}{GLOBOCAN 2022 age-standardized cancer incidence rate quartile category}
#'   ...
#' }
"dat.asr.cat"

#' European countries age-specific cancer burden from GLOBOCAN 2022
#'
#'
#' A multi-columns dataset containing for all the European countries (UN definition) (as ISO3 code), sex and cancer type
#' combinations the number of cases and population at risk estimated in GLOBOCAN 2022. This data
#' are used in `cancerradarr` to compute the relative index on a standard reference population that
#' could be more easily compared between registries.
#' In addition to individual European countries, aggregated areas such as
#' E27 (European Union 27 countries) and EUN (all the UN European countries)
#' are stred in the dataset
#'
#' @concept data
#' @format
#' A data frame with 6,384 rows and 6 columns:
#' \describe{
#'   \item{cob_iso3}{Country ISO3 code}
#'   \item{sex}{targeted sex}
#'   \item{ageg}{targeted age group}
#'   \item{can}{the cancer type}
#'   \item{ncanref}{number of cancer cases estimated in GLOBOCAN 2022}
#'   \item{pyref}{population at risk estimated in GLOBOCAN 2022}
#'   ...
#' }
#' @source https://gco.iarc.fr/today/en
#' @references Bray F, Laversanne M, Sung H, Ferlay J, Siegel RL, Soerjomataram I, Jemal A. Global cancer statistics 2022: GLOBOCAN estimates of incidence and mortality worldwide for 36 cancers in 185 countries. CA Cancer J Clin. 2024 May-Jun;74(3):229-263. doi: 10.3322/caac.21834. Epub 2024 Apr 4. PMID: 38572751.
"globocan.2022.eu"

#
# #' Example of Cancer RADAR filled input data
# #'
# #' A .xlsx fille generated with create_registry_input_file() and filled with fake data to be
# #' used in examples.
# #'
# #'
# #' @format ## `ex_cancerRADAR_input_filled.xlsx"`
# #' A data frame with 185 rows and 60 columns:
# #' \describe{
# #'   \item{cob_label}{Country name}
# #'   \item{cob_code}{Country code}
# #'   ...
# #' }
# "ex_cancerRADAR_input_filled"
#

#' Pre-computed chopped combination for vectors size 1 to 18
#'
#' This is a list containing all the possible combination of slices to chop
#' vectors of size 1 to 18. It is useful to compute custom age group
#' aggregation to ensure we are not disclosing age group with too few cancer
#' cases.
#'
#' @concept data
#' @format
#' A 18 item list:
#' \describe{
#'   each element is a matrix containing all the possible chop combinations
#'   to aggregate a vector of size n.
#'   ...
#' }
"chopped.vector.list"
