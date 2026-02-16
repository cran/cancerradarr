#' Validate Age Group Labels
#'
#' Checks that age group labels in a dataset match expected formats.
#'
#' @param dat A data frame containing an `ageg` column with age group labels.
#'
#' @return NULL (invisibly). The function is called for its side effect of
#'   stopping execution if invalid age groups are found.
#'
#' @details
#' Expected age group labels are:
#' - Five-year age bands: '00_04', '05_09', ..., '80_84'
#' - Open-ended group: '85'
#' - Special categories: 'total', 'DCO', 'MV'
#'
#' @export
#'
#' @examples
#' dat <- data.frame(ageg = c("00_04", "05_09", "10_14"))
#' validate_age_groups(dat)
validate_age_groups <- function(dat) {
  expected_ageg <- c(
    '00_04',
    '05_09',
    '10_14',
    '15_19',
    '20_24',
    '25_29',
    '30_34',
    '35_39',
    '40_44',
    '45_49',
    '50_54',
    '55_59',
    '60_64',
    '65_69',
    '70_74',
    '75_79',
    '80_84',
    '85'
  )

  invalid_ageg <- setdiff(
    unique(dat$ageg),
    c(expected_ageg, 'total', 'DCO', 'MV')
  )

  if (length(invalid_ageg) > 0) {
    stop(
      "Invalid age group labels found: ",
      paste(invalid_ageg, collapse = ", "),
      "\nExpected format: XX_YY (e.g., '20_24', '30_34', '85')"
    )
  }
}
