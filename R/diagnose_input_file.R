#' Diagnose Input File for Data Quality Issues
#'
#' This function checks the input file for common data quality issues that can
#' cause errors during processing, including duplicate age groups and unexpected
#' age group labels.
#'
#' @param filename.in file path, the file containing the 5 years age counts of
#'   cancers stratified per cancer type, sex and country of birth
#'
#' @return A list containing diagnostic information:
#'   \itemize{
#'     \item duplicate_age_groups: Data frame of combinations with duplicate age groups
#'     \item invalid_age_groups: Character vector of invalid age group labels found
#'     \item summary: Overall summary of issues found
#'   }
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_subset str_split_i str_remove
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#'   file.in <- system.file("extdata", "ex_cancerRADAR_input_filled.xlsx",
#'                          package = "cancerradarr")
#'   diagnosis <- diagnose_input_file(file.in)
#'   print(diagnosis$summary)
#' }
diagnose_input_file <- function(filename.in) {
  cat('\n> Reading', filename.in, '\n')

  ## Define expected age groups
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
    '85',
    'total',
    'DCO',
    'MV'
  )

  ## Read the input file
  sheets.names <- openxlsx::getSheetNames(filename.in) |>
    setdiff(c('readme', 'data_info'))

  common.vars <- c("cob_iso3")

  ## Read person-years data
  cat('> Checking person-years sheets...\n')
  pop.migr.names <- sheets.names |> stringr::str_subset('py_.*')
  dat.py <-
    purrr::map_dfr(
      pop.migr.names,
      ~ {
        openxlsx::read.xlsx(filename.in, .x) |>
          select(c(all_of(common.vars), starts_with('py'))) |>
          pivot_longer(
            cols = -all_of(common.vars),
            names_to = 'py_label',
            values_to = 'py'
          ) |>
          mutate(
            sex = str_split_i(.data$py_label, '_', 2),
            ageg = str_remove(.data$py_label, '.*male_'),
            sheet = .x
          )
      }
    ) |>
    mutate(ageg = .data$ageg |> replace(.data$ageg == 'tot', 'total'))

  ## Read cancer data
  cat('> Checking cancer case sheets...\n')
  can.migr.names <- sheets.names |>
    str_subset('ncx_.*|nliv_.*|nstm_.*|nbrea_.*|ncolo_.*|nlun_.*|nallC_.*')
  dat.can <-
    purrr::map_dfr(
      can.migr.names,
      ~ {
        openxlsx::read.xlsx(filename.in, .x) |>
          select(c(all_of(common.vars), starts_with('n'))) |>
          pivot_longer(
            cols = -all_of(common.vars),
            names_to = 'ncan_label',
            values_to = 'ncan'
          ) |>
          mutate(
            sex = str_split_i(.data$ncan_label, '_', 2),
            ageg = str_remove(.data$ncan_label, '.*male_'),
            can = str_split_i(.data$ncan_label, '_', 1) |> str_remove('^n'),
            sheet = .x
          )
      }
    )

  ## Combine all data
  all_data <- bind_rows(
    dat.py |> mutate(data_type = 'person-years'),
    dat.can |> mutate(data_type = 'cancer cases')
  )

  ## Check for invalid age groups
  cat('> Checking for invalid age group labels...\n')
  invalid_ageg <- setdiff(unique(all_data$ageg), expected_ageg)

  ## Check for duplicate age groups within combinations
  cat('> Checking for duplicate age groups...\n')
  duplicates <- all_data |>
    group_by(across(all_of(c(
      'cob_iso3',
      'sex',
      'ageg',
      'data_type',
      'sheet'
    )))) |>
    summarise(n_occurrences = n(), .groups = 'drop') |>
    filter(.data$n_occurrences > 1)

  ## Check for duplicate age groups in cancer data by cancer type
  cancer_duplicates <- dat.can |>
    group_by(across(all_of(c('cob_iso3', 'sex', 'can', 'ageg')))) |>
    summarise(n_occurrences = n(), .groups = 'drop') |>
    filter(.data$n_occurrences > 1)

  ## Generate summary
  cat('\n=== DIAGNOSIS SUMMARY ===\n')

  issues_found <- FALSE

  if (length(invalid_ageg) > 0) {
    issues_found <- TRUE
    cat('\n[ERROR] Invalid age group labels found:\n')
    cat('  ', paste(invalid_ageg, collapse = ', '), '\n')
    cat('  Expected format: XX_YY (e.g., "20_24", "30_34", "85")\n')
    cat('  Also accepted: "total", "DCO", "MV"\n')
  }

  if (nrow(duplicates) > 0) {
    issues_found <- TRUE
    cat('\n[ERROR] Duplicate age groups found in the following combinations:\n')
    print(duplicates, n = 20)
  }

  if (nrow(cancer_duplicates) > 0) {
    issues_found <- TRUE
    cat('\n[ERROR] Duplicate age groups in cancer data:\n')
    print(cancer_duplicates, n = 20)
  }

  if (!issues_found) {
    cat('\n[OK] No data quality issues detected!\n')
    cat('Your input file appears to be properly formatted.\n')
  } else {
    cat('\n[ACTION REQUIRED] Please fix the issues above in your input file.\n')
    cat('Common causes:\n')
    cat(
      '  - Extra spaces in column names (e.g., "20_24 " instead of "20_24")\n'
    )
    cat('  - Typos in age group labels\n')
    cat('  - Duplicate columns in Excel sheets\n')
    cat('  - Data copied incorrectly between sheets\n')
  }

  cat('\n========================\n\n')

  invisible(list(
    duplicate_age_groups = duplicates,
    duplicate_cancer_age_groups = cancer_duplicates,
    invalid_age_groups = invalid_ageg,
    all_age_groups = sort(unique(all_data$ageg)),
    summary = list(
      total_issues = length(invalid_ageg) +
        nrow(duplicates) +
        nrow(cancer_duplicates),
      has_invalid_labels = length(invalid_ageg) > 0,
      has_duplicates = nrow(duplicates) > 0,
      has_cancer_duplicates = nrow(cancer_duplicates) > 0
    )
  ))
}
