#' Read cancer registry summary statistics (non age-specific)
#'
#' @param filename.out file path, the path to a cancer RADAR output file
#' @param aggr.level character, the aggregation level to be considered. Should be one of `cob_iso3`
#'                   (country of birth; smaller unit), `un_region` (UN region), `un_subergion` (
#'                   UN sub-region), `hdi_cat` (HDI category), `asr_rank_cat` (cancer burden category
#'                   in the country of birth), `any_migr` (any migration background)
#'
#' @return a tibble with 9 columns
#'  - reg_label: factor, the chosen aggregation level id
#'  - sex: character, male/female
#'  - ageg: character, age group (here `total`)
#'  - can: character, the cancer type
#'  - ref: character, the reference population for relative index
#'  - index: character, the type of index
#'  - est: dbl, the index estimator
#'  - lci: dbl, the index confidence interval lower bound
#'  - uci: dbl, the index confidence interval upper bound
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
#' dat.out <- read_cancerradar_output_01(filename.out, 'un_region')
#' head(dat.out)
#'
#'
read_cancerradar_output_01 <-
  function(filename.out, aggr.level = 'cob_iso3') {
    wb <- openxlsx::loadWorkbook(filename.out)
    sheets.list <- openxlsx::sheets(wb)

    if (any(stringr::str_detect(sheets.list, paste0(aggr.level, '$')))) {
      dat.migr.count <-
        openxlsx::read.xlsx(
          wb,
          paste0('count_migr_', aggr.level),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename('n' = 'n_tot') |>
        rename_with(~'reg_label', any_of(aggr.level)) |>
        mutate(py = as.numeric(.data$py), n = as.numeric(.data$n)) |>
        filter(!is.na(.data$reg_label))

      dat.migr.other <-
        openxlsx::read.xlsx(
          wb,
          paste0('other_migr_', aggr.level),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename_with(~'reg_label', any_of(aggr.level)) |>
        filter(!is.na(.data$reg_label)) |>
        mutate(across(
          c(
            starts_with('cir'),
            starts_with('asir'),
            starts_with('sir'),
            starts_with('pir')
          ),
          ~ as.numeric(.x)
        )) |>
        mutate(across(
          c(any_of(c('reg_label', 'sex', 'ageg', 'can'))),
          ~ as.character(.x)
        ))

      dat.ref.count <-
        openxlsx::read.xlsx(
          wb,
          paste0('count_gen'),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename('n' = 'n_tot') |>
        rename_with(~'reg_label', any_of('population')) |>
        mutate(py = as.numeric(.data$py), n = as.numeric(.data$n)) |>
        filter(!is.na(.data$reg_label))

      dat.ref.other <-
        openxlsx::read.xlsx(
          wb,
          paste0('other_gen'),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename_with(~'reg_label', any_of('population')) |>
        filter(!is.na(.data$reg_label)) |>
        mutate(across(
          c(starts_with('cir'), starts_with('asir')),
          ~ as.numeric(.x)
        )) |>
        mutate(across(
          c(any_of(c('reg_label', 'sex', 'ageg', 'can'))),
          ~ as.character(.x)
        ))

      dat.01 <-
        bind_rows(
          left_join(
            dat.migr.count,
            dat.migr.other,
            by = c('reg_label', 'sex', 'ageg', 'can')
          ),
          left_join(
            dat.ref.count,
            dat.ref.other,
            by = c('reg_label', 'sex', 'ageg', 'can')
          )
        ) |>
        mutate(
          reg_label = factor(
            .data$reg_label,
            levels = c(
              'general population',
              gtools::mixedsort(unique(dat.migr.count$reg_label))
            )
          )
        ) |>
        filter(!is.na(.data$reg_label))

      common.vars <- c('reg_label', 'sex', 'ageg', 'can')
      index.name.abs <- c('cir', 'asir', 'py', 'n', 'n_mv', 'n_dco')
      index.names.rel.reg <- c('cirr', 'cird', 'asirr', 'asird', 'sir', 'pir')
      index.names.rel.glonat <- paste0(index.names.rel.reg, '_glonat')
      index.names.rel.gloeu27 <- paste0(index.names.rel.reg, '_gloeu27')
      index.names.rel.gloeuUN <- paste0(index.names.rel.reg, '_gloeuUN')
      index.names <- c(
        index.name.abs,
        index.names.rel.reg,
        index.names.rel.glonat,
        index.names.rel.gloeu27,
        index.names.rel.gloeuUN
      )

      dat.est <-
        dat.01 |>
        select(any_of(c(common.vars, index.names))) |>
        pivot_longer(
          !any_of(common.vars),
          names_to = 'index',
          values_to = 'est'
        )

      dat.lci <-
        dat.01 |>
        select(any_of(c(common.vars, paste0(index.names, '_lci')))) |>
        pivot_longer(
          !any_of(common.vars),
          names_to = 'index',
          values_to = 'lci'
        ) |>
        mutate(index = stringr::str_remove(.data$index, '_lci$'))

      dat.uci <-
        dat.01 |>
        select(any_of(c(common.vars, paste0(index.names, '_uci')))) |>
        pivot_longer(
          !any_of(common.vars),
          names_to = 'index',
          values_to = 'uci'
        ) |>
        mutate(index = stringr::str_remove(.data$index, '_uci$'))

      dat <-
        dat.est |>
        left_join(dat.lci, by = c(common.vars, 'index')) |>
        left_join(dat.uci, by = c(common.vars, 'index')) |>
        mutate(
          ref = rep('registry', n()) |>
            replace(
              stringr::str_detect(.data$index, '_glonat'),
              'globocan national'
            ) |>
            replace(
              stringr::str_detect(.data$index, '_gloeu27'),
              'globocan eu27'
            ) |>
            replace(
              stringr::str_detect(.data$index, '_gloeuUN'),
              'globocan euUN'
            ),
          index = .data$index |>
            stringr::str_remove('_glonat|_gloeu27|_gloeuUN'),
          .after = 'can'
        )
    } else {
      dat <-
        tibble(
          reg_label = factor(),
          sex = character(),
          ageg = character(),
          can = character(),
          ref = character(),
          index = character(),
          est = numeric(),
          lci = numeric(),
          uci = numeric()
        )
    }
    dat
  }
