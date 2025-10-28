#' Read cancer registry summary statistics (age-specific incidence rate and proportional rates)
#'
#' @param filename.out file path, the path to a cancer RADAR output file
#' @param aggr.level character, the aggregation level to be considered. Should be one of `cob_iso3`
#'                   (country of birth; smaller unit), `un_region` (UN region), `un_subergion` (
#'                   UN sub-region), `hdi_cat` (HDI category), `asr_rank_cat` (cancer burden category
#'                   in the country of birth), `any_migr` (any migration background)
#'
#' @return a tibble with 11 columns
#'  - reg_label: factor, the chosen aggregation level id
#'  - sex: character, male/female
#'  - ageg: character, age group (here `total`)
#'  - can: character, the cancer type
#'  - index: character, the type of index
#'  - est: dbl, the index estimator
#'  - lci: dbl, the index confidence interval lower bound
#'  - uci: dbl, the index confidence interval upper bound
#'  - ageg_sta: dbl, the age group starting age
#'  - ageg_sto: dbl, the age group stopping age
#'  - ageg_mid: dbl, the age group middle age
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
#' dat.out <- read_cancerradar_output_02(filename.out, 'un_region')
#' head(dat.out)
#'
#'

read_cancerradar_output_02 <-
  function(filename.out, aggr.level = 'cob_iso3') {
    wb <- openxlsx::loadWorkbook(filename.out)
    sheets.list <- openxlsx::sheets(wb)

    if (any(stringr::str_detect(sheets.list, paste0(aggr.level, '$')))) {
      dat.migr.ir.pr <-
        openxlsx::read.xlsx(
          wb,
          paste0('ir_pr_migr_aggr_', aggr.level),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename_with(~'reg_label', any_of(aggr.level))

      dat.gen.ir.pr <-
        openxlsx::read.xlsx(
          wb,
          paste0('ir_pr_gen_aggr'),
          na.strings = c('NA', paste0('< ', 1:50))
        ) |>
        as_tibble() |>
        rename_with(~'reg_label', any_of('population'))

      dat.01 <-
        bind_rows(
          dat.migr.ir.pr,
          dat.gen.ir.pr
        ) |>
        mutate(
          reg_label = factor(
            .data$reg_label,
            levels = c(
              'general population',
              gtools::mixedsort(unique(dat.migr.ir.pr$reg_label))
            )
          )
        )

      common.vars <- c('reg_label', 'sex', 'ageg', 'can')
      index.names <- c('ir', 'pr')

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
          ageg_sta = .data$ageg |> str_remove('_.*') |> as.numeric(),
          ageg_sto = .data$ageg |> str_remove('.*_') |> as.numeric(),
          ageg_mid = (.data$ageg_sto + .data$ageg_sta) / 2
        )
    } else {
      dat <- tribble(
        ~reg_label,
        ~sex,
        ~ageg,
        ~can,
        ~index,
        ~est,
        ~lci,
        ~uci,
        ~ageg_sta,
        ~ageg_sto,
        ~ageg_mid
      )
    }
  }
