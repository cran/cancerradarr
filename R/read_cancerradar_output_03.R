#' Read cancer registry summary statistics (age-specific number of cancer cases)
#'
#' @param filename.out file path, the path to a cancer RADAR output file
#' @param aggr.level character, the aggregation level to be considered. Should be one of `cob_iso3`
#'                   (country of birth; smaller unit), `un_region` (UN region), `un_subergion` (
#'                   UN sub-region), `hdi_cat` (HDI category), `asr_rank_cat` (cancer burden category
#'                   in the country of birth), `any_migr` (any migration background)
#'
#' @return a tibble with 10 columns
#'  - reg_label: factor, the chosen aggregation level id
#'  - sex: character, male/female
#'  - ageg: character, age group (here `total`)
#'  - can: character, the cancer type
#'  - index: character, the type of index
#'  - ncan: dbl, the number of cancer
#'  - py: dbl, the population size (if available)
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
#' dat.out <- read_cancerradar_output_03(filename.out, 'any_migr')
#' head(dat.out)
#'
#'
read_cancerradar_output_03 <-
  function(filename.out, aggr.level = 'cob_iso3'){
    wb <- openxlsx::loadWorkbook(filename.out)
    sheets.list <- openxlsx::sheets(wb)

    if(any(stringr::str_detect(sheets.list, paste0(aggr.level, '$')))){

      dat.migr.ir.pr <-
        openxlsx::read.xlsx(wb, paste0('ir_pr_migr_aggr_', aggr.level), na.strings = c('NA', paste0('< ', 1:50))) %>%
        as_tibble() %>%
        rename_with(~ 'reg_label', any_of(aggr.level))

      dat.gen.ir.pr <-
        openxlsx::read.xlsx(wb, paste0('ir_pr_gen_aggr'), na.strings = c('NA', paste0('< ', 1:50))) %>%
        as_tibble() %>%
        rename_with(~ 'reg_label', any_of('population'))

      dat.01 <-
        bind_rows(
          dat.migr.ir.pr,
          dat.gen.ir.pr
        ) %>%
        mutate(
          reg_label = factor(.data$reg_label, levels = c('general population', gtools::mixedsort(unique(dat.migr.ir.pr$reg_label))))
        )

      common.vars <- c('reg_label', 'sex', 'ageg', 'can')
      index.names <- c('ncan', 'py')

      dat.est <-
        dat.01 %>%
        select(any_of(c(common.vars, index.names)))

      dat <-
        dat.est %>%
        mutate(
          ageg_sta = .data$ageg |> str_remove('_.*') |> as.numeric(),
          ageg_sto = .data$ageg |> str_remove('.*_') |> as.numeric(),
          ageg_mid = (.data$ageg_sto + .data$ageg_sta) / 2
        )
    } else {
      dat <- tribble(~ reg_label, ~ sex, ~ ageg, ~ can, ~ index, ~ ncan, ~ py, ~ ageg_sta, ~ ageg_sto, ~ ageg_mid)
    }
  }
