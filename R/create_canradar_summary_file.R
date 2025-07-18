#' Compute summary statistics from 5 years age-group cancer registry data
#'
#' @param filename.in file path, the file containing the 5 years age counts of cancers stratified per cancer type, sex and country of birth
#' @param filename.out file path, the file where summary .xlsx file will be save
#' @param ncan.min integer, the minimum number of cancer per age group o be displayed
#' @param include.by.cob.stat logical, (TRUE by default) should the statistic per country-of-birth  be computed and included in the output file.
#' @param verbose logical, shall progress message be printed
#'
#' @return
#'  a .xlsx with all the summary statistics needed for Cancer RADAR project to be transmitted to project PIs.
#'
#' @import dplyr
#' @importFrom rlang .data set_names
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_subset str_split_i str_remove
#' @importFrom purrr keep_at
#'
#' @export
#'
#' @concept main
#'
#' @examples
#' \donttest{
#'   ## Update file.in with the path to the input file containing your registry data
#'   ## (e.g. file.filled <- "cancerRADAR_input.xlsx")
#'   file.in <- system.file("extdata", "ex_cancerRADAR_input_filled.xlsx", package = "cancerradarr")
#'   file.out <- 'cancerRADAR_input.xlsx'
#'   ## for cancer radar data submission, we advise to use the parameter ncan.min = 5 and 
#'   ## include.by.cob.stat = TRUE
#'   create_canradar_summary_file(file.in, file.out, ncan.min = 20, include.by.cob.stat = FALSE)
#'   ## remove the file to pass package computation tests
#'   unlink(file.out)
#' }
create_canradar_summary_file <-
  function(filename.in, filename.out, ncan.min = 5, include.by.cob.stat = TRUE, verbose = TRUE){
    ## Contents
    ##
    ## Chapter 1 - Generate rates:
    ##  Chapter 1.1 - 	Incidence Rate (ir) by age-group
    ##  Chapter 1.2 - 	Proportion rate (pr) by age-group, which is number of cancers if a specific cancer by all cancers in the respective group.
    ## 	          			This rate will be used to estimate the proportional incidence ratio (PIR).
    ##  Chapter 1.3 - 	Crude incidence rate (cir), with corresponding standard error
    ##  Chapter 1.4 - 	Age-adjusted world standardized incidence rate (air), with corresponding standard error
    ##
    ## Chapter 2 - Generate ratios:
    ##  Chapter 2.1 -	Incidence Rate Ratio (IRR), with corresponding standard error
    ##  Chapter 2.2 -	Standardized Incidence Ratio (SIR)
    ##    Chapter 2.2a-	Reference IR
    ##    Chapter 2.2b-	Expected by age-group
    ##    Chapter 2.2c-	SIR, with corresponding standard error
    ##  Chapter 2.3 -	Proportional Incidence Ratio (PIR)
    ##    Chapter 2.3a-	Reference PR
    ##    Chapter 2.3b-	Expected by age-group
    ##    Chapter 2.3c-	PIR, with corresponding standard error


    ## Chapter 0 - Read and reshape input data #####################################
    if(verbose) cat('\n> Reading', filename.in)
    data.info <- openxlsx::read.xlsx(filename.in, 'data_info') |> magrittr::set_colnames(c('inf.lab', 'inf.val'))
    ref.cob <- data.info[1, 2]
    ref.cob.iso3 <-
      openxlsx::read.xlsx(filename.in, 'py_male') |>
      select('cob_label', 'cob_iso3') |>
      filter(.data$cob_label == ref.cob) |>
      pull('cob_iso3')

    ## add the geographical aggregation columns
    sheets.names <- openxlsx::getSheetNames(filename.in) |> setdiff(c('readme', 'data_info'))

    dat.in.list <-
      purrr::map(sheets.names, ~ openxlsx::read.xlsx(filename.in, .x)) |>
      magrittr::set_names(sheets.names)

    common.vars <- c("cob_iso3")

    ## migrant background data
    pop.migr.names <- sheets.names |> stringr::str_subset('py_.*')
    dat.in.pop <-
      purrr::reduce(
        keep_at(dat.in.list, pop.migr.names),
        full_join,
        by = common.vars
      ) |>
      select(c(all_of(common.vars), starts_with('py'))) |>
      pivot_longer(cols = - all_of(common.vars), names_to = 'py_label', values_to = 'py') |>
      mutate(
        sex = str_split_i(.data$py_label, '_', 2),
        ageg = str_remove(.data$py_label, '.*male_'),
        .after = last(common.vars)
      ) |>
      select(- 'py_label') |>
      mutate(ageg = .data$ageg |> replace(.data$ageg == 'tot', 'total'))

    can.migr.names <- sheets.names |> str_subset('ncx_.*|nliv_.*|nstm_.*|nbrea_.*|ncolo_.*|nlun_.*|nallC_.*')
    dat.in.can.raw <-
      purrr::reduce(
        keep_at(dat.in.list, can.migr.names),
        full_join,
        by = common.vars
      ) |>
      select(c(all_of(common.vars), starts_with('ncx'), starts_with('nliv'), starts_with('nstm'), starts_with('nbrea'), starts_with('ncolo'), starts_with('nlun'), starts_with('nallC'))) |>
      pivot_longer(cols = - all_of(common.vars), names_to = 'ncan_label', values_to = 'ncan') |>
      mutate(
        sex = str_split_i(.data$ncan_label, '_', 2),
        ageg = str_remove(.data$ncan_label, '.*male_'),
        can = str_split_i(.data$ncan_label, '_', 1) |> str_remove('^n'),
        .after = last(common.vars)
      ) |>
      select(- 'ncan_label')

    dat.in.can <-
      dat.in.can.raw |>
      filter(!(.data$ageg %in% c('DCO', 'MV')))

    dat.in.qual <-
      dat.in.can.raw |>
      filter((.data$ageg %in% c('total', 'DCO', 'MV'))) |>
      mutate(
        index = .data$ageg,
        ageg = 'total'
      ) |>
      pivot_wider(names_from = 'index', values_from = 'ncan')

    ## load aggregation data
    dat.aggr <- dat.asr.cat <- NULL
    ## regional + hdi categories
    utils::data('dat.aggr', envir = environment())
    ## burden categories
    utils::data('dat.asr.cat', envir = environment())

    dat.in.long.raw <-
      left_join(dat.in.can, dat.in.pop, by = c(common.vars, 'sex', 'ageg'), multiple = "all") |>
      mutate(ncan = round(.data$ncan), py = round(.data$py)) |>
      left_join(dat.aggr, by = 'cob_iso3') |>
      left_join(dat.asr.cat |> select(-c('asr')), by = c('cob_iso3', 'sex', 'can')) |>
      mutate(
        across(any_of(c(setdiff(colnames(dat.aggr), 'any_migr'), colnames(dat.asr.cat))), ~ replace(.x, is.na(.x), 'Other')),
        across(any_of(c('any_migr')), ~ replace(.x, is.na(.x), 'migrant'))
      )

    dat.in.qual.raw <-
      left_join(dat.in.qual, dat.in.pop, by = c(common.vars, 'sex', 'ageg')) |>
      left_join(dat.aggr, by = 'cob_iso3') |>
      left_join(dat.asr.cat |> select(-c('asr')), by = c('cob_iso3', 'sex', 'can')) |>
      mutate(
        across(any_of(c(setdiff(colnames(dat.aggr), 'any_migr'), colnames(dat.asr.cat))), ~ replace(.x, is.na(.x), 'Other')),
        across(any_of(c('any_migr')), ~ replace(.x, is.na(.x), 'migrant'))
      )

    # ## deal with the non matching iso3 codes ATA: antartica, UND: Undefined, UNK: Umknown
    # for(c_ in setdiff(colnames(dat.aggr), 'cob_iso3')){
    #   na.ids_ <- which(is.na(dat.in.long.raw[[c_]]))
    #   dat.in.long.raw[[c_]][na.ids_] <-  dat.in.long.raw[['cob_iso3']][na.ids_]
    # }


    ## general population from the cancer registry
    dat.ref.in.pop <- dat.in.pop |> filter(.data$cob_iso3 == ref.cob.iso3) |> rename('pyref' = 'py') |> select(- c('cob_iso3'))
    dat.ref.in.can <- dat.in.can |> filter(.data$cob_iso3 == ref.cob.iso3) |> rename('ncanref' = 'ncan') |> select(- c('cob_iso3'))
    dat.ref.in.qual <- dat.in.qual |> filter(.data$cob_iso3 == ref.cob.iso3) |> select(- c('cob_iso3'))

    dat.ref.in.long <-
      left_join(dat.ref.in.can, dat.ref.in.pop, by = c('sex', 'ageg'), multiple = "all") |>
      mutate(ncanref = round(.data$ncanref), pyref = round(.data$pyref))

    if(verbose) cat('\n> Reference population aggregation\n')
    dat.ref.in.long.aggr <-
      dat.ref.in.long |>
      dplyr::filter(!(.data$ageg %in% c('total', '00_04', '05_09', '10_14', '15_19'))) |>
      # dplyr::filter(ageg != 'total') |>
      tidyr::nest(data = - all_of(c('sex', 'can'))) |>
      dplyr::mutate(
        data.aggr =
          purrr::map(
            .data$data,
            ~ custom_ageg_aggregation(
              .x |> dplyr::arrange('ageg'),
              ncan.min = ncan.min,
              add.total = TRUE,
              ncan.lab = 'ncanref',
              py.lab = 'pyref'
            ),
            .progress =
              list(
                name = 'custom aggregation'
              )
          )
      ) |>
      dplyr::select(- 'data') |>
      tidyr::unnest('data.aggr') |>
      dplyr::rename('ageg' = 'ageg.aggr')

    ## general population from globocan 2022
    globocan.2022.eu <- NULL
    utils::data('globocan.2022.eu', envir = environment())

    dat.ref.in.long.globocan <-
      globocan.2022.eu |>
      filter(.data$cob_iso3 == ref.cob.iso3) |>
      select(- c('cob_iso3'))

    dat.ref.in.long.globocan.eu27 <-
      globocan.2022.eu |>
      filter(.data$cob_iso3 == 'E27') |>
      select(- c('cob_iso3'))

    dat.ref.in.long.globocan.euUN <-
      globocan.2022.eu |>
      filter(.data$cob_iso3 == 'EUN') |>
      select(- c('cob_iso3'))

    list_out <- list_out_ref <- list()
    ## DEVEL:
    # aggr.level.list <- c('asr_rank_cat')
    aggr.level.list <- c(if(include.by.cob.stat) 'cob_iso3', 'un_region', 'un_subregion', 'hdi_cat', 'any_migr', 'asr_rank_cat')
    for(aggr.level in aggr.level.list){
      if(verbose) cat('\n> Aggregation level:', aggr.level)

      if(verbose) cat('\n\t- Data aggregation..\n')
      dat.in.long <-
        dat.in.long.raw |>
        filter(.data$cob_iso3 != ref.cob.iso3) |>
        group_by(across(all_of(c(aggr.level, 'sex', 'ageg', 'can')))) |>
        reframe(
          ncan.all.na = all(is.na(.data$ncan)),
          py.all.na = all(is.na(.data$py)),
          ncan = if(!.data$ncan.all.na) sum(.data$ncan, na.rm = TRUE) else NA,
          py = if(!.data$py.all.na) sum(.data$py, na.rm = TRUE) else NA
        ) |>
        select(- c('ncan.all.na', 'py.all.na'))

      ## TODO check with Nienke if we want to perform custom aggregation
      ##.     for cancer cases only or if we want to expand it to all
      ##.     the metrics.

      dat.in.long.aggr <-
        dat.in.long |>
        dplyr::filter(!(.data$ageg %in% c('total', '00_04', '05_09', '10_14', '15_19'))) |>
        # dplyr::filter(ageg != 'total') |>
        tidyr::nest(data = - all_of(c(aggr.level, 'sex', 'can'))) |>
        dplyr::mutate(
          data.aggr =
            purrr::map(
              .data$data,
              ~ custom_ageg_aggregation(
                  .x |> dplyr::arrange('ageg'),
                  ncan.min = ncan.min,
                  add.total = TRUE
                ),
              .progress =
                list(
                  name = 'custom aggregation'
                )
            )
        ) |>
        dplyr::select(- 'data') |>
        tidyr::unnest('data.aggr') |>
        dplyr::rename('ageg' = 'ageg.aggr')

      ## Chapter 1.1 - 	Incidence Rate (ir) by age-group #############################
      if(verbose) cat('\n\t- Incidence rates (ir)..')
      compute_incidence_rates <-
        function(dat, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncan.min = 5){
          dat |>
            filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0) |>
            mutate(
              incidence_rates(.data[[ncan.lab]], .data[[py.lab]], ncan.min = ncan.min)
            ) |>
            select(all_of(c(aggr.level, 'sex', 'ageg', 'can', 'est', 'lci', 'uci')))
        }

      dat.ir <- compute_incidence_rates(dat.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncan.min = ncan.min)
      dat.ir.aggr <- compute_incidence_rates(dat.in.long.aggr, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncan.min = ncan.min)
      ## registry reference population
      dat.ref.ir <- compute_incidence_rates(dat.ref.in.long, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)
      dat.ref.ir.aggr <- compute_incidence_rates(dat.ref.in.long.aggr, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)
      ## globocan national reference population
      dat.ref.globocan.ir <- compute_incidence_rates(dat.ref.in.long.globocan, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)
      ## globocan EU27 reference population
      dat.ref.globocan.eu27.ir <- compute_incidence_rates(dat.ref.in.long.globocan.eu27, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)
      ## globocan EUUN reference population
      dat.ref.globocan.euUN.ir <- compute_incidence_rates(dat.ref.in.long.globocan.euUN, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)

      ## Chapter 1.2 - 	Proportional Rate (pr) by age-group ##########################
      if(verbose) cat('\n\t- Proportional rates (pr)..')
      compute_proportional_rates <-
        function(dat.aggr, dat.notaggr = dat.aggr, aggr.level = NULL, ncan.lab = 'ncan', ncan.min = 5){
          right_join(
            dat.aggr |>
              filter(.data$can != 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
              select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))),
            convert_new_ageg(
              dat.tc = dat.notaggr |>
                filter(.data$can == 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
                select(all_of(c(aggr.level, 'sex', 'ageg', 'nallC' = ncan.lab))),
              dat.r = dat.aggr |>
                filter(.data$can != 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
                select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))),
              by.fixed = c(aggr.level, 'sex'),
              summ.vars = c('nallC')
            ),
            by = c(aggr.level, 'sex', 'ageg')
          ) |>
            mutate(
              proportional_rates(.data[[ncan.lab]], .data$nallC, ncan.min = ncan.min)
            ) |>
            select(all_of(c(aggr.level, 'sex', 'ageg', 'can', 'est', 'lci', 'uci')))
        }

      ## Special case of aggregation by burden in the country of birth
      ## We have to unsure that the same set of countries are compared together which is not necessary
      ## the case in cancer specific aggregation. For instance countries at the highest burden level
      ## for cervical cancer are often at the lowest burden level for all cancer.
      compute_proportional_rates_burden <-
        function(dat.aggr, dat.notaggr = dat.aggr, aggr.level = NULL, ncan.lab = 'ncan', ncan.min = 5){
          right_join(
            dat.aggr |>
              filter(.data$can != 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
              select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))),
            convert_new_ageg(
              dat.tc = dat.notaggr |>
                filter(.data$can == 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
                ## here is the trick to ensure the same countries are aggregated together
                select(all_of(c('cob_iso3', 'sex', 'ageg', 'ncan', 'py'))) |>
                left_join(
                  dat.asr.cat |> filter(.data$can != 'allC') |> select(- all_of('asr')),
                  by = c('cob_iso3', 'sex'),
                  relationship = "many-to-many"
                ) |>
                select(all_of(c(aggr.level, 'sex', 'ageg', 'nallC' = ncan.lab, 'can'))) |>
                group_by(across(all_of(c(aggr.level, 'sex', 'ageg', 'can')))) |>
                summarise(
                  across(all_of('nallC'), ~ sum(.x, na.rm = TRUE)),
                  .groups = 'drop'
                ),
              dat.r = dat.aggr |>
                filter(.data$can != 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
                select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))),
              by.fixed = c(aggr.level, 'sex', 'can'),
              summ.vars = c('nallC')
            ),
            by = c(aggr.level, 'sex', 'can', 'ageg')
          ) |>
            mutate(
              proportional_rates(.data[[ncan.lab]], .data$nallC, ncan.min = ncan.min)
            ) |>
            select(all_of(c(aggr.level, 'sex', 'ageg', 'can', 'est', 'lci', 'uci')))
        }

      if(aggr.level == 'asr_rank_cat'){ ## special case of burden aggregation
        dat.pr <- compute_proportional_rates_burden(dat.in.long, dat.in.long.raw, aggr.level = aggr.level, ncan.lab = 'ncan', ncan.min = ncan.min)
        dat.pr.aggr <- compute_proportional_rates_burden(dat.in.long.aggr, dat.in.long.raw, aggr.level = aggr.level, ncan.lab = 'ncan', ncan.min = ncan.min)
      } else { ## all other aggregations
        dat.pr <- compute_proportional_rates(dat.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', ncan.min = ncan.min)
        dat.pr.aggr <- compute_proportional_rates(dat.in.long.aggr, dat.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', ncan.min = ncan.min)
      }

      dat.ref.pr <- compute_proportional_rates(dat.ref.in.long, aggr.level = NULL, ncan.lab = 'ncanref', ncan.min = ncan.min)
      dat.ref.pr.aggr <- compute_proportional_rates(dat.ref.in.long.aggr, dat.ref.in.long, aggr.level = NULL, ncan.lab = 'ncanref', ncan.min = ncan.min)


      ## Chapter 1.3 - 	Crude incidence rate (cir), with corresponding standard error ####
      if(verbose) cat('\n\t- Crude incidence rates (cir)..')
      compute_crude_incidence_rates <-
        function(dat, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncan.min = 5){
          dat |>
            filter(.data$ageg == 'total', !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0) |>
            mutate(
              incidence_rates(.data[[ncan.lab]], .data[[py.lab]], ncan.min = ncan.min)
            ) |>
            select(all_of(c(aggr.level, 'sex', 'ageg', 'can', 'est', 'lci', 'uci')))
        }

      dat.cir <- compute_crude_incidence_rates(dat.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncan.min = ncan.min)
      dat.ref.cir <- compute_crude_incidence_rates(dat.ref.in.long, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)

      ## Chatper 1.4 - 	Age-adjusted incidence rate (air), with corresponding standard error ####
      ## asir
      if(verbose) cat('\n\t- Age-standardized incidence rate (asir)..')
      ## World standard population used by GLOBOCCAN
      ## https://gco.iarc.fr/today/help
      ## Page 184 --> https://ci5.iarc.fr/CI5I-X/old/vol10/CI5vol10.pdf
      ## For the standard error the binomial distribution was used
      std.pop <-
        tribble(
          ~ ageg, ~ ageg10, ~ ageg20, ~ pystd,
          '00_04', '00_09', '00_19',  12000,
          '05_09', '00_09', '00_19',  10000,
          '10_14', '10_19', '00_19',  9000,
          '15_19', '10_19', '00_19',  9000,
          '20_24', '20_29', '20_39',  8000,
          '25_29', '20_29', '20_39',  8000,
          '30_34', '30_39', '20_39',  6000,
          '35_39', '30_39', '20_39',  6000,
          '40_44', '40_49', '40_59',  6000,
          '45_49', '40_49', '40_59',  6000,
          '50_54', '50_59', '40_59',  5000,
          '55_59', '50_59', '40_59',  4000,
          '60_64', '60_69', '60_79',  4000,
          '65_69', '60_69', '60_79',  3000,
          '70_74', '70_79', '60_79',  2000,
          '75_79', '70_79', '60_79',  1000,
          '80_84', '80'   , '80'   ,  500,
          '85',    '80'   , '80'   ,  500
        )

      compute_age_standardized_incidence_rates <-
        function(dat, std.pop, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncan.min = 5){
          left_join(
            dat |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0),
            std.pop,
            by = c('ageg')
          ) |>
            group_by(across(all_of(c(aggr.level, 'can', 'sex')))) |>
            summarize(
              age_standardized_incidence_rates(.data[[ncan.lab]], .data[[py.lab]], .data$pystd, ncan.min = ncan.min),
              .groups = 'drop'
            )
        }

      dat.asir <- compute_age_standardized_incidence_rates(dat.in.long, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncan.min = ncan.min)
      dat.ref.asir <- compute_age_standardized_incidence_rates(dat.ref.in.long, std.pop, aggr.level = NULL, ncan.lab = 'ncanref', py.lab = 'pyref', ncan.min = ncan.min)

      ## Chapter 2.1 -	Incidence Rate Ratio (IRR) and Incidence Rate Difference (IRD), with corresponding standard error ####
      ## note: the generated standard error is on the log scale, so when confidence
      ## intervals are estimated this needs to be taken into account
      ## cirr
      if(verbose) cat('\n\t- Crude incidence rates ratio (cirr)..')
      compute_incidence_rates_ratio <-
        function(dat, dat.ref, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = 5){
          left_join(
            dat |> filter(.data$ageg == 'total', !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0 ),
            dat.ref |> filter(.data$ageg == 'total', !is.na(.data[[ncanref.lab]]), .data[[pyref.lab]] > 0),
            by = c('sex', 'ageg', 'can')
          ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              incidence_rates_ratio(ncan = .data[[ncan.lab]], py = .data[[py.lab]], ncanref = .data[[ncanref.lab]], pyref = .data[[pyref.lab]], ncan.min = ncan.min)
            )
        }

      dat.cirr <- compute_incidence_rates_ratio(dat.in.long, dat.ref.in.long, aggr.level = aggr.level, ncan.min = 5)
      dat.globocan.cirr <- compute_incidence_rates_ratio(dat.in.long, dat.ref.in.long.globocan, aggr.level = aggr.level, ncan.min = 5)
      dat.globocan.eu27.cirr <- compute_incidence_rates_ratio(dat.in.long, dat.ref.in.long.globocan.eu27, aggr.level = aggr.level, ncan.min = 5)
      dat.globocan.euUN.cirr <- compute_incidence_rates_ratio(dat.in.long, dat.ref.in.long.globocan.euUN, aggr.level = aggr.level, ncan.min = 5)

      ## asirr
      if(verbose) cat('\n\t- Age-standardized incidence rates ratio (asirr)..')
      compute_age_standardized_incidence_rate_ratio <-
        function(dat, dat.ref, std.pop, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = 5){
          left_join(
            dat |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0),
            dat.ref |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncanref.lab]]), .data$pyref > 0),
            by = c('sex', 'ageg', 'can')
          ) |>
            left_join(
              std.pop,
              by = c('ageg')
            ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              standardized_incidence_rate_ratio(.data[[ncan.lab]], .data[[py.lab]], .data[[ncanref.lab]], .data[[pyref.lab]], .data$pystd, ncan.min = ncan.min)
            )
        }

      dat.asirr <- compute_age_standardized_incidence_rate_ratio(dat.in.long, dat.ref.in.long, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.asirr <- compute_age_standardized_incidence_rate_ratio(dat.in.long, dat.ref.in.long.globocan, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.eu27.asirr <- compute_age_standardized_incidence_rate_ratio(dat.in.long, dat.ref.in.long.globocan.eu27, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.euUN.asirr <- compute_age_standardized_incidence_rate_ratio(dat.in.long, dat.ref.in.long.globocan.euUN, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)

      ## cird
      if(verbose) cat('\n\t- Crude incidence rates difference (cird)..')
      compute_incidence_rates_difference <-
        function(dat, dat.ref, std.pop, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = 5){
          left_join(
            dat |> filter(.data$ageg == 'total', !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0),
            dat.ref |> filter(.data$ageg == 'total', !is.na(.data[[ncanref.lab]]), .data[[pyref.lab]] > 0),
            by = c('sex', 'ageg', 'can')
          ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              incidence_rates_difference(ncan = .data[[ncan.lab]], py = .data[[py.lab]], ncanref = .data[[ncanref.lab]], pyref = .data[[pyref.lab]], ncan.min = ncan.min)
            )
        }

      dat.cird <- compute_incidence_rates_difference(dat.in.long, dat.ref.in.long, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.cird <- compute_incidence_rates_difference(dat.in.long, dat.ref.in.long.globocan, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.eu27.cird <- compute_incidence_rates_difference(dat.in.long, dat.ref.in.long.globocan.eu27, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.euUN.cird <- compute_incidence_rates_difference(dat.in.long, dat.ref.in.long.globocan.euUN, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)

      ## asird
      if(verbose) cat('\n\t- Age-standardized incidence rates difference (asird)..')
      compute_age_standardized_incidence_rate_difference <-
        function(dat, dat.ref, std.pop, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = 5){
          left_join(
            dat |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0),
            dat.ref |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncanref.lab]]), .data[[pyref.lab]] > 0),
            by = c('sex', 'ageg', 'can')
          ) |>
            left_join(
              std.pop,
              by = c('ageg')
            ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              standardized_incidence_rate_difference(.data[[ncan.lab]], .data[[py.lab]], .data[[ncanref.lab]], .data[[pyref.lab]], .data$pystd, ncan.min = ncan.min)
            )
        }

      dat.asird <- compute_age_standardized_incidence_rate_difference(dat.in.long, dat.ref.in.long, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.asird <- compute_age_standardized_incidence_rate_difference(dat.in.long, dat.ref.in.long.globocan, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.eu27.asird <- compute_age_standardized_incidence_rate_difference(dat.in.long, dat.ref.in.long.globocan.eu27, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.euUN.asird <- compute_age_standardized_incidence_rate_difference(dat.in.long, dat.ref.in.long.globocan.euUN, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)

      ## Chapter 2.2 - Indirect Standardized Incidence Ratio (SIR) ####
      if(verbose) cat('\n\t- Indirect standardized incidence ratio (sir)..')
      compute_indirect_standardized_incidence_ratio <-
        function(dat, dat.ref, std.pop, aggr.level = NULL, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = 5){
          left_join(
            dat |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]]), .data[[py.lab]] > 0),
            dat.ref |> filter(!(.data$ageg %in% c('total')), !is.na(.data[[ncanref.lab]]), .data[[pyref.lab]] > 0),
            by = c('sex', 'ageg', 'can')
          ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              indirect_standardized_incidence_ratio(.data[[ncan.lab]], .data[[py.lab]], .data[[ncanref.lab]], .data[[pyref.lab]], ncan.min = ncan.min)
            )
        }

      dat.sir <- compute_indirect_standardized_incidence_ratio(dat.in.long, dat.ref.in.long, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
      dat.globocan.sir <- compute_indirect_standardized_incidence_ratio(dat.in.long, dat.ref.in.long.globocan, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
       dat.globocan.eu27.sir <- compute_indirect_standardized_incidence_ratio(dat.in.long, dat.ref.in.long.globocan.eu27, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)
       dat.globocan.euUN.sir <- compute_indirect_standardized_incidence_ratio(dat.in.long, dat.ref.in.long.globocan.euUN, std.pop, aggr.level = aggr.level, ncan.lab = 'ncan', py.lab = 'py', ncanref.lab = 'ncanref', pyref.lab = 'pyref', ncan.min = ncan.min)

      ## Chapter 2.3 - Proportional Incidence Ratio (PIR) ####
       if(verbose) cat('\n\t- Proportional incidence ratio (pir)..')
      compute_indirect_proportional_incidence_ratio <-
        function(dat, dat.ref, aggr.level = NULL, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = 5){
         left_join(
           dat |> filter(!(.data$ageg %in% c('total')), .data$can != 'allC', !is.na(.data[[ncan.lab]])) |> select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))),
           dat |> filter(!(.data$ageg %in% c('total')), .data$can == 'allC', !is.na(.data[[ncan.lab]])) |> select(all_of(c(aggr.level, 'sex', 'ageg', 'ntot' = ncan.lab))),
           by = c(aggr.level, 'sex', 'ageg')
         ) |>
           left_join(
             left_join(
               dat.ref |> filter(!(.data$ageg %in% c('total')), .data$can != 'allC', !is.na(.data[[ncanref.lab]])) |> select(all_of(c('sex', 'ageg', 'can', ncanref.lab))),
               dat.ref |> filter(!(.data$ageg %in% c('total')), .data$can == 'allC', !is.na(.data[[ncanref.lab]])) |> select(all_of(c('sex', 'ageg', 'ntotref' = ncanref.lab))),
               by = c('sex', 'ageg')
             ),
             by = c('sex', 'ageg', 'can')
           ) |>
           group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
           reframe(
             indirect_proportional_incidence_ratio(.data[[ncan.lab]], .data$ntot, .data[[ncanref.lab]], .data$ntotref, ncan.min = ncan.min)
           )
        }

      ## Special case of aggregation by burden in the country of birth
      ## We have to unsure that the same set of countries are compared together which is not necessary
      ## the case in cancer specific aggregation. For instance countries at the highest burden level
      ## for cervical cancer are often at the lowest burden level for all cancer.
      compute_indirect_proportional_incidence_ratio_burden <-
        function(dat, dat.ref, aggr.level = NULL, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = 5){
          left_join(
            dat |>
              filter(.data$can != 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
              select(all_of(c(aggr.level, 'sex', 'ageg', 'can', ncan.lab))) |>
              group_by(across(all_of(c(aggr.level, 'sex', 'ageg', 'can')))) |>
              summarise(
                across(all_of(ncan.lab), ~ sum(.x, na.rm = TRUE)),
                .groups = 'drop'
              ),
            dat |>
              filter(.data$can == 'allC', !(.data$ageg %in% c('total')), !is.na(.data[[ncan.lab]])) |>
              ## here is the trick to ensure the same countries are aggregated together
              select(all_of(c('cob_iso3', 'sex', 'ageg', 'ncan', 'py'))) |>
              left_join(
                dat.asr.cat |> filter(.data$can != 'allC') |> select(- all_of('asr')),
                by = c('cob_iso3', 'sex'),
                relationship = "many-to-many"
              ) |>
              select(all_of(c(aggr.level, 'sex', 'ageg', 'ntot' = ncan.lab, 'can'))) |>
              group_by(across(all_of(c(aggr.level, 'sex', 'ageg', 'can')))) |>
              summarise(
                across(all_of('ntot'), ~ sum(.x, na.rm = TRUE)),
                .groups = 'drop'
              ),
            by = c(aggr.level, 'sex', 'ageg', 'can')
          ) |>
            left_join(
              left_join(
                dat.ref |> filter(!(.data$ageg %in% c('total')), .data$can != 'allC', !is.na(.data[[ncanref.lab]])) |> select(all_of(c('sex', 'ageg', 'can', ncanref.lab))),
                dat.ref |> filter(!(.data$ageg %in% c('total')), .data$can == 'allC', !is.na(.data[[ncanref.lab]])) |> select(all_of(c('sex', 'ageg', 'ntotref' = ncanref.lab))),
                by = c('sex', 'ageg')
              ),
              by = c('sex', 'ageg', 'can')
            ) |>
            group_by(across(all_of(c(aggr.level, 'sex', 'can')))) |>
            reframe(
              indirect_proportional_incidence_ratio(.data[[ncan.lab]], .data$ntot, .data[[ncanref.lab]], .data$ntotref, ncan.min = ncan.min)
            )
        }

      if(aggr.level == 'asr_rank_cat'){ ## special case of burden aggregation
        dat.pir <- compute_indirect_proportional_incidence_ratio_burden(dat.in.long.raw, dat.ref.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.pir <- compute_indirect_proportional_incidence_ratio_burden(dat.in.long.raw, dat.ref.in.long.globocan, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.eu27.pir <- compute_indirect_proportional_incidence_ratio_burden(dat.in.long.raw, dat.ref.in.long.globocan.eu27, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.euUN.pir <- compute_indirect_proportional_incidence_ratio_burden(dat.in.long.raw, dat.ref.in.long.globocan.euUN, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
      } else {
        dat.pir <- compute_indirect_proportional_incidence_ratio(dat.in.long, dat.ref.in.long, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.pir <- compute_indirect_proportional_incidence_ratio(dat.in.long, dat.ref.in.long.globocan, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.eu27.pir <- compute_indirect_proportional_incidence_ratio(dat.in.long, dat.ref.in.long.globocan.eu27, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
        dat.globocan.euUN.pir <- compute_indirect_proportional_incidence_ratio(dat.in.long, dat.ref.in.long.globocan.euUN, aggr.level = aggr.level, ncan.lab = 'ncan', ncanref.lab = 'ncanref', ncan.min = ncan.min)
      }

      ## Chapter 3- Gather aggregated data ####
      ## compute the totals number of cancers
      if(verbose) cat('\n\t- Total number of cancer..')

      dat.ncan <-
        dat.in.qual.raw |>
        filter(.data$cob_iso3 != ref.cob.iso3) |>
        group_by(across(all_of(c(aggr.level, 'sex', 'ageg', 'can')))) |>
        summarise(
          n_tot = ifelse(all(is.na(.data$total)), NA, sum(.data$total, na.rm = TRUE)),
          n_mv = sum(.data$MV, na.rm = TRUE),
          n_dco = sum(.data$DCO, na.rm = TRUE),
          py = ifelse(all(is.na(.data$py)), NA, sum(.data$py, na.rm = TRUE)),
          .groups = 'drop'
        ) |>
        mutate(
          n_tot = .data$n_tot |> replace(.data$n_tot < ncan.min, paste('<', ncan.min)),
          n_mv = .data$n_mv |> replace((.data$n_tot == paste('<', ncan.min) | is.na(.data$n_tot)), NA),
          n_dco = .data$n_dco |> replace((.data$n_tot == paste('<', ncan.min) | is.na(.data$n_tot)), NA)
        )

      dat.ref.ncan <-
        dat.in.qual.raw |>
        filter(.data$cob_iso3 == ref.cob.iso3) |>
        group_by(across(all_of(c('sex', 'ageg', 'can')))) |>
        summarise(
          n_tot = ifelse(all(is.na(.data$total)), NA, sum(.data$total, na.rm = TRUE)),
          n_mv = sum(.data$MV, na.rm = TRUE),
          n_dco = sum(.data$DCO, na.rm = TRUE),
          py = ifelse(all(is.na(.data$py)), NA, sum(.data$py, na.rm = TRUE)),
          .groups = 'drop'
        ) |>
        mutate(
          n_tot = .data$n_tot |> replace(.data$n_tot < ncan.min, paste('<', ncan.min)),
          n_mv = .data$n_mv |> replace((.data$n_tot == paste('<', ncan.min) | is.na(.data$n_tot)), NA),
          n_dco = .data$n_dco |> replace((.data$n_tot == paste('<', ncan.min) | is.na(.data$n_tot)), NA)
        )

      ## compute the totals population at risk
      # if(verbose) cat('\n\t- Total population at risk..')
      # dat.py <-
      #   dat.in.long |>
      #   filter(.data$ageg == 'total') |>
      #   select(all_of(c(aggr.level, 'sex', 'ageg', 'can', 'py'))) |>
      #   distinct() |>
      #   mutate(py = .data$py |> replace(.data$py < ncan.min, paste('<', ncan.min)))
      #
      # dat.ref.py <-
      #   dat.ref.in.long |>
      #   filter(.data$ageg == 'total') |>
      #   select(all_of(c('sex', 'ageg', 'py' = 'pyref'))) |>
      #   distinct() |>
      #   mutate(py = .data$py |> replace(.data$py < ncan.min, paste('<', ncan.min)))

      ## TODO add cum rates? see page 147

      ## Chapter 4- Save canRADAR summary file ####
      count_gen <-
        dat.ref.ncan |>
        mutate(population = 'general population', .before = 1) |>
        filter(!is.na('n_tot'))

      count_migr <-
        dat.ncan |>
        filter(!is.na('n_tot'))

      ir_pr_gen <-
        dat.ref.in.long |> rename('ncan' = 'ncanref', 'py' = 'pyref') |> filter(!(.data$ageg %in% c('total')), !(is.na(.data$ncan) & is.na(.data$py))) |>
        left_join(dat.ref.ir |> rename('ir' = 'est', 'ir_lci' = 'lci', 'ir_uci' = 'uci'), by = c('sex', 'ageg', 'can')) |>
        left_join(dat.ref.pr |> rename('pr' = 'est', 'pr_lci' = 'lci', 'pr_uci' = 'uci'), by = c('sex', 'ageg', 'can')) |>
        mutate(population = 'general population', .before = 1) |>
        filter(!(.data$ageg %in% c('00_04', '05_09', '10_14', '15_19')))

      ir_pr_gen_aggr <-
        dat.ref.in.long.aggr |> rename('ncan' = 'ncanref', 'py' = 'pyref') |> filter(!(.data$ageg %in% c('total')), !(is.na(.data$ncan) & is.na(.data$py))) |>
        left_join(dat.ref.ir.aggr |> rename('ir' = 'est', 'ir_lci' = 'lci', 'ir_uci' = 'uci'), by = c('sex', 'ageg', 'can')) |>
        left_join(dat.ref.pr.aggr |> rename('pr' = 'est', 'pr_lci' = 'lci', 'pr_uci' = 'uci'), by = c('sex', 'ageg', 'can')) |>
        mutate(population = 'general population', .before = 1) |>
        filter(!(.data$ageg %in% c('00_04', '05_09', '10_14', '15_19')))

      ir_pr_migr <-
        dat.in.long |> filter(!(.data$ageg %in% c('total')), !(is.na(.data$ncan) & is.na(.data$py))) |>
        left_join(dat.ir |> rename('ir' = 'est', 'ir_lci' = 'lci', 'ir_uci' = 'uci'), by = c(aggr.level, 'sex', 'ageg', 'can')) |>
        left_join(dat.pr |> rename('pr' = 'est', 'pr_lci' = 'lci', 'pr_uci' = 'uci'), by = c(aggr.level, 'sex', 'ageg', 'can')) |>
        filter(!(.data$ageg %in% c('00_04', '05_09', '10_14', '15_19')))

      ir_pr_migr_aggr <-
        dat.in.long.aggr |> filter(!(.data$ageg %in% c('total')), !(is.na(.data$ncan) & is.na(.data$py))) |>
        left_join(dat.ir.aggr |> rename('ir' = 'est', 'ir_lci' = 'lci', 'ir_uci' = 'uci'), by = c(aggr.level, 'sex', 'ageg', 'can')) |>
        left_join(dat.pr.aggr |> rename('pr' = 'est', 'pr_lci' = 'lci', 'pr_uci' = 'uci'), by = c(aggr.level, 'sex', 'ageg', 'can')) |>
        filter(!(.data$ageg %in% c('00_04', '05_09', '10_14', '15_19')))

      other_gen <-
        dat.ref.cir |> rename('cir' = 'est', 'cir_lci' = 'lci', 'cir_uci' = 'uci') |>
        full_join(dat.ref.asir |> rename('asir' = 'est', 'asir_lci' = 'lci', 'asir_uci' = 'uci'), by = c('sex', 'can')) |>
        mutate(population = 'general population', .before = 1)

      ## trick to handle registries without py for the general population
      if(nrow(other_gen) == 0){
        other_gen <-
          dat.asr.cat |> select(all_of(c('sex', 'can'))) |> distinct() |>
          mutate(population = 'general population', .before = 1) |>
          mutate(ageg = 'total', .before = 3) |>
          mutate(cir = NaN, cir_lci = NaN, cir_uci = NaN, asir = NaN, asir_lci = NaN, asir_uci = NaN)
      }

      other_migr <-
        dat.cir |> rename('cir' = 'est', 'cir_lci' = 'lci', 'cir_uci' = 'uci') |>
        full_join(dat.cirr |> rename('cirr' = 'est', 'cirr_lci' = 'lci', 'cirr_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.cird |> rename('cird' = 'est', 'cird_lci' = 'lci', 'cird_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.asir |> rename('asir' = 'est', 'asir_lci' = 'lci', 'asir_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.asirr |> rename('asirr' = 'est', 'asirr_lci' = 'lci', 'asirr_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.asird |> rename('asird' = 'est', 'asird_lci' = 'lci', 'asird_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.sir |> rename('sir' = 'est', 'sir_lci' = 'lci', 'sir_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.pir |> rename('pir' = 'est', 'pir_lci' = 'lci', 'pir_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        ## globocan national as reference
        full_join(dat.globocan.cirr |> rename('cirr_glonat' = 'est', 'cirr_glonat_lci' = 'lci', 'cirr_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.cird |> rename('cird_glonat' = 'est', 'cird_glonat_lci' = 'lci', 'cird_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.asirr |> rename('asirr_glonat' = 'est', 'asirr_glonat_lci' = 'lci', 'asirr_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.asird |> rename('asird_glonat' = 'est', 'asird_glonat_lci' = 'lci', 'asird_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.sir |> rename('sir_glonat' = 'est', 'sir_glonat_lci' = 'lci', 'sir_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.pir |> rename('pir_glonat' = 'est', 'pir_glonat_lci' = 'lci', 'pir_glonat_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        ## globocan EU27 as reference
        full_join(dat.globocan.eu27.cirr |> rename('cirr_gloeu27' = 'est', 'cirr_gloeu27_lci' = 'lci', 'cirr_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.eu27.cird |> rename('cird_gloeu27' = 'est', 'cird_gloeu27_lci' = 'lci', 'cird_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.eu27.asirr |> rename('asirr_gloeu27' = 'est', 'asirr_gloeu27_lci' = 'lci', 'asirr_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.eu27.asird |> rename('asird_gloeu27' = 'est', 'asird_gloeu27_lci' = 'lci', 'asird_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.eu27.sir |> rename('sir_gloeu27' = 'est', 'sir_gloeu27_lci' = 'lci', 'sir_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.eu27.pir |> rename('pir_gloeu27' = 'est', 'pir_gloeu27_lci' = 'lci', 'pir_gloeu27_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        ## globocan EUUN as reference
        full_join(dat.globocan.euUN.cirr |> rename('cirr_gloeuUN' = 'est', 'cirr_gloeuUN_lci' = 'lci', 'cirr_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.euUN.cird |> rename('cird_gloeuUN' = 'est', 'cird_gloeuUN_lci' = 'lci', 'cird_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.euUN.asirr |> rename('asirr_gloeuUN' = 'est', 'asirr_gloeuUN_lci' = 'lci', 'asirr_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.euUN.asird |> rename('asird_gloeuUN' = 'est', 'asird_gloeuUN_lci' = 'lci', 'asird_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.euUN.sir |> rename('sir_gloeuUN' = 'est', 'sir_gloeuUN_lci' = 'lci', 'sir_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        full_join(dat.globocan.euUN.pir |> rename('pir_gloeuUN' = 'est', 'pir_gloeuUN_lci' = 'lci', 'pir_gloeuUN_uci' = 'uci'), by = c(aggr.level, 'sex', 'can')) |>
        ## trick to ensure age group is correctly filled (it could be empty if no py available but other stats available)
        mutate(ageg = 'total')

      list_aggr_tmp <-
        list(
          count_migr = count_migr,
          ir_pr_migr_aggr = ir_pr_migr_aggr,
          other_migr = other_migr
        )
      names(list_aggr_tmp) <- paste0(names(list_aggr_tmp), '_', aggr.level)
      list_out <- c(list_out, list_aggr_tmp)

      list_out_ref <-
        list(
          count_gen = count_gen,
          ir_pr_gen_aggr = ir_pr_gen_aggr,
          other_gen = other_gen
        )
    }

    list_data_info_out <-
      list(
        data_info = data.info |> magrittr::set_colnames(c('DATA INFORMATION', ''))
      )

    ## compile some logs of the file creation
    list_log_out <-
      list(
        log =
          tribble(
            ~ LOG,
            paste0('date: ', date()),
            paste0('cancerradar version: ', utils::packageVersion('cancerradarr')),
            paste0('input file: ', filename.in),
            paste0('number of cases threshold: ', ncan.min),
            paste0('stratification per country of birth included: ', include.by.cob.stat),
            paste0('R version: ', R.version$version.string),
            paste0('OS: ', R.version$platform),
            paste0('user: ', Sys.info()[["user"]])
          )
      )

    list_out <- c(list_log_out, list_data_info_out, list_out_ref, list_out)

    headerStyle <- openxlsx::createStyle(textDecoration = 'bold', border = 'bottom', halign = 'center', valign = 'center', wrapText = TRUE, locked = TRUE)
    casesStyle <- openxlsx::createStyle(numFmt = 'COMMA', locked = FALSE)

    readme.file <- system.file("extdata", "readme_cancerradarr_output_file.xlsx", package = "cancerradarr")
    wb <- loadWorkbook(readme.file, na.convert = FALSE) # create a workbook using predefined README

    for(sn_ in names(list_out)){
      addWorksheet(wb, sn_)
      if(sn_ %in% c('log')){
        writeData(wb, sn_, list_out[[sn_]], headerStyle = headerStyle)
      } else {
        writeDataTable(wb, sn_, list_out[[sn_]], headerStyle = headerStyle)
      }
    }

    saveWorkbook(wb, filename.out, overwrite = TRUE) # save workbook

    if(verbose) cat("\n\n>", filename.out, "created!\n")
    return(invisible(list_out))
  }
