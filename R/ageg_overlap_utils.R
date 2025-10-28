#' @importFrom rlang .data

inter_conv_tab <- function(i.str.old, i.str.new, sep = '_') {
  inter.conv.tab <-
    tidyr::expand_grid(inter.old = i.str.old, inter.new = i.str.new) |>
    dplyr::mutate(
      overlap = inter_overlap(
        i1 = inter_start(.data$inter.old, sep = sep),
        o1 = inter_stop(.data$inter.old, sep = sep),
        i2 = inter_start(.data$inter.new, sep = sep),
        o2 = inter_stop(.data$inter.new, sep = sep)
      )
    ) |>
    dplyr::filter(.data$overlap > 0)
  inter.conv.tab
}

inter_start <- function(i.str, sep = "_") {
  purrr::map_dbl(
    i.str,
    ~ {
      if (stringr::str_detect(.x, sep)) {
        as.numeric(stringr::str_squish(stringr::str_remove(
          .x,
          paste0(sep, ".*$")
        )))
      } else if (stringr::str_detect(.x, "<|\u2264|<=")) {
        -Inf
      } else if (
        stringr::str_detect(.x, "\u2265|>=") |
          stringr::str_detect(.x, "^\\d{1,2}$")
      ) {
        as.numeric(stringr::str_squish(stringr::str_remove(.x, "\u2265|>=")))
      } else if (stringr::str_detect(.x, ">")) {
        as.numeric(stringr::str_squish(stringr::str_remove(.x, ">"))) + 1
      } else {
        warning(paste0(.x, " is not a valid interval."))
        NA
      }
    }
  )
}

inter_stop <- function(i.str, sep = "_") {
  purrr::map_dbl(
    i.str,
    ~ {
      if (stringr::str_detect(.x, sep)) {
        as.numeric(stringr::str_squish(stringr::str_remove(
          .x,
          paste0("^.*", sep)
        )))
      } else if (
        stringr::str_detect(.x, ">|\u2265|>=") |
          stringr::str_detect(.x, "^\\d{1,2}$")
      ) {
        Inf
      } else if (stringr::str_detect(.x, "\u2264|<=")) {
        as.numeric(stringr::str_squish(stringr::str_remove(.x, "\u2264|<=")))
      } else if (stringr::str_detect(.x, "<")) {
        as.numeric(stringr::str_squish(stringr::str_remove(.x, "<"))) + 1
      } else {
        warning(paste0(.x, " is not a valid interval."))
        NA
      }
    }
  )
}

inter_overlap <- function(i1, o1, i2, o2) {
  max1 <- o1 - i1 + 1
  max2 <- o2 - i2 + 1
  t1 <- o1 - i2 + 1
  t2 <- o2 - i1 + 1
  pmax(pmin(t1, t2, max1, max2), 0)
}

convert_new_ageg <-
  function(
    dat.tc,
    dat.r,
    by.fixed = c('cob_iso3', 'sex'),
    summ.vars = c('nallC')
  ) {
    var.adapt.tc <- dat.tc |> pull('ageg') |> unique()
    var.adapt.r <- dat.r |> pull('ageg') |> unique()

    conv.tab <-
      inter_conv_tab(var.adapt.tc, var.adapt.r) |>
      rename('ageg.tc' = 'inter.old', 'ageg.r' = 'inter.new')

    dat.tc.multi.aggr <-
      dat.tc |>
      rename('ageg.tc' = 'ageg') |>
      left_join(
        conv.tab,
        by = c('ageg.tc'),
        relationship = "many-to-many"
      ) |>
      group_by(across(all_of(c(by.fixed, 'ageg.r')))) |>
      summarise(
        across(all_of(summ.vars), ~ sum(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) |>
      rename(
        'ageg' = 'ageg.r'
      )

    dat.tc.multi.aggr
  }
