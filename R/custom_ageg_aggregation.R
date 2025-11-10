#' Smart aggregation of cancer cases per age group
#'
#' @param dat tibble, a single cancer/sex/country tibble containing
#'        cancer cases from a registry. It should contains the column
#'        ageg and ncan
#' @param ncan.min integer, the minimal number of cancer in
#'        each category
#' @param py.min integer, the minimal number of person-years in
#'        each category
#' @param add.total logical, should the 'total' category added to
#'        the output dataset
#' @param ncan.lab character, the column label where cancer cases are stored
#' @param py.lab character, the column label where (optional) population at
#'               risk are stored
#'
#' @return aggregated dataset where all the age group contains at least
#'         ncan.min cancers cases
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' dat <-
#'   dplyr::tribble(
#'     ~ ageg, ~ ncan,
#'     '00_04', 0,
#'     '05_09', 0,
#'     '10_14', 0,
#'     '15_19', 0,
#'     '20_24', 1,
#'     '25_29', 2,
#'     '30_34', 4,
#'     '35_39', 5,
#'     '40_44', 1,
#'     '45_49', 10,
#'     '50_54', 14,
#'     '55_59', 1,
#'     '60_64', 2,
#'     '65_69', 2,
#'     '70_74', 5,
#'     '75_79', 1,
#'     '80_84', 0,
#'     '85', 0
#'   )
#'
#' custom_ageg_aggregation(dat, 0)
#' custom_ageg_aggregation(dat, 5)
#' custom_ageg_aggregation(dat, 10)
#' custom_ageg_aggregation(dat, 100)
#'
custom_ageg_aggregation <- function(
  dat,
  ncan.min = 5,
  py.min = 0,
  add.total = FALSE,
  ncan.lab = 'ncan',
  py.lab = 'py'
) {
  ## check param validity
  if (ncan.min < 1) {
    warning('aggregation not relevant for ncan.min < 1 and was set to 1.')
    ncan.min <- 1
  }

  ## define the ageg list
  ageg.in.list <-
    intersect(
      c(
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
      ),
      unique(dat$ageg)
    )

  ## check if py column exists
  has.py <- py.lab %in% colnames(dat)

  i.first <- i.last <- NA
  n.bigger <- 0
  for (i in seq_along(ageg.in.list)) {
    i.count <- dat |> filter(.data$ageg == ageg.in.list[i]) |> pull(ncan.lab)
    i.py <- if (has.py) {
      dat |> filter(.data$ageg == ageg.in.list[i]) |> pull(py.lab)
    } else {
      NA
    }
    if (!is.na(i.count)) {
      ## detect the first non null count
      if (is.na(i.first) & i.count > 0) {
        i.first <- i
      }
      ## detect the last non null count
      if (i.count > 0) {
        i.last <- i
      }
      ## count the number of cells >= ncan.min
      if (has.py) {
        if (
          (i.count >= ncan.min) &
            (i.py >= py.min)
        ) {
          n.bigger <- n.bigger + 1
        }
      } else {
        if (i.count >= ncan.min) n.bigger <- n.bigger + 1
      }
    }
  }

  ## deal with no data at all case and
  ## less than n.can.min cancer overalll and
  ## less than py.min person-years overalll
  small.pop.test <-
    if (has.py) {
      (sum(dat[[ncan.lab]], na.rm = TRUE) < ncan.min) |
        (sum(dat[[py.lab]], na.rm = TRUE) < py.min)
    } else {
      (sum(dat[[ncan.lab]], na.rm = TRUE) < ncan.min)
    }

  if (
    (is.na(i.first) & is.na(i.last)) |
      small.pop.test
  ) {
    ## TODO maybe we should return '< ncan.min' or '< py.min' instead of NA ?
    dat.out <-
      tibble(
        ageg.aggr = aggregated_ageg_name(dat$ageg),
        ncan = NA,
        py = if (has.py) {
          if ((sum(dat[[py.lab]], na.rm = TRUE) >= py.min)) {
            sum(dat[[py.lab]], na.rm = TRUE)
          } else {
            NA
          }
        } else {
          NA
        }
      )
    if (add.total) {
      dat.out <-
        dplyr::bind_rows(
          dat.out,
          dat.out |> mutate(ageg.aggr = 'total')
        )
    }
    ## relabel ncan and py if needed
    colnames(dat.out) <- plyr::mapvalues(
      colnames(dat.out),
      c('ncan', 'py'),
      c(ncan.lab, py.lab),
      warn_missing = FALSE
    )
    return(dat.out)
  }

  dat.02 <- dat[i.first:i.last, ]
  vect.lenght <- i.last - i.first + 1

  # chop.vector <- chop_vector(vect.lenght) |> as.matrix()
  ## load chopped.vector.list data
  chopped.vector.list <- NULL
  utils::data('chopped.vector.list', envir = environment())
  chop.vector.full <- chopped.vector.list[[vect.lenght]]

  dat.03 <- if (has.py) {
    dat.02[, c(ncan.lab, py.lab)] |> as.matrix()
  } else {
    dat.02[, c(ncan.lab)] |> as.matrix()
  }

  ## to speed up the process we only consider the combinations
  ## that have more categories > ncan.min in the original dataset
  chop.vector <- chop.vector.full[
    apply(chop.vector.full, 1, function(.x) length(unique(.x)) >= n.bigger),
    ,
    drop = FALSE
  ]

  ## detect the combinations of slices where we get more than the mininmal
  ## number of cancer cases in each cell
  valid.aggr <-
    which(
      apply(
        chop.vector,
        1,
        function(.x) {
          all(
            sapply(
              base::split(dat.03, .x, drop = FALSE),
              function(.xx) {
                if (has.py) {
                  ncan.ids <- 1:(length(.xx) / 2)
                  py.ids <- ((length(.xx) / 2) + 1):length(.xx)
                  ((sum(.xx[ncan.ids], na.rm = TRUE) >= ncan.min) &
                    (sum(.xx[py.ids], na.rm = TRUE) >= py.min)) |
                    (sum(.xx[ncan.ids], na.rm = TRUE) == 0)
                } else {
                  (sum(.xx, na.rm = TRUE) >= ncan.min) |
                    (sum(.xx, na.rm = TRUE) == 0)
                }
              }
            )
          )
        }
      )
    )

  ## select the most detailed data combinations possible
  ## (i.e. bigest number of age groups)
  valid.aggr.nb <- apply(chop.vector[valid.aggr, , drop = FALSE], 1, max)
  best.aggr.nb <- valid.aggr[valid.aggr.nb == max(valid.aggr.nb)]

  ## among the selected keep the one that have the most homogeneous
  ## size distribution
  if (length(best.aggr.nb) > 1) {
    best.aggr.sd <-
      apply(
        chop.vector[best.aggr.nb, , drop = FALSE],
        1,
        function(.x) {
          stats::sd(table(.x))
        }
      )
  } else {
    best.aggr.sd <- 0
  }

  best.aggr <- best.aggr.nb[which.min(best.aggr.sd)]

  ## add the cells without cancer in the beginning and the end of
  ## the cancer distribution
  selected.ageg.aggr.id <-
    c(
      ## trailing zeros (younger ageg)
      if (i.first > 1) {
        1:(i.first - 1)
      },
      ## select the combination that keep the most detailed age group structure
      ## without loss of data
      chop.vector[best.aggr, , drop = TRUE] + i.first - 1,
      ## trailing zeros (older ageg)
      if (i.last < length(ageg.in.list)) {
        (i.last + 1):length(ageg.in.list)
      }
    )

  ## detect the selected age group
  selected.ageg.aggr <- split(ageg.in.list, selected.ageg.aggr.id)

  ## compute the aggregated age group labels
  ageg.out.list.names <-
    purrr::map_chr(selected.ageg.aggr, ~ aggregated_ageg_name(.x))

  names(selected.ageg.aggr) <- ageg.out.list.names

  ## define the list of aggregated names
  ageg.aggr.vect <- purrr::map_dbl(selected.ageg.aggr, length)
  ageg.aggr.vect.02 <- purrr::map2(
    names(ageg.aggr.vect),
    ageg.aggr.vect,
    ~ rep(.x, .y)
  ) |>
    unlist()

  ## aggregate the input dataset with the best combination possible
  dat.out <-
    dat |>
    dplyr::mutate(ageg.aggr = ageg.aggr.vect.02) |>
    dplyr::group_by(.data$ageg.aggr) |>
    dplyr::summarise(
      ncan = sum(.data[[ncan.lab]], na.rm = TRUE),
      py = if (has.py) {
        sum(.data[[py.lab]], na.rm = !all(is.na(.data[[py.lab]])))
      },
      .groups = 'drop'
    )

  if (add.total) {
    dat.out <-
      dplyr::bind_rows(
        dat.out,
        dat.out |>
          dplyr::summarise(
            ncan = sum(.data$ncan, na.rm = TRUE),
            py = if (has.py) {
              sum(.data$py, na.rm = !all(is.na(.data$py)))
            },
            .groups = 'drop'
          ) |>
          dplyr::mutate(ageg.aggr = 'total')
      )
  }
  ## relabel ncan and py if needed
  colnames(dat.out) <- plyr::mapvalues(
    colnames(dat.out),
    c('ncan', 'py'),
    c(ncan.lab, py.lab),
    warn_missing = FALSE
  )
  dat.out
}
