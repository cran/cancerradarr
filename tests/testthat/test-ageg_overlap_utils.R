test_that("inter_start correctly parses interval start values", {
  # Test standard age intervals with underscore separator
  expect_equal(inter_start("00_04"), 0)
  expect_equal(inter_start("25_29"), 25)
  expect_equal(inter_start("80_84"), 80)

  # Test open intervals with >= symbol
  expect_equal(inter_start("85"), 85)
  expect_equal(inter_start("≥65"), 65)
  expect_equal(inter_start(">=70"), 70)

  # Test open intervals with > symbol
  expect_equal(inter_start(">64"), 65) # Should add 1
  expect_equal(inter_start(">74"), 75)

  # Test upper bound intervals
  expect_equal(inter_start("<15"), -Inf)
  expect_equal(inter_start("≤14"), -Inf)
  expect_equal(inter_start("<=19"), -Inf)

  # Test custom separator
  expect_equal(inter_start("20-24", sep = "-"), 20)
  expect_equal(inter_start("30.34", sep = "\\."), 30)

  # Test vector input
  intervals <- c("00_04", "05_09", "85")
  expected <- c(0, 5, 85)
  expect_equal(inter_start(intervals), expected)

  # Test invalid intervals generate warnings and NA
  expect_warning(result <- inter_start("invalid"))
  expect_true(is.na(result))
})

test_that("inter_stop correctly parses interval stop values", {
  # Test standard age intervals with underscore separator
  expect_equal(inter_stop("00_04"), 4)
  expect_equal(inter_stop("25_29"), 29)
  expect_equal(inter_stop("80_84"), 84)

  # Test open intervals with >= symbol (should be Inf)
  expect_equal(inter_stop("85"), Inf)
  expect_equal(inter_stop("≥65"), Inf)
  expect_equal(inter_stop(">=70"), Inf)
  expect_equal(inter_stop(">64"), Inf)

  # Test upper bound intervals
  expect_equal(inter_stop("≤14"), 14)
  expect_equal(inter_stop("<=19"), 19)
  expect_equal(inter_stop("<15"), 16) # Should add 1

  # Test custom separator
  expect_equal(inter_stop("20-24", sep = "-"), 24)
  expect_equal(inter_stop("30.34", sep = "\\."), 34)

  # Test vector input
  intervals <- c("00_04", "05_09", "85")
  expected <- c(4, 9, Inf)
  expect_equal(inter_stop(intervals), expected)

  # Test invalid intervals generate warnings and NA
  expect_warning(result <- inter_stop("invalid"))
  expect_true(is.na(result))
})

test_that("inter_overlap calculates overlaps correctly", {
  # Test perfect overlap
  expect_equal(inter_overlap(0, 4, 0, 4), 5) # 00_04 with 00_04

  # Test partial overlap
  expect_equal(inter_overlap(0, 4, 2, 6), 3) # 00_04 with 02_06 overlaps 2,3,4
  expect_equal(inter_overlap(5, 9, 7, 11), 3) # 05_09 with 07_11 overlaps 7,8,9

  # Test no overlap
  expect_equal(inter_overlap(0, 4, 10, 14), 0) # 00_04 with 10_14
  expect_equal(inter_overlap(20, 24, 30, 34), 0) # 20_24 with 30_34

  # Test adjacent intervals (no overlap)
  expect_equal(inter_overlap(0, 4, 5, 9), 0) # 00_04 with 05_09

  # Test one interval contains another
  expect_equal(inter_overlap(0, 19, 5, 9), 5) # 00_19 contains 05_09
  expect_equal(inter_overlap(5, 9, 0, 19), 5) # 05_09 contained in 00_19

  # Test with infinite bounds
  expect_equal(inter_overlap(0, Inf, 85, 89), 5) # Open interval with closed
  expect_equal(inter_overlap(80, 84, 85, Inf), 0) # No overlap with open interval
  expect_equal(inter_overlap(80, Inf, 85, Inf), Inf) # Both open intervals overlap

  # Test vectorized input
  i1 <- c(0, 5, 20)
  o1 <- c(4, 9, 24)
  i2 <- c(2, 7, 22)
  o2 <- c(6, 11, 26)
  expected <- c(3, 3, 3) # Overlaps: [2,3,4], [7,8,9], [22,23,24]
  expect_equal(inter_overlap(i1, o1, i2, o2), expected)
})

test_that("inter_conv_tab creates correct conversion table", {
  # Test basic conversion table
  old_intervals <- c("00_04", "05_09")
  new_intervals <- c("00_09", "10_19")

  result <- inter_conv_tab(old_intervals, new_intervals)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("inter.old", "inter.new", "overlap"))

  # Check that only overlapping intervals are included
  expect_true(all(result$overlap > 0))

  # Test specific case: 00_04 should overlap with 00_09 (overlap = 5)
  overlap_00_04_00_09 <- result |>
    dplyr::filter(inter.old == "00_04", inter.new == "00_09") |>
    dplyr::pull(overlap)
  expect_equal(overlap_00_04_00_09, 5)

  # Test with custom separator
  old_dash <- c("00-04", "05-09")
  new_dash <- c("00-09")
  result_dash <- inter_conv_tab(old_dash, new_dash, sep = "-")
  expect_equal(nrow(result_dash), 2) # Both old intervals overlap with 00-09

  # Test with no overlapping intervals
  old_no_overlap <- c("00_04", "05_09")
  new_no_overlap <- c("20_24", "25_29")
  result_empty <- inter_conv_tab(old_no_overlap, new_no_overlap)
  expect_equal(nrow(result_empty), 0)
})

test_that("convert_new_ageg aggregates data correctly", {
  # Create test data
  dat_tc <- dplyr::tibble(
    ageg = c("00_04", "05_09", "10_14"),
    cob_iso3 = rep("USA", 3),
    sex = rep("M", 3),
    nallC = c(10, 20, 30)
  )

  dat_r <- dplyr::tibble(
    ageg = c("00_09", "10_19"),
    cob_iso3 = rep("USA", 2),
    sex = rep("M", 2),
    nallC = c(0, 0) # Values don't matter for this test
  )

  result <- convert_new_ageg(dat_tc, dat_r)

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("cob_iso3", "sex", "ageg", "nallC"))

  # Check aggregation results
  # 00_04 and 05_09 should be combined into 00_09: 10 + 20 = 30
  ageg_00_09 <- result |>
    dplyr::filter(ageg == "00_09") |>
    dplyr::pull(nallC)
  expect_equal(ageg_00_09, 30)

  # 10_14 should map to 10_19: 30
  ageg_10_19 <- result |>
    dplyr::filter(ageg == "10_19") |>
    dplyr::pull(nallC)
  expect_equal(ageg_10_19, 30)

  # Test with custom grouping variables
  result_custom <- convert_new_ageg(
    dat_tc,
    dat_r,
    by.fixed = c("cob_iso3"),
    summ.vars = c("nallC")
  )
  expect_named(result_custom, c("cob_iso3", "ageg", "nallC"))
})

test_that("convert_new_ageg handles multiple summary variables", {
  # Create test data with multiple summary variables
  dat_tc <- dplyr::tibble(
    ageg = c("00_04", "05_09"),
    cob_iso3 = rep("USA", 2),
    sex = rep("M", 2),
    nallC = c(10, 20),
    py = c(1000, 2000)
  )

  dat_r <- dplyr::tibble(
    ageg = "00_09",
    cob_iso3 = "USA",
    sex = "M",
    nallC = 0,
    py = 0
  )

  result <- convert_new_ageg(
    dat_tc,
    dat_r,
    summ.vars = c("nallC", "py")
  )

  # Check that both variables are summed correctly
  expect_equal(result$nallC, 30) # 10 + 20
  expect_equal(result$py, 3000) # 1000 + 2000
})

test_that("edge cases and error handling", {
  # Test empty data
  empty_tc <- dplyr::tibble(
    ageg = character(0),
    cob_iso3 = character(0),
    sex = character(0),
    nallC = numeric(0)
  )

  dat_r <- dplyr::tibble(
    ageg = "00_09",
    cob_iso3 = "USA",
    sex = "M",
    nallC = 0
  )

  result_empty <- convert_new_ageg(empty_tc, dat_r)
  expect_equal(nrow(result_empty), 0)

  # Test with NA values in intervals
  expect_error(inter_start(c("00_04", NA, "05_09")))

  # Test inter_overlap with edge cases
  expect_equal(inter_overlap(5, 4, 0, 10), 0) # Invalid interval (start > stop)
  expect_equal(inter_overlap(-Inf, 5, 0, 10), 6) # Infinite start
  expect_equal(inter_overlap(0, Inf, 5, 10), 6) # Infinite stop
})
