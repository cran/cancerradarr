# Tests for run_dynamic_report function

test_that("run_dynamic_report finds correct Rmd template", {
  # Check that the expected Rmd file exists in package
  expected_rmd <- system.file('extdata/dynamic_report.Rmd', package = "cancerradarr")
  expect_true(file.exists(expected_rmd))
})
