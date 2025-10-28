# library(testthat)
# library(cancerradarr)

test_that('Input file creation', {
  file.in <- 'input_file_test.xlsx'
  create_registry_input_file(file.in)
  expect_true(file.exists(file.in))
  unlink(file.in)
})

test_that('Output file creation', {
  # skip_on_ci()
  # skip_on_cran()
  file.in <- system.file(
    "extdata",
    "ex_cancerRADAR_input_filled.xlsx",
    package = "cancerradarr"
  )
  file.out <- paste0(tempfile(), '.xlsx')
  create_canradar_summary_file(
    file.in,
    file.out,
    20,
    100,
    include.by.cob.stat = FALSE
  )
  expect_true(file.exists(file.out))

  # Expected HTML output path
  report.out <- file.out |> stringr::str_replace('.xlsx$', '.html')

  # Remove HTML file if it already exists
  if (file.exists(report.out)) {
    unlink(report.out)
  }

  # Run the function
  create_static_report(file.out)

  # Check that HTML file was created
  expect_true(file.exists(report.out))

  # Cleanup
  unlink(report.out)
  unlink(file.out)
})
