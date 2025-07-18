# library(testthat)
# library(cancerradarr)

test_that(
  'Input file creation',
  {
    file.in <- 'input_file_test.xlsx'
    create_registry_input_file(file.in)
    expect_true(file.exists(file.in))
    unlink(file.in)
  }
)

test_that(
  'Output file creation',
  {
    skip_on_ci()
    file.in <- system.file("extdata", "ex_cancerRADAR_input_filled.xlsx", package = "cancerradarr")
    file.out <- paste0(tempfile(), '.xlsx')
    create_canradar_summary_file(file.in, file.out, 20, include.by.cob.stat = FALSE)
    expect_true(file.exists(file.out))
    unlink(file.out)
  }
)