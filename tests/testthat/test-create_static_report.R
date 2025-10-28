# Tests for create_static_report function
test_that("returning error if file does not exist", {
  # skip_on_cran()

  expect_error(
    create_static_report("non_existent_file.xlsx")
  )
})


test_that("create_static_report works with example output file", {
  # skip_on_cran()

  # Use the example output file from the package
  filename.out <- system.file(
    'extdata/ex_cancerRADAR_output.xlsx',
    package = "cancerradarr"
  )

  if (file.exists(filename.out)) {
    # Create a temporary copy to avoid modifying package files
    temp_dir <- tempdir()
    temp_file <- file.path(temp_dir, "test_output.xlsx")
    file.copy(filename.out, temp_file)

    # Expected HTML output path
    expected_html <- file.path(temp_dir, "test_output.html")

    # Remove HTML file if it already exists
    if (file.exists(expected_html)) {
      unlink(expected_html)
    }

    # Run the function
    create_static_report(temp_file)

    # Check that HTML file was created
    expect_true(file.exists(expected_html))

    # Cleanup
    unlink(temp_file)
    unlink(expected_html)
  } else {
    skip("Example output file not found")
  }
})
