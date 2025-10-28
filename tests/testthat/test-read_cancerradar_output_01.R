# Tests for read_cancerradar_output_01 function

test_that("read_cancerradar_output_01 returns empty tibble for non-existent aggregation level", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_01(filename.out, 'non_existent_level')
    
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_equal(names(result), c("reg_label", "sex", "ageg", "can", "ref", "index", "est", "lci", "uci"))
    
    # Check column types
    expect_true(is.factor(result$reg_label))
    expect_true(is.character(result$sex))
    expect_true(is.character(result$ageg))
    expect_true(is.character(result$can))
    expect_true(is.character(result$ref))
    expect_true(is.character(result$index))
    expect_true(is.numeric(result$est))
    expect_true(is.numeric(result$lci))
    expect_true(is.numeric(result$uci))
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_01 works with example file and un_region", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_01(filename.out, 'un_region')
    
    expect_s3_class(result, "tbl_df")
    expect_equal(names(result), c("reg_label", "sex", "ageg", "can", "ref", "index", "est", "lci", "uci"))
    
    if(nrow(result) > 0) {
      # Check that all expected columns are present with correct types
      expect_true(is.factor(result$reg_label))
      expect_true(all(result$sex %in% c("male", "female", NA)))
      expect_true(is.character(result$ageg))
      expect_true(is.character(result$can))
      expect_true(is.character(result$ref))
      expect_true(is.character(result$index))
      expect_true(is.numeric(result$est))
      expect_true(is.numeric(result$lci))
      expect_true(is.numeric(result$uci))
      
      # Check that reg_label has expected structure (general population should be first level)
      if(length(levels(result$reg_label)) > 0) {
        expect_equal(levels(result$reg_label)[1], "general population")
      }
    }
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_01 works with cob_iso3 (default)", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result_default <- read_cancerradar_output_01(filename.out)
    result_explicit <- read_cancerradar_output_01(filename.out, 'cob_iso3')
    
    expect_equal(result_default, result_explicit)
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_01 handles reference population correctly", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_01(filename.out, 'un_region')
    
    if(nrow(result) > 0) {
      # Check that ref column contains expected values
      expected_refs <- c("registry", "globocan national", "globocan eu27", "globocan euUN")
      expect_true(all(result$ref %in% expected_refs))
      
      # Check that index names are cleaned (no _glonat, _gloeu27, _gloeuUN suffixes)
      index_names <- unique(result$index)
      expect_false(any(grepl("_glonat|_gloeu27|_gloeuUN", index_names)))
    }
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_01 errors appropriately with non-existent file", {
  expect_error(
    read_cancerradar_output_01("non_existent_file.xlsx", "un_region")
  )
})