# Tests for read_cancerradar_output_02 function

test_that("read_cancerradar_output_02 returns empty tibble for non-existent aggregation level", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_02(filename.out, 'non_existent_level')
    
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_equal(names(result), c("reg_label", "sex", "ageg", "can", "index", "est", "lci", "uci", "ageg_sta", "ageg_sto", "ageg_mid"))
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_02 works with example file and un_region", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_02(filename.out, 'un_region')
    
    expect_s3_class(result, "tbl_df")
    expect_equal(names(result), c("reg_label", "sex", "ageg", "can", "index", "est", "lci", "uci", "ageg_sta", "ageg_sto", "ageg_mid"))
    
    if(nrow(result) > 0) {
      # Check column types
      expect_true(is.factor(result$reg_label))
      expect_true(all(result$sex %in% c("male", "female", NA)))
      expect_true(is.character(result$ageg))
      expect_true(is.character(result$can))
      expect_true(all(result$index %in% c("ir", "pr", NA)))
      expect_true(is.numeric(result$est))
      expect_true(is.numeric(result$lci))
      expect_true(is.numeric(result$uci))
      expect_true(is.numeric(result$ageg_sta))
      expect_true(is.numeric(result$ageg_sto))
      expect_true(is.numeric(result$ageg_mid))
      
      # Check age group calculations
      expect_true(all(result$ageg_mid == (result$ageg_sta + result$ageg_sto) / 2, na.rm = TRUE))
      expect_true(all(result$ageg_sta <= result$ageg_sto, na.rm = TRUE))
      
      # Check that reg_label has expected structure
      if(length(levels(result$reg_label)) > 0) {
        expect_equal(levels(result$reg_label)[1], "general population")
      }
    }
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_02 works with cob_iso3 (default)", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result_default <- read_cancerradar_output_02(filename.out)
    result_explicit <- read_cancerradar_output_02(filename.out, 'cob_iso3')
    
    expect_equal(result_default, result_explicit)
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_02 handles age group parsing correctly", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_02(filename.out, 'un_region')
    
    if(nrow(result) > 0) {
      # Test age group format (should be like "20_24", "25_29", etc.)
      age_groups <- unique(result$ageg[!is.na(result$ageg)])
      
      for(ageg in age_groups) {
        if(grepl("_", ageg)) {
          parts <- strsplit(ageg, "_")[[1]]
          if(length(parts) == 2) {
            start_age <- as.numeric(parts[1])
            stop_age <- as.numeric(parts[2])
            expect_true(start_age <= stop_age)
            
            # Find corresponding rows
            ageg_rows <- result[result$ageg == ageg & !is.na(result$ageg), ]
            if(nrow(ageg_rows) > 0) {
              expect_equal(unique(ageg_rows$ageg_sta), start_age)
              expect_equal(unique(ageg_rows$ageg_sto), stop_age)
              expect_equal(unique(ageg_rows$ageg_mid), (start_age + stop_age) / 2)
            }
          }
        }
      }
    }
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_02 contains only expected index values", {
  filename.out <- system.file('extdata/ex_cancerRADAR_output.xlsx', package = "cancerradarr")
  
  if(file.exists(filename.out)) {
    result <- read_cancerradar_output_02(filename.out, 'un_region')
    
    if(nrow(result) > 0) {
      unique_indices <- unique(result$index[!is.na(result$index)])
      expect_true(all(unique_indices %in% c("ir", "pr")))
    }
  } else {
    skip("Example output file not found")
  }
})

test_that("read_cancerradar_output_02 errors appropriately with non-existent file", {
  expect_error(
    read_cancerradar_output_02("non_existent_file.xlsx", "un_region")
  )
})