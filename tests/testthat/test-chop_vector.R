# Tests for chop_vector function

test_that("chop_vector works with size 1", {
  result <- chop_vector(1)
  expected <- matrix(1, nrow = 1, ncol = 1)
  colnames(expected) <- "Var1"
  
  expect_equal(result, expected)
  expect_equal(ncol(result), 1)
  expect_equal(nrow(result), 1)
})

test_that("chop_vector works with size 2", {
  result <- chop_vector(2)
  
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), c("Var1", "Var2"))
  
  # Check that all combinations are valid (monotonic increasing)
  for(i in 1:nrow(result)) {
    expect_true(all(diff(result[i, ]) >= 0))
    expect_true(all(diff(result[i, ]) <= 1))
  }
})

test_that("chop_vector works with size 3", {
  result <- chop_vector(3)
  
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("Var1", "Var2", "Var3"))
  expect_true(nrow(result) > 0)
  
  # Check that all combinations are valid (monotonic increasing with step <= 1)
  for(i in 1:nrow(result)) {
    diffs <- diff(result[i, ])
    expect_true(all(diffs >= 0 & diffs <= 1))
  }
})

test_that("chop_vector returns correct matrix format", {
  result <- chop_vector(2)
  
  expect_true(is.matrix(result))
  expect_true(is.numeric(result))
})

test_that("chop_vector default parameter works", {
  result_default <- chop_vector()
  result_explicit <- chop_vector(3)
  
  expect_equal(result_default, result_explicit)
})

test_that("chop_vector with larger sizes", {
  result <- chop_vector(4)
  
  expect_equal(ncol(result), 4)
  expect_true(nrow(result) > 0)
  
  # Verify all values are within expected range
  expect_true(all(result >= 1))
  expect_true(all(result <= 4))
})