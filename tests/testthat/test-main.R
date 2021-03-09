context("test-main.R")

#------------------------------------------------
test_that("check_data_entry working correctly", {
  
  x1 <- c(1, 1.230, 1.231, 1e6)
  x2 <- c(1, 1.23, 1.232, 1e6)
  expect_equal(check_data_entry(x1, x2, nsmall = 3), c(0, 0, 1, 0))
  
  x1 <- c(1, NA, NA, NaN, NaN)
  x2 <- c(1, 1, NA, 1, NaN)
  expect_equal(check_data_entry(x1, x2, nsmall = 1), c(0, NA, NA, NA, NA))
  
})
