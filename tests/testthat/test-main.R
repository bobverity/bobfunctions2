context("test-main.R")

#------------------------------------------------
test_that("check_data_entry working correctly", {
  
  x1 <- c(1, 1.230, 1.231, 1e6)
  x2 <- c(1, 1.23, 1.232, 1e6)
  expect_equal(check_data_entry(x1, x2), c(0,0,1,0))
  
})
