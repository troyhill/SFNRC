context("test-sfnrc")
library(SFNRC)



test_that("se gives correct output", {
  expect_equal(se(c(5, 5, 5, NA)), 0)
  expect_error(se("string")) 
})

test_that("geoMean gives correct output", {
  expect_equal(geoMean(10), 10)
  expect_equal(geoMean(c(15, 15, 15, NA)), 15)
  expect_error(geoMean(c(15, 15, 15, NA), nas = FALSE)) 
  expect_equal(geoMean(c(15, 15, 15, NA), nas = TRUE, zero.propagate = TRUE), 15) 
  expect_equal(geoMean(c(15, 15, 15, 0), nas = TRUE, zero.propagate = TRUE), 0)
})


# test_that("fld.frq gives correct output", {
#   expect_equal(fld.frq(2, 1:10, units = "percent"), 0.8)
#   expect_equal(fld.frq(2, 1:10, units = "tides"), 9)
#   expect_error(fld.frq("string", 1:10)) 
#   expect_error(fld.frq(2, "string"))
#   expect_error(fld.frq(2, 1:10, units = "furlongs")) 
# })
