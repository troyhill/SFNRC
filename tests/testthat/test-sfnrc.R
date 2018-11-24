context("test-sfnrc")
library(SFNRC)

# test_that("interp runs", {
#   expect_that(a <- plyr::join_all(list(finDat, as.data.frame(masterCoords)));
#               coordinates(a) <- c("long", "lat");
#     interp(inputData = a, by = "stn", 
#               paramCol = "param", year = "2016") )
# }) 

test_that("se gives correct output", { 
  expect_equal(se(c(5, 5, 5, NA)), 0)
})

test_that("geoMean gives correct output", {
  expect_equal(geoMean(10), 10)
  expect_equal(geoMean(c(15, 15, 15, NA)), 15)
  expect_error(geoMean(c(15, 15, 15, NA), nas = FALSE)) 
  expect_equal(geoMean(c(15, 15, 15, NA), nas = TRUE, zero.propagate = TRUE), 15) 
  expect_equal(geoMean(c(15, 15, 15, 0), nas = TRUE, zero.propagate = TRUE), 0)
})


test_that("getHydro can error out", {
  expect_error(getHydro(stns = "string")) 
})


test_that("getDataTypes can error out", {
  expect_error(getDataTypes(parameter = "salinity")) 
})