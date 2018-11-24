context("test-sfnrc")
library(SFNRC)


test_that("interp runs", {
  a <- plyr::join_all(list(finDat, as.data.frame(masterCoords)));
  a <- a[!is.na(a$long), ]
  coordinates(a) <- c("long", "lat")
  proj4string(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  a.interp <- interp(inputData = a, by = "stn", returnRas = TRUE,
                     paramCol = "value", year = "2016")
  
  expect_true(typeof(a.interp) == "S4")
  expect_true(typeof(interp(inputData = a, by = "stn", returnRas = TRUE,
                            interpMethod = "nearest neighbor",
                            paramCol = "value", year = "2016")) == "S4")
})


# test_that("getWQ breaks", {
#   expect_error(getWQ())
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

# 
# test_that("getHydro can error out", {
#   expect_error(getHydro(stns = "string")) 
# })


test_that("getDataTypes can error out", {
  expect_error(getDataTypes(parameter = "salinity")) 
})