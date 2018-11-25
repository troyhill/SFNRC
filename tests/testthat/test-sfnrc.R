context("test-sfnrc")
library(SFNRC)


test_that("interp runs", {
  a <- plyr::join_all(list(finDat, as.data.frame(masterCoords)));
  a <- a[!is.na(a$long), ]
  coordinates(a) <- c("long", "lat")
  proj4string(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  a.interp <- interp(inputData = a, by = "stn", returnRas = TRUE, plotZLims = c(0, 10),
                     paramCol = "value", year = "2016")
  
  expect_true(typeof(a.interp) == "S4")
  expect_true(typeof(interp(inputData = a, by = "stn", returnRas = TRUE,
                            interpMethod = "nearest neighbor",
                            paramCol = "value", year = "2016")) == "S4")
})


test_that("getWQ breaks", {
  expect_error(getWQ(stns = "S333", target_analytes = c("string1", "string2")))
  expect_error(getWQ(stns = "S333", matricesToExclude = c("string1", "string2")))
  expect_error(getWQ(stns = "S333", output_colNames = c("string1", "string2")))
  expect_error(getWQ(stns = "S333", output_colClasses = c("string1", "string2")))
  expect_error(getWQ(stns = "S333", rFriendlyParamNames = "string1"))
  expect_error(getWQ(stns = 20))
})


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
  expect_error(getHydro(stns = 20))
  expect_error(getHydro(stns = "S333", parameter_list = 20))
  expect_error(getHydro(stns = "S333", data_shape = 20))
  expect_error(getHydro(stns = "S333", data_shape = "sort_of_wide"))
  expect_error(getHydro(stns = "S333", getWaterQuality = "string1"))
})


test_that("getDataTypes can error out", {
  expect_error(getDataTypes(parameter = "salinity"))
})

test_that("getStn can error out", {
  expect_error(getStn(query = 20))
  expect_error(getStn(query = c("string1", "string2")))
})


test_that("dbhydro.stn input checks", {
  expect_error(dbhydro.stn(rename_proj = 20))
  expect_error(dbhydro.stn(incl_qc_flags = "string"))
  expect_error(dbhydro.stn(incl_flagged_data = "string"))
  expect_error(dbhydro.stn(import_data = "string"))
  expect_error(dbhydro.stn(report_type = "string"))
  # expect_error(dbhydro.stn(report_type = "crosstab"))
  # expect_error(dbhydro.stn(end_date = "today"))
})


test_that("dbhydro.proj input checks", {
  expect_error(dbhydro.proj(destfile = 20))
  expect_error(dbhydro.proj(project_codes = 20))
  expect_error(dbhydro.proj(start_date = 20))
  expect_error(dbhydro.proj(import_data = "string"))
  expect_error(dbhydro.proj(rename_proj = "string"))
  expect_error(dbhydro.proj(destination = 20))
})


test_that("dbhydro.stn.batch test", {
  expect_error(dbhydro.stn.batch(codes = 20))
  expect_error(dbhydro.stn.batch(codes = "string", report_type = "string"))
  
})

# test_that("seas tests", {
#   a <- seas(wqDat, timeCol = "datetime")
#   head(a)
#   
#   expect_error(seas())
#   expect_error(seas())
# })