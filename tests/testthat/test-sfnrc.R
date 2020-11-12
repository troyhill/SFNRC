context("test-sfnrc")
library(SFNRC)



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

test_that("dbhydro.stn input checks", {
  expect_error(dbhydro.stn(rename_proj = 20))
  expect_error(dbhydro.stn(incl_qc_flags = "string"))
  expect_error(dbhydro.stn(incl_flagged_data = "string"))
  expect_error(dbhydro.stn(import_data = "string"))
  expect_error(dbhydro.stn(report_type = "string"))
  expect_error(dbhydro.stn(report_type = c("string1", "string2") ))
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
})


test_that("dbhydro.proj.batch test", {
  expect_error(dbhydro.proj.batch(codes = 20))
})


test_that("seas tests", {
  a <- seas(wqDat[1:100, ], timeCol = "datetime")
  head(a)

  expect_equal(length(seas(wqDat[1:100, ], timeCol = "datetime")$seas), 100)
  expect_error(seas(wqDat[1:100, ], timeCol = 20))
  expect_error(seas(wqDat[1:100, ], timeCol = "not a column name"))
  expect_error(seas(wqDat[1:100, ], timeCol = "year")) # column exists in dataset but isn't POSIXct
  
  expect_error(seas(wqDat[1:100, ], waterYearBegin = 20))
  expect_error(seas(wqDat[1:100, ], waterYearBegin = "string"))
  expect_error(seas(wqDat[1:100, ], wetSeas = 20))
})
