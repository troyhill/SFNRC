#' @title Create columns indicating wet/dry season and water year 
#'
#' @description See above. Water year 2015 runs from Oct 01 2014 - Sept 30 2015.
#' 
#' @usage seas(inputData, timeCol = "datetime", wetSeas = c("May", "Oct"), waterYearBegin = "Oct")
#' 
#' @param inputData input dataframe 
#' @param timeCol POSIXct column of timestamps
#' @param wetSeas the first and last month of the wet season (inclusive; use abbreviated months as in \code{month.abb}) 
#' @param waterYearBegin the first month of each water year (abbreviated form, as in \code{month.abb}). 
#' 
#' @return two columns are appended to the input dataframe
#' 
#' @examples
#' a <- seas(wqDat, timeCol = "datetime")
#' head(a)
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom stats reshape
#' 
#' @export

seas <- function(inputData, timeCol = "datetime", 
                 wetSeas = c("May", "Oct"), waterYearBegin = "Oct") {
  ### assign wet/dry season status
  wetStart         <- which(month.abb %in% wetSeas[1])
  wetEnd           <- which(month.abb %in% wetSeas[2])
  mos              <- as.numeric(format(as.Date(inputData[, timeCol]), "%m"))
  inputData$seas   <- ifelse(mos %in% c(wetStart:wetEnd), "wet", "dry")
  
  ### assign water year
  yrs               <- as.numeric(format(as.Date(inputData[, timeCol]), "%Y"))
  wtrYrStart        <- which(month.abb %in% waterYearBegin)
  # Dry season where mo < waterYearBegin will be only period sharing water and calendar years
  # wet season will always be previous calendar year. 
  inputData$waterYr <- ifelse(mos < wtrYrStart, yrs, yrs + 1)
  
  invisible(inputData)
}
