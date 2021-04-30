#' @title Create columns indicating wet/dry season and water year 
#'
#' @description Federal water year 2022 runs from 01 Oct 2021 - 30 Sept 2022 (beginning with the dry season). Florida water year 2022 runs from 01 May 2021 - 30 April 2022 (beginning with the wet season).
#' 
#' @usage seas(inputData, timeCol = "datetime", wetSeas = c("May", "Sep"), waterYearBegin = month.abb[which(month.abb %in% wetSeas[2]) + 1])
#' 
#' @param inputData input dataframe 
#' @param timeCol POSIXct column of timestamps
#' @param wetSeas the first and last month of the wet season (inclusive; use abbreviated months as in \code{month.abb}) 
#' @param waterYearBegin the first full month of each water year (abbreviated form, as in \code{month.abb}, e.g., "Oct"). Default behavior is consistent with the federal water year, where a new WY begins at the start of the dry season (Oct).
#' 
#' @return two columns are appended to the input dataframe
#' 
#' @examples
#' a <- seas(head(wqDat), timeCol = "datetime")
#' head(a)
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom stats reshape
#' 
#' @export

seas <- function(inputData, timeCol = "datetime", 
                 wetSeas = c("May", "Sep"), waterYearBegin = month.abb[which(month.abb %in% wetSeas[2]) + 1]) {
  
  if (!is.character(timeCol) || !(length(timeCol) == 1)) {
    stop("'timeCol' must be a single character string naming the column with date/time info")
  }
  if (!(timeCol) %in% names(inputData)) {
    stop("'timeCol' is not the name of a column in your dataset, 'inputData'")
  }
  if (!inherits(inputData[, timeCol], "POSIXct") ) { # serves as 'is.POSIXct' per https://stackoverflow.com/a/26413765
    stop("'timeCol' is not a POSIXct data type. See ?as.POSIXct for more info.")
  }
  if (!is.character(wetSeas) || !(length(wetSeas) == 2)) {
    stop("'wetSeas' must be a two-element character vector with the first and last full months of the wet season")
  }
  if (!is.character(waterYearBegin) || !(length(waterYearBegin) == 1)) {
    stop("'waterYearBegin' must be a single character string naming the first full month of the water year")
  }
  
  if (sum(c(wetSeas, waterYearBegin) %in% month.abb) < 3) {
    stop("values in 'wetSeas' and 'waterYearBegin' appear incorrect. Make sure they appear in the 'month.abb' object")
  }
  
  
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
