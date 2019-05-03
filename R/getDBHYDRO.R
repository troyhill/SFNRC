#' @title Produce R-friendly water quality data from DBHYDRO
#'
#' @description Downloads and lightly processes DBHYDRO data to make it easier to work with in R.
#' 
#' @usage getDBHYDRO(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P", "TURBIDITY"), 
#'     removeFlaggedData = TRUE, outputType = "wide")
#' 
#' @param stn character string. Case insensitive.
#' @param removeFlaggedData Logical. Indicates whether flagged data should be removed
#' @param parameters water quality parameters to report. Set to NA to report all available parameters.
#' @param outputType output type can be "full", "long", or "wide". "full" is a long dataset with all samples (possibility of multiple samples on a single day). "long" and "wide" average duplicate samples and are either long datasets (one column with parameter names and one column with parameter values) or wide datasets (one column of values for each parameter). The "long" form reports more information (units, MDL, PQL, RDL) than the "wide" form, which only reports values for each parameter.
#' 
#' @return dataframe \code{getDBHYDRO} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' longDat <- getDBHYDRO(stn = "s333", outputType = "long")
#' wideDat <- getDBHYDRO(stn = "s333", outputType = "wide")
#' }
#' 
#' @importFrom utils read.delim
#' @importFrom stats reshape
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#'  
#' @export


getDBHYDRO <- function(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P", "TURBIDITY"), 
                       removeFlaggedData = TRUE, outputType = "wide") {
  
  stn <- toupper(stn)
  parameters <- toupper(parameters)
  
  a <- SFNRC::dbhydro.stn(stations = stn, import = TRUE)
  names(a)[names(a) %in% "Station.ID"]  <- "stn"
  names(a)[names(a) %in% "Test.Name"]   <- "param"
  names(a)[names(a) %in% "Value"]       <- "value"
  names(a)[names(a) %in% "Uncertainty"] <- "uncertainty"
  names(a)[names(a) %in% "Units"]       <- "units"
  
  ### remove flagged data
  #  "PMF" "yes" "J3"  "J"   "V"   "Q"   "K"   "J5"
  # head(a[a$Flag %in% "yes", ]) 
  # unique(a[a$Flag %in% "yes", c("Result.Comments")]) 
  # unique(a[a$Flag %in% "J5", c("Result.Comments")]) 
  if (removeFlaggedData) {
    a <- a[grepl(x = a$Flag, pattern = "$^"), ]
  }
  
  ### remove everything but surface water samples (lose field and equipment blanks)
  a <- a[grepl(x = a$Matrix, pattern = "SW") & grepl(x = a$Sample.Type.New, pattern = "SAMP"), ]
   
  ### process date data
  a$date <- NA 
  for (i in 1:nrow(a)) {
    a$date[i] <- ifelse(grepl(x = a$First.Trigger.Date[i], pattern = "$^") | is.na(a$First.Trigger.Date[i]), a$Collection_Date[i], a$First.Trigger.Date[i])
  }
  # gsub("([a-zA-Z]{2}-.*)", "\\L\\1", a$date[1], perl=TRUE) # really tough to convert all-cap month into title case month inside of a string.
  
  a$date <- gsub(pattern = "([a-zA-Z]{2}-.*)", replacement = "\\L\\1", x = a$date, perl=TRUE)
  a$date <- as.POSIXct(strptime(a$date, format = "%d-%b-%Y %H:%M"))
  
  if (sum(is.na(parameters)) > 0) {
    parameters <- unique(a$param)
  }
  a <- a[a$param %in% parameters, c("stn", "date", "param", "value", "uncertainty", "MDL", "PQL", "RDL", "units")]
  
  
  ### average measurements on same day
  tempDat <- plyr::ddply(a, c("stn", "date", "param", "MDL", "PQL", "RDL", "units"), plyr::summarise, 
                         value = mean(value, na.rm = TRUE))
  
  ### reshape dataset to one column per parameter 
  wideDat  <- stats::reshape(tempDat[, c("stn", "date", "param", "value")],
                             idvar = c("stn", "date"),
                             timevar = "param", direction = "wide")
  names(wideDat) <- gsub(x = names(wideDat), pattern = "value.| |,", replacement = "")
  # tail(wideDat)
  
  if (outputType %in% "full") {
    ### may include multiple samples on a single day
    outDat <- a
  }
  
  if (outputType %in% "long") {
    ### averages multiple samples on a single day but still reports in long form
    outDat <- tempDat
  }
  
  if (outputType %in% "wide") {
    outDat <- wideDat
  }
  
  invisible(outDat)
}
