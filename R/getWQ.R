#' @title Retrieve R-friendly water quality data from DBHYDRO
#'
#' @description Downloads and lightly processes DBHYDRO water quality data.
#' 
#' @usage getWQ(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"), 
#'     removeFlaggedData = TRUE, outputType = "wide")
#' 
#' @param stn character string. Case insensitive.
#' @param removeFlaggedData Logical. Indicates whether flagged data should be removed
#' @param parameters a grep-style character string identifying water quality parameters to report. Set to NA to report all available parameters. Partial matches are made (e.g., "TURB" and "TURBIDITY" will both yield turbidity data)
#' @param outputType output type can be "full", "long", or "wide". "full" is a long dataset with all samples (possibility of multiple samples on a single day). "long" and "wide" average duplicate samples and are either long datasets (one column with parameter names and one column with parameter values) or wide datasets (one column of values for each parameter). The "long" form reports more information (units, MDL, PQL, RDL) than the "wide" form, which only reports values for each parameter.
#' 
#' @return dataframe \code{getWQ} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' longDat <- getWQ(stn = "s333", outputType = "long")
#' wideDat <- getWQ(stn = "s333", outputType = "wide")
#' }
#' 
#' @importFrom stats reshape
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#'  
#' @export


getWQ <- function(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"), 
                       removeFlaggedData = TRUE, outputType = "wide") {
  stn        <- toupper(stn)
  parameters <- toupper(parameters)
  
  a          <- SFNRC::dbhydro.stn(stations = stn, 
                                   rename_proj = TRUE,
                                   parameters = "all",
                                   report_type = "full", # full = long dataset - 1 line per sample; crosstab = wide dataset
                                   incl_qc_flags = TRUE,
                                   incl_flagged_data = removeFlaggedData,
                                   destination = "file_csv", 
                                   start_date = "01-JAN-1960", # note format
                                   end_date   = "today",
                                   import_data = TRUE)
  # Sys.sleep(2)
  a$Test.Name <- toupper(a$Test.Name) # not all params are capitalized in DBHydro (FLAB04, Temperature) so this is needed to make query case0insensitive
  i    <- sapply(a, is.factor) # convert factor columns to character
  a[i] <- lapply(a[i], as.character)
  
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
  a <- a[grepl(x = a$Matrix, pattern = "SW|SA") & grepl(x = a$Sample.Type.New, pattern = "SAMP"), ]
  
  ### process date data
  a$datetime <- NA 
  for (i in 1:nrow(a)) {
    a$datetime[i] <- ifelse(grepl(x = a$First.Trigger.Date[i], pattern = "$^") | is.na(a$First.Trigger.Date[i]), a$Collection_Date[i], a$First.Trigger.Date[i])
  }
  # gsub("([a-zA-Z]{2}-.*)", "\\L\\1", a$datetime[1], perl=TRUE) # really tough to convert all-cap month into title case month inside of a string.
  
  a$datetime <- gsub(pattern = "([a-zA-Z]{2}-.*)", replacement = "\\L\\1", x = a$datetime, perl=TRUE)
  a$datetime <- as.POSIXct(strptime(a$datetime, format = "%d-%b-%Y %H:%M"))
  a$year     <- as.numeric(substr(a$datetime, 1, 4))
  a$mo       <- as.numeric(substr(a$datetime, 6, 7))
  a$day      <- as.numeric(substr(a$datetime, 9, 10))
  a$date     <- as.POSIXct(substr(a$datetime, 1, 10), format = "%Y-%m-%d")
  a$time     <- paste0(substr(a$datetime, 12, 13), substr(a$datetime, 15, 16))
  
  if (sum(is.na(parameters)) > 0) {
    parameters <- unique(a$param)
  }
  a         <- a[grepl(x = a$param, pattern = parameters), c("stn", "datetime", "date", "time", "year", "mo", "day", "param", "value", "uncertainty", "MDL","units")]
  
  
  if (outputType %in% "full") {
    ### may include multiple samples on a single day
    outDat <- a
  } else {
    ### average measurements on same day
    tempDat  <- plyr::ddply(a, c("stn", "date", "year", "mo", "day", "param", "MDL", "units"), plyr::summarise, 
                            value = mean(get("value"), na.rm = TRUE))
    
    # ?anyDuplicated(tempDat[, c("stn", "date", "year", "mo", "day", "param")])
    # tempDat[tempDat$date == "2006-09-26", ]
  }
  
  if (outputType %in% "long") {
    ### averages multiple samples on a single day but still reports in long form
    outDat <- tempDat
  }
  
  if (outputType %in% "wide") {
    ### reshape dataset to one column per parameter 
    wideDat  <- stats::reshape(tempDat[, c("stn", "date", "year", "mo", "day", "param", "value")],
                               idvar = c("stn", "date", "year", "mo", "day"),
                               timevar = "param", direction = "wide")
    names(wideDat) <- gsub(x = names(wideDat), pattern = "value.| |,", replacement = "")
    # tail(wideDat)
    
    outDat <- wideDat
  }
  
  invisible(outDat)
}
