#' @title Reports available water quality parameters from DBHYDRO
#'
#' @description a simple download-and-query, designed to get parameter names correct in future downloads
#' 
#' @usage getDBHYDROparams(stn = "S333", parameters = NA)
#' 
#' @param stn character string. Case insensitive.
#' @param parameters a grep-style character string identifying water quality parameters to report. Set to NA to report all available parameters. Partial matches are made (e.g., "TURB" and "TURBIDITY" will both yield turbidity data)
#' 
#' @return dataframe \code{getDBHYDROparams} returns a vector of parameter names and the number of observations for each.
#' 
#' 
#' @examples
#' \dontrun{
#' tableDat <- getDBHYDROparams(stn = "s333") # all parameters
#' tableDat
#' 
#' phosDat <- getDBHYDROparams(stn = "s333", parameters = "PHOS") # using parameter filter
#' phosDat
#' }
#' 
#'  
#' @export


getDBHYDROparams <- function(stn = "S333", parameters = NA) {
  
  stn        <- toupper(stn)
  parameters <- toupper(parameters)
  
  a          <- SFNRC::dbhydro.stn(stations = stn, import = TRUE)
  
  i    <- sapply(a, is.factor) # convert factor columns to character
  a[i] <- lapply(a[i], as.character)
  
  names(a)[names(a) %in% "Station.ID"]  <- "stn"
  names(a)[names(a) %in% "Test.Name"]   <- "param"
  names(a)[names(a) %in% "Value"]       <- "value"
  
  if (is.na(parameters)) {
    outDat <- table(a$param)
  } else {
    outDat <- table(a$param[grepl(x = a$param, pattern = parameters)])
  }
  
  invisible(outDat)
}
