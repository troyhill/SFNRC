#' @title Retrieve DBkeys from DBHYDRO
#'
#' @description Retrieves all DBkeys associated with a given station.
#' 
#' @usage getDBkey(stn = "S333", type = "all", freq = "all")
#' 
#' @param stn character string. Case insensitive.
#' @param type type of data desired, e.g., "FLOW" or "STG". Must be a perfect match, so it's a good idea to leave this set to "all".
#' @param freq desired data frequency, e.g., "DA" or "INST". Must be a perfect match, so it's a good idea to leave this set to "all".
#' 
#' @return dataframe \code{getDBkey} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' getDBkey(stn = "S12d", type = "FLOW")
#' }
#' 
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#'  
#' @export


getDBkey <- function(stn = "S333", type = "all", freq = "all") {
  url.init <- paste0("http://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_station=", toupper(stn), "%25&v_js_flag=N")
  theurl <- RCurl::getURL(url.init, .opts = list(ssl.verifypeer = FALSE) )
  tables <- XML::readHTMLTable(theurl)
  
  info <- tables[[length(tables) - 1]]
  if (!type == "all") {
    info <- info[info$`Data Type` == type, ]
  }
  if (!freq == "all") {
    info <- info[info$Freq == freq, ]
  }
  outDat <-   info[, c(2:3, 5:8, 11:12)]
  return(outDat)
}