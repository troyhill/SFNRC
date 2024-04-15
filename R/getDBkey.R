#' @title Retrieve DBkeys from DBHYDRO
#'
#' @description Retrieves all DBkeys associated with a given station.
#' 
#' @usage getDBkey(stn = "S333", type = "all", freq = "all", activeOnly = TRUE)
#' 
#' @param stn character string. Case insensitive.
#' @param type type of data desired, e.g., "FLOW" or "STG". Must be a perfect match, so it's a good idea to leave this set to "all".
#' @param freq desired data frequency, e.g., "DA" or "INST". Must be a perfect match, so it's a good idea to leave this set to "all".
#' @param activeOnly logical (default = TRUE). If set to TRUE, only active DBKeys are reported. "Active" is defined as having new data within the last three months (to accommodate lengthy pandemic disruptions)
#' 
#' @return dataframe \code{getDBkey} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' getDBkey(stn = "S12d", type = "FLOW")
#' }
#' 
#' @importFrom XML readHTMLTable
#' @importFrom XML htmlParse
#' @importFrom XML getNodeSet
#'  
#' @export


getDBkey <- function(stn = "S333", type = "all", freq = "all", activeOnly = TRUE) {
  url.init <- paste0("http://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_station=", toupper(stn), "%25&v_js_flag=N")

  tempDoc      <- XML::htmlParse(readLines(url.init, warn=FALSE),
                                 useInternalNodes = TRUE)
  TempNodes    <- XML::getNodeSet(tempDoc, "//tr")
  tables <- XML::readHTMLTable(tempDoc)
  
  ### identify which table to use
  TableToUse <- which(sapply(X = tables, FUN = function(x) sum(names(x) %in% c("Dbkey", "Station"))) == 2)
  info <- tables[[TableToUse]]
  ### old RCurl approach (removed 20201013):
  # theurl <- RCurl::getURL(url.init, .opts = list(ssl.verifypeer = FALSE) )
  # tables <- XML::readHTMLTable(theurl)
  # info <- tables[[length(tables) - 1]]
  if (!type == "all") {
    info <- info[info$`Data Type` == type, ]
  }
  if (!freq == "all") {
    info <- info[info$Freq == freq, ]
  }
  cols_to_keep <- grep(x = tolower(names(info)), 
                       pattern = '^dbkey$|^station$|^site$|^data type$|^freq$|^stat$|^start date$|^end date$|^sensor$|^latitude$|^longitude$|^x coord$|^y coord$')
  outDat <-   info[, cols_to_keep]
  
  ### format as dates for reporting only active time series
  # dateCols <- outDat[, grep(x = tolower(names(outDat)), pattern = "date")]
  # ### de-capitalize the 5th-6th characters)
  # dateCols <- apply(X = dateCols, FUN = function(x) paste0(substr(x, 1, 4), 
  #                                              tolower(substr(x, 5, 6)),
  #                                              substr(x, 7, nchar(x))), MARGIN = 2)
  dateCols <- grep(x = tolower(names(outDat)), pattern = "date")
  for (j in 1:length(dateCols)) {
    outDat[, dateCols[j]] <- as.Date(as.character(tolower(outDat[, dateCols[j]])), format = "%d-%b-%Y")
  }
  
  if (activeOnly == TRUE) {
    outDat <- outDat[(outDat$`End Date` >= (Sys.Date() - 90)) & (!is.na(outDat$`End Date`)), ]
  } 
  
  return(outDat)
}