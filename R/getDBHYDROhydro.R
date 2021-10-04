#' @title Retrieve R-friendly hydrology data from DBHYDRO using DB keys
#'
#' @description Downloads and lightly processes DBHYDRO data. This function uses DB keys to download datasets and is therefore more versatile than getDBHYDRO.
#' 
#' @usage getDBHYDROhydro(dbkey = "03638", startDate = "19600101",
#' endDate = Sys.Date())
#' 
#' @param dbkey DBkey for station and parameter of interest. Use 'getDBkey()' to find DBkeys associated with a station.
#' @param startDate beginning of period of record, in form YYYYMMDD or YYYY-MM-DD
#' @param endDate end of period of record, in form YYYYMMDD or YYYY-MM-DD
#' 
#' @return dataframe \code{getDBHYDROhydro} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' ### example workflow:
#' getDBkey(stn = "S333", type = "FLOW") # find DBkey for station/parameter of interest
#' s333.daily <- getDBHYDROhydro(dbkey = "15042") # download data for DBkey
#' tail(s333.daily)
#' 
#' ### check that instantaneous data is also loaded properly
#' ### downloading large datasets may take a very long time
#' s333.inst <- getDBHYDROhydro(dbkey = "65086", 
#'    startDate = gsub(x = Sys.Date(), pattern = "-", replacement = ""))
#' tail(s333.inst)
#' }
#' 
#' @importFrom httr GET
#' @importFrom utils read.csv
#'  
#' @export


getDBHYDROhydro <- function(dbkey = "03638", startDate = "19600101",
                            endDate = Sys.Date()) { # format(x = strptime(x = as.character(Sys.Date()), format = "%Y-%m-%d"), "%Y-%m-%d")
  message("\n`getDBHYDROhydro` is deprecated; `getHydro` is preferred\n")
  outDat <- getHydro(dbkey = dbkey, startDate = startDate,
                            endDate = endDate)
  return(outDat)
}
