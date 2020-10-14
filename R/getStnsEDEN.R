#' @title Retrieve station coordinates from EDEN
#'
#' @description Scrapes EDEN station coordinate data.
#' 
#' @usage getStns_EDEN(url = "https://sofia.usgs.gov/eden/stationlist-area.php?area=WCA3")
#' 
#' @param url character string. Web address with EDEN stations (e.g., https://sofia.usgs.gov/eden/stationlist-area.php?area=WCA3).
#' 
#' @return dataframe \code{getCoords_EDEN} returns a dataframe with EDEN stations in the requested region.
#' 
#' 
#' @examples
#' \dontrun{
#' tst <- getStns_EDEN()
#' ### generate list of all stations
#' areas <- c("WCA1", "WCA2", "WCA3", "BCNP", "ENP", "Pennsuco", "FLBay", "GOM")
#' area.urls <- c(paste0("https://sofia.usgs.gov/eden/stationlist-area.php?area=", areas))
#' stn.list <- lapply(X = area.urls, getStns_EDEN)
#' stns     <- do.call(c, stn.list)
#' }
#' 
#' @importFrom XML  htmlParse
#' @importFrom XML  getNodeSet
#' @importFrom XML  xpathSApply
#' @importFrom XML  saveXML
#'  
#' @export

getStns_EDEN <- function(url = "https://sofia.usgs.gov/eden/stationlist-area.php?area=WCA3") {
  targetURL    <- url
  tempDoc      <- XML::htmlParse(readLines(targetURL, warn=FALSE),
                                 useInternalNodes = TRUE)
  TempNodes    <- XML::getNodeSet(tempDoc, "//tr")
  
  # strsplit(as.character(TempNodes[[50]]), "stn_name=")[[1]][1]
  
  outVals <- NULL
  i <- 50
  ### hardcoded positions of lat and long coords
  for (i in 1:length(TempNodes)) {
    out2   <- XML::xpathSApply(tempDoc, "//tr", XML::saveXML)[i] # converts to char vector
    # get parameter name
    clip1      <- strsplit(out2, "stn_name=")[[1]][2]
    vals.char  <- strsplit(clip1, "\\\">|&amp")[[1]][1] # S150T has a wierd string fter it
    
    if ((!is.na(vals.char)) & (nchar(vals.char) > 100)) { # this removes a spurious entry appearing at the top of each vector
      vals.char <- NA
    }
    
    ### now combine, if there's a value to combine
    if (!is.na(vals.char)) {
      if (is.null(outVals)) {
        outVals  <- vals.char
      } else {
        outVals  <- c(outVals, vals.char)
      }
    }
  }
  as.vector(outVals)
}



