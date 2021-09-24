#' @title Retrieve station coordinates from EDEN
#'
#' @description Scrapes EDEN station coordinate data.
#' 
#' @usage getCoords_EDEN(stn = "S18C_T", spatial = TRUE)
#' 
#' @param stn character string. Case sensitive.
#' @param spatial logical. If `TRUE`, a spatialPointsDataFrame is returned. If `FALSE`, a dataframe is returned
#' 
#' @return dataframe/spdf \code{getCoords_EDEN} returns a dataframe or spatialPointsDataFrame with lat/long (epsg:4326) UTM zone 19 coordinates, and the NAVD-to-NGVD conversion factor (units = feet). [ft. NAVD88] + [conversion factor] = [ft. NGVD29]
#' 
#' 
#' @examples
#' \dontrun{
#' ### example usage as part of a workflow
#' 
#' stns <- c(
#' "S18C_T",
#' "NESRS1"
#' )
#' stn.coords <- do.call(rbind, lapply(X = stns, getCoords_EDEN))
#' 
#' 
#' plot(stn.coords)
#' }
#' 
#' @importFrom XML  htmlParse
#' @importFrom XML  getNodeSet
#' @importFrom XML  xpathSApply
#' @importFrom XML  saveXML
#'  
#' @export


getCoords_EDEN <- function(stn = "S18C_T", spatial = TRUE) {
  targetURL <- paste0("https://sofia.usgs.gov/eden/station.php?stn_name=", stn)
  tempDoc      <- XML::htmlParse(readLines(targetURL, warn=FALSE),
                                 useInternalNodes = TRUE)
  TempNodes    <- XML::getNodeSet(tempDoc, "//tr")
  
  outDF        <- data.frame(stn = stn, 
                             latitude = NA,
                             longitude= NA,
                             easting  = NA,
                             northing = NA,
                             NAVD_to_NGVD = NA)
  ### hardcoded positions of lat and long coords
  for (i in 1:length(TempNodes)) {
    out2   <- XML::xpathSApply(tempDoc, "//tr", XML::saveXML)[i] # converts to char vector
    if (!grepl(x = out2, pattern = 
               "Latitude|Longitude|Vertical Conversion at Gage|Easting Zone 17|Northing Zone 17")) {
      next
    }
    # stop()
    # get lat/longs
    coordType <- ifelse(grepl(x = out2, pattern = "Latitude"), "latitude", 
                        ifelse(grepl(x = out2, pattern = "Longitude"), "longitude", 
                               ifelse(grepl(x = out2, pattern = "Vertical Conversion at Gage"), "NAVD_to_NGVD", 
                                      ifelse(grepl(x = out2, pattern = "Easting Zone 17"), "easting", 
                                             ifelse(grepl(x = out2, pattern = "Northing Zone 17"), "northing", NA)))))
    
    val       <- strsplit(out2, "</td>\\n    <td>")[[1]][2] # wow, this actually seems to work for all three.
    if (coordType %in% c("northing", "easting")) {
      decimalDegrees       <- as.numeric(strsplit(val, "</td>")[[1]][1]) 
    } else if (coordType %in% c("longitude", "latitude")) {
      val       <- strsplit(val, "\\\"</td>")[[1]][1]
      # vals.char  <- strsplit(clip1, "</td><td>")[[1]][2] # should be, e.g., "30[degree symbol] 1.6"
      ### remove degree symbol
      clip2      <- as.numeric(strsplit(val, "\u00B0|'")[[1]]) # degrees, minutes, seconds
      # decimalDegrees        <- clip2[1] + clip2[2] / 60 + clip2[3] / 360
      degs <- clip2[1]
      mins <- clip2[2]
      secs <- clip2[3]
      decimalDegrees       <- degs + mins / 60 + secs / 3600
    } else if (coordType %in% c("NAVD_to_NGVD")) {
      decimalDegrees       <- -1 * as.numeric(strsplit(val, "</td>")[[1]][1]) # value on website is NGVD to NAVD. I want inverse.
    }
    outDF[, coordType] <- decimalDegrees
    if(!any(is.na(outDF))) {
      # message("breaking loop: iteration ", i)
      break
    }
    # message("continuing loop: iteration ", i)
  }
  if (spatial) {
    ### convert to spatial data
    outDF <- makeSpatialCoords(outDF, format = "eden")
  }
  outDF
}

