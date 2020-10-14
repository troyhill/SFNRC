#' @title Retrieve station coordinates from EDEN
#'
#' @description Scrapes EDEN station coordinate data.
#' 
#' @usage getCoords_EDEN(stn = "S18C_T")
#' 
#' @param stn character string. Case sensitive.
#' 
#' @return dataframe \code{getCoords_EDEN} returns a dataframe with lat/long (epsg:4326) UTM zone 19 coordinates, and the NAVD-to-NGVD conversion factor (units = feet). [ft. NAVD88] + [conversion factor] = [ft. NGVD29]
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
#' stn.coords <- do.call(rbind, lapply(X = stns, getEDENcoords))
#' 
#' ### convert to spatial data and plot
#' stn.coords           <- stn.coords[!is.na(stn.coords$latitude), ]
#' stn.coords$longitude <- -1 * stn.coords$longitude
#' coordinates(stn.coords) <- c("easting", "northing")
#' proj4string(stn.coords) <- CRS('+proj=utm +datum=WGS84')
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


getCoords_EDEN <- function(stn = "S18C_T") {
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
      decimalDegrees        <- clip2[1] + clip2[2] / 60 + clip2[3] / 360
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
  outDF
}

