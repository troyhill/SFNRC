#' @title Retrieve station coordinates from DBHYDRO
#'
#' @description Downloads DBHYDRO station coordinate data. Note that these coordinates may be incorrect in DBHYDRO (e.g., LASPALM12 appears right next to LPG1, when in reality it is ~500 meters north).
#' 
#' @usage getCoords_DBHYDRO(stn = "LASPALM11")
#' 
#' @param stn character string. Case sensitive.
#' 
#' @return dataframe \code{getCoords_DBHYDRO} returns a dataframe with lat/long (epsg:4326) and UTM zone 19 coordinates.
#' 
#' 
#' @examples
#' \dontrun{
#' ### example usage as part of a workflow
#' 
#' SMA.keys <- c(
#' 37737, # LPG1
#' 37740, # LPG2
#' 39288, # LASPALM11
#' 39289, # LASPALM12
#' 39290, # LASPALM13
#' 39291, # LASPALM14
#' 39292, # LASPALM15 
#' "07103", # ANGEL
#' "05738", # G-3273 
#' "WN173",  # S357_H
#' "AM177"  # S357N_H
#' )
#' SMAlist    <- lapply(X = SMA.keys, getDBHYDROhydro, startDate = "20200101", 
#'      endDate = "20200901")
#' SMADat     <- do.call(rbind, SMAlist)
#' sma.coords <- do.call(rbind, lapply(X = unique(SMADat$stn), getCoords_DBHYDRO))
#' 
#' ### convert to spatial data and plot
#' sma.crds           <- sma.coords[!is.na(sma.coords$latitude), ]
#' sma.crds$longitude <- -1 * sma.crds$longitude
#' coordinates(sma.crds) <- c("longitude", "latitude")
#' proj4string(sma.crds) <- CRS("+init=epsg:4326")
#' 
#' plot(sma.crds)
#' }
#' 
#' @importFrom XML  htmlParse
#' @importFrom XML  getNodeSet
#' @importFrom XML  xpathSApply
#' @importFrom XML  saveXML
#'  
#' @export


getCoords_DBHYDRO <- function(stn = "LASPALM11") {
  targetURL <- paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_station_info?v_station=", stn)
  tempDoc      <- XML::htmlParse(readLines(targetURL, warn=FALSE),
                                 useInternalNodes = TRUE)
  TempNodes    <- XML::getNodeSet(tempDoc, "//tr")
  
  outDF        <- data.frame(stn = stn, 
                             latitude = NA,
                             longitude= NA,
                             easting  = NA,
                             northing = NA)
  ### hardcoded positions of lat and long coords
  for (i in 1:length(TempNodes)) {
    out2   <- XML::xpathSApply(tempDoc, "//tr", XML::saveXML)[i] # converts to char vector
    if (!grepl(x = out2, pattern = 
               "Latitude|Longitude|X Coord|Y Coord")) {
      next
    }
    # stop()
    # get lat/longs
    coordType <- ifelse(grepl(x = out2, pattern = "Latitude"), "latitude", 
                        ifelse(grepl(x = out2, pattern = "Longitude"), "longitude", 
                               ifelse(grepl(x = out2, pattern = "Vertical Conversion at Gage"), "NAVD_to_NGVD", 
                                      ifelse(grepl(x = out2, pattern = "X Coord"), "easting", 
                                             ifelse(grepl(x = out2, pattern = "Y Coord"), "northing", NA)))))
    
    # val       <- strsplit(out2, "</td>\\n    <td>")[[1]][2] # wow, this actually seems to work for all three.
    val       <- strsplit(out2, 'nowrap;\">')[[1]][2] # wow, this actually seems to work for all three.
    if (coordType %in% c("northing", "easting")) {
      charVal <- strsplit(val, "</td>")[[1]][1]
      decimalDegrees       <- as.numeric(charVal) # this fixed multiplier might be an issue
    } else if (coordType %in% c("longitude", "latitude")) {
      charVal <- strsplit(val, "</td>")[[1]][1]
      degs <- as.numeric(substr(charVal, 1, 2))
      mins <- as.numeric(substr(charVal, 3, 4))
      secs <- as.numeric(substr(charVal, 5, nchar(charVal)))
      decimalDegrees       <- degs + mins / 60 + secs / 3600
    } else if (coordType %in% c("NAVD_to_NGVD")) {
      decimalDegrees       <- NA
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
