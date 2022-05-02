#' @title Make EDEN/DBHYDRO station coordinates spatial data
#'
#' @description Converts scraped EDEN/DBHYDRO station coordinates to spatial data.
#' 
#' @param data input data. Typically, output from `getCoords_XXXX` functions.
#' @param format data source: 'eden' or 'dbhydro'
#' @param new_crs coordinate system to convert data to. Optional (set to `NULL` if a conversion is not desired).
#' 
#' @return spatialPointsDataFrame converts the output from getCoords_EDEN and getCoords_DBHYDRO to a spatialPointsDataFrame. EDEN data are set in the same projection as the EDEN DEM ("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") and DBHYDRO data are in lat/long ("+init=epsg:4326")
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
#' stn.coords <- do.call(rbind, lapply(X = stns, getCoords_EDEN, spatial = FALSE))
#' 
#' ### convert to spatial data and plot
#' stn.coords <- makeSpatialCoords(stn.coords, format = "eden")
#' 
#' plot(stn.coords)
#' }
#' 
#' @importFrom terra  vect
#' @importFrom terra  project
#' @importFrom terra  crs
#'  
#' @export


makeSpatialCoords <- function(data, 
                           format = "dbhydro",  # 'dbhydro' or 'eden'
                           new_crs = "+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") {
  ### turns dataframe output from getCoords_XXXX into a spatialPointsDataFrame
  data$longitude <- -1 * data$longitude
  
  if (tolower(format) %in% "dbhydro") {
    data <- data[!is.na(data$longitude), ]
    # sp::coordinates(data) <- c("longitude", "latitude")
    # sp::proj4string(data) <- CRS("+init=epsg:4326")
    data <- terra::vect(data, geom = c("longitude", "latitude"), crs = terra::crs("+init=epsg:4326", proj = TRUE))
  }
  if (tolower(format) %in% "eden") {
    data <- data[!is.na(data$northing), ]
    # sp::coordinates(data) <- c("easting", "northing")
    # sp::proj4string(data) <- "+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" # crs(fireHydro::edenDEM)
    data <- terra::vect(data, geom = c("easting", "northing"), crs = terra::crs("+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", proj = TRUE))
    
  }
  if (!is.null(new_crs)) {
    # data <- sp::spTransform(data, new_crs)
    data <- terra::project(data, y = new_crs)
  }
  return(data)
}
