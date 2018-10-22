#' Data: Coordinates for water quality and hydrology stations in DataForEver
#'
#'
#'
#' @format A dataframe with 5109 monitoring stations. Coordinate reference system is "+init=epsg:3512".
#' \describe{
#' \item{stn}{station name}
#' \item{long}{station longitude in decimal degrees}
#' \item{lat}{station latitude in decimal degrees}
#'}
#' @docType data
#' @keywords data, water quality
#' @name masterCoords
#' @usage masterCoords
#' @examples ### export to .csv:
#' write.csv(masterCoords, file = file.path(tempdir(), "masterCoords.csv"))
#' 
#' #### used to generate coordinate object
#' \dontrun{
#' library(dplyr)
#' masterCoords <- read.csv(file = "/home/thill/RDATA/dataForEver/WQdata/station_coordinates/stnCoords_all.csv", stringsAsFactors = FALSE)
#' masterCoords <- masterCoords %>% group_by(stn) %>% filter(duplicated(stn) | n()==1)
#' coordinates(masterCoords) <- c("long", "lat")
#' proj4string(masterCoords) <- CRS("+init=epsg:3512")
#' }
NULL
