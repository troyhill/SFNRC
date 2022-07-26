#' Data: Biscayne Bay model boundaries
#'
#' @description Biscayne Bay box model boundaries, simplified using Ramer-Douglas-Peucker algorithm.
#'
#' @format A shapefile of model boundaries in Biscayne Bay, Florida. Coordinate reference system is "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' \describe{
#' \item{HECTARES}{area of basin in hectares}
#' \item{BOX_CODE}{basin code}
#' \item{Box_Basin}{name of basin}
#'}
#' @docType data
#' @keywords biscayne bay, map
#' @name bnpMod
#' @usage bnpMod
#' @examples 
#' 
#' \dontrun{
#' # bnpMod <- system.file("inst/extdata/bnpMod.shp", package="SFNRC")
#' plot(bnpMod)
#' # tools::resaveRdaFiles(list.files("data/", full.names= TRUE),compress="xz")
#' # tools::checkRdaFiles("data/")
#' }
#' 
NULL
