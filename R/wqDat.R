#' Data: DataForEver water quality data for northern boundary of Everglades National Park
#'
#'
#'
#' @format A dataframe with x observations of y variables:
#' \describe{
#' \item{stn}{station name}
#' \item{date}{date of observation}
#' \item{param}{parameter name}
#' \item{units}{measurement units}
#' \item{year}{year (extracted from date column)}
#' \item{value}{amount of parameter measured}
#' \item{mdl}{method detection limit, where available}
#' \item{src}{data source (DataForEver)}
#'}
#' @docType data
#' @keywords data, water quality
#' @name wqDat
#' @usage wqDat
#' @examples ### export to .csv:
#' write.csv(wqDat, file = file.path(tempdir(), "wqDat.csv"))
NULL
