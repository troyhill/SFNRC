#' Convert DBHYDRO flow and water quality datasets to match DataForEver for use in convertToEgret()
#'
#' @param data input dataset from DBHYDRO. Dataset will be converted to be consistent with data from DataForEver.
#' @param type type of input data: "hydrology" or "waterquality"
#' 
#' @return a list as created by \code{\link[EGRET]{mergeReport}}
#' @export
#'
#' @examples
#' targStn <- "S333"


DBHYDROToDFE <- function(data = hydDat, type = "hydrology") {
  ### converts DBHYDRO data to format of DataForEver, to facilitate use in convertToEgret
  if (type %in% "hydrology") {
    ### flow data: easy name change
    names(data)[grep(x = names(data), pattern = "value")] <- "flow"
  }
  if (type %in% "waterquality") {
    ### WQ data: date, value, parameter, stn
    if (unique(data$param) > 1) stop ("water quality dataset should not contain multiple parameters.")
    names(data)[grep(x = names(data), pattern = "^param$")] <- "parameter"
  }
  invisible(data)
}

