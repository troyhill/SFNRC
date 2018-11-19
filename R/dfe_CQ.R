#' @title Plot concentration-discharge relationship for a DataForEver station
#'
#' @description Downloads and compiles DataForEver hydrology and water quality data. Creates plot and returns descriptive statistics and raw data in list form. This function Works only on linux machines on the SFNRC network with access to the opt/physical drive. Code issues system commands, runs shell scripts, and modifies files in a temp folder on the local hard drive.
#' 
#' @usage dfe.CQ(stn, parameter_list = c("flow", "tail_water", "head_water", "stage"),
#' target_analytes = "PHOSPHATE, TOTAL AS P", date_range = "all", rFriendlyParamNames = TRUE
#' )
#' 
#' @param stn the target station from DataForEver. Recommended to run function on a single station at a time.
#' @param parameter_list vector of desired parameters from the hydrology database
#' @param target_analytes grep-style character vector naming water quality analytes of interest. default is "orthophosphate". Case insensitive.
#' @param date_range date range to be included. Default is entire period of record.
#' @param rFriendlyParamNames TRUE/FALSE; indicates whether parameter names should be modified to be R-friendly (no special characters, commas, or spaces). Advisable for analysis, as this makes analysis easier and pre-empts changes coerced by, e.g., \code{plyr::ddply}
#' 
#' @return list \code{dfe.CQ} returns a plot of the CQ dataframe with water quality measurements from each station identified in \code{stns}.
#' 
#' @seealso \code{\link{dfe.wq}}, \code{\link{dfe.hydro}}
#' 
#' @examples
#' \dontrun{
#' dfe.CQ(stn = "S333")
#' }
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom graphics abline
#' @importFrom stats reshape
#' @importFrom stats lm
#' 
#' @export




dfe.CQ <- function(stn, 
                    parameter_list = c("flow", "tail_water", "head_water", "stage"),
                    target_analytes = "PHOSPHATE, TOTAL AS P", date_range = "all", 
                   rFriendlyParamNames = TRUE) {
  
  ### get hydro data
  hyd <- dfe.hydro(stns = stn, data_shape = "wide")
  # hyd <- hydDat[hydDat$stn %in% "S700", ] ### for testing

  ### get water quality data
  wq <- dfe.wq(stns = stn, target_analytes = toupper(target_analytes), rFriendlyParamNames = rFriendlyParamNames)
  # wq <- wqDat[(wqDat$param %in% "PHOSPHATE, TOTAL AS P") & (wqDat$stn %in% "S700"), ] ### for testing
  ### convert wq to wide format
  
  tempDat <- stats::reshape(wq, idvar = c("stn", "date", "year", "mo", "day", "time", "datetime"), timevar = "param", direction = "wide")
  names(tempDat) <- gsub(x = names(tempDat), pattern = "value.| |,", replacement = "")
  
  tempDatForMerge <- tempDat[, c("stn", "date", grep(x = names(tempDat), pattern = "units|mdl|matrix", value = TRUE), names(tempDat)[length(names(tempDat))])]
  mergDat <- plyr::join_all(list(hyd, tempDatForMerge), by = c("stn", "date"))
  
  ### plot
  plot(log(get(names(tempDat)[length(names(tempDat))])) ~ log(flow), data = mergDat[(mergDat[, names(tempDat)[length(names(tempDat))]] > 0)  & (mergDat$flow > 0), ],
       ylab = paste0(names(tempDat)[length(names(tempDat))], " (log)"), xlab = "Flow (log)")
  
  summary(lmTmp <- stats::lm(log(get(names(tempDat)[length(names(tempDat))])) ~ log(flow), data = mergDat[(mergDat[, names(tempDat)[length(names(tempDat))]] > 0) & (mergDat$flow > 0), ]))
  graphics::abline(lmTmp, col = "red")
  
  ### return data
  mergDat
  
}


