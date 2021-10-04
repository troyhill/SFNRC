#' @title Retrieve R-friendly water quality data from DBHYDRO
#'
#' @description Downloads and lightly processes DBHYDRO data. This function is identical to `getWQ()`
#' 
#' @usage getDBHYDRO(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"), 
#'     removeFlaggedData = TRUE, outputType = "wide")
#' 
#' @param stn character string. Case insensitive.
#' @param removeFlaggedData Logical. Indicates whether flagged data should be removed
#' @param parameters a grep-style character string identifying water quality parameters to report. Set to NA to report all available parameters. Partial matches are made (e.g., "TURB" and "TURBIDITY" will both yield turbidity data)
#' @param outputType output type can be "full", "long", or "wide". "full" is a long dataset with all samples (possibility of multiple samples on a single day). "long" and "wide" average duplicate samples and are either long datasets (one column with parameter names and one column with parameter values) or wide datasets (one column of values for each parameter). The "long" form reports more information (units, MDL, PQL, RDL) than the "wide" form, which only reports values for each parameter.
#' 
#' @return dataframe \code{getDBHYDRO} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' longDat <- getDBHYDRO(stn = "s333", outputType = "long")
#' wideDat <- getDBHYDRO(stn = "s333", outputType = "wide")
#' }
#' 
#' @importFrom stats reshape
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#'  
#' @export


getDBHYDRO <- function(stn = "S333", parameters = c("PHOSPHATE, TOTAL AS P|TURBIDITY"), 
                       removeFlaggedData = TRUE, outputType = "wide") {
  message("\n`getDBHYDRO` is deprecated; `getWQ` is preferred\n")
  outDat <- getWQ(stn = stn, parameters = parameters, 
                       removeFlaggedData = removeFlaggedData, outputType = outputType)
  return(outDat)
}
