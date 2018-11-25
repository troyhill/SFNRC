#' @title A wrapper for \code{dbhydro.stn}
#'
#' @description Downloads DBHYDRO water quality data for specified stations. Avoids the problem of complete failure if a single station is invalid.
#' 
#' @usage dbhydro.stn.batch(codes, 
#' destfile = "stn_report_todaysDate.csv",
#' rename_proj = TRUE,
#' parameters = "all",
#' report_type = "full", # full = long dataset - 1 line per sample; crosstab = wide dataset
#' incl_qc_flags = TRUE,
#' incl_flagged_data = FALSE,
#' destination = "file_csv", 
#' start_date = "01-JAN-1960", # note format
#' end_date   = "today",
#' import_data = FALSE)
#' 
#' @param codes a character vector of station names. If one station in the list returns no data, the entire url request fails.
#' @param ... additional arguments supplied to \link{\code{dbhydro.stn}}
#' 
#' @return dataframe \code{dbhydro.stn.batch} saves a csv of DBHYDRO water quality data to disk.
#' 
#' @seealso \code{dbhydro.stn}
#' 
#' @examples
#' \dontrun{
#' head(dbh.stn.batch(codes = c("S333", "S151"))
#' }
#' 
#' @export

dbhydro.stn.batch <- function(codes, ...) {
  
  if (!is.character(codes)) {
    stop("'codes' must be a character vector")
  }
  
  for (i in 1:length(codes)) {
    tryCatch({
      print(codes[i])
      dbhydro.stn(stations = codes[i], ...)
    }, error = function(e) {print("Error in project ", codes[i], ": ", conditionMessage(e), "\n", quote = FALSE)})
  }
}
