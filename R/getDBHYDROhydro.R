#' @title Retrieve R-friendly hydrology data from DBHYDRO using DB keys
#'
#' @description Downloads and lightly processes DBHYDRO data. This function uses DB keys to download datasets and is therefore more versatile than getDBHYDRO.
#' 
#' @usage getDBHYDROhydro(dbkey = "03638", startDate = "19600101",
#' endDate = gsub(x = Sys.Date(), pattern = "-", replacement = ""))
#' 
#' @param dbkey DBkey for station and parameter of interest. Use 'getDBkey()' to find DBkeys associated with a station.
#' @param startDate beginning of period of record, in form YYYYMMDD
#' @param endDate end of period of record, in form YYYYMMDD
#' 
#' @return dataframe \code{getDBHYDROhydro} returns a dataframe of data
#' 
#' 
#' @examples
#' \dontrun{
#' ### example workflow:
#' getDBkey(stn = "S12d", type = "FLOW") # find DBkey for station/parameter of interest
#' fdat <- getDBHYDROhydro(dbkey = "01310") # download data for DBkey
#' tail(fdat)
#' }
#' 
#' @importFrom httr GET
#' @importFrom utils read.csv
#'  
#' @export


getDBHYDROhydro <- function(dbkey = "03638", startDate = "19600101",
                            endDate = gsub(x = Sys.Date(), pattern = "-", replacement = "")) { # format(x = strptime(x = as.character(Sys.Date()), format = "%Y-%m-%d"), "%Y-%m-%d")
  urlDL <- paste0("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=", startDate, "&v_end_date=", endDate, "&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_db_request_id=5603897&v_where_clause=&v_dbkey=", dbkey, "&v_os_code=Unix&v_interval_count=5&v_cutover_datum=1")
  
 
  
  fileLoc <- tempfile()
  httr::GET(urlDL, httr::write_disk(fileLoc, overwrite = TRUE), httr::timeout(99999))
  ### TODO: make robust to odd cases, e.g., a <- getDBHYDROhydro(dbkey = "VV474")
  ### "skip" is not always = 3
  ### 1. load file, identify first case where DBKEY column == dbkey argument
  ### 3. set skip argument and re-load file
  output_temp <- utils::read.csv(fileLoc, stringsAsFactors = FALSE, skip = 0, header = FALSE)  
  skip_arg <- min(which(output_temp[, 2] == dbkey)) ### this is potentially a fragile approach to specifying DBKEY column
  
  output <- utils::read.csv(fileLoc, stringsAsFactors = FALSE, skip = skip_arg)  
  
  names(output) <- tolower(names(output))
  names(output)[1] <- "stn"
  names(output)[4] <- "value"
  
  ### process date data
  output$date <- gsub(pattern = "([a-zA-Z]{2}-.*)", replacement = "\\L\\1", x = output$daily.date, perl=TRUE)
  output$date <- as.POSIXct(strptime(output$date, format = "%d-%b-%Y"))
  output$year     <- as.numeric(substr(output$date, 1, 4))
  output$mo       <- as.numeric(substr(output$date, 6, 7))
  output$day      <- as.numeric(substr(output$date, 9, 10))
  output$stn      <- gsub(pattern = "_.*", replacement = "\\1", x = output$stn, perl=TRUE) # remove anything after the underscore
  
  ### TODO: optionally, replace "value" with name of parameter obtained from DBKey query?
  output <- output[-nrow(output), c("stn", "date", "year", "mo", "day", "value")]
  invisible(output)
}
