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
#' 
#' ### check that instantaneous data is also loaded properly
#' s333.inst <- getDBHYDROhydro(dbkey = "65086")
#' tail(s333.inst)
#' }
#' 
#' @importFrom httr GET
#' @importFrom utils read.csv
#'  
#' @export


getDBHYDROhydro <- function(dbkey = "03638", startDate = "19600101",
                            endDate = gsub(x = Sys.Date(), pattern = "-", replacement = "")) { # format(x = strptime(x = as.character(Sys.Date()), format = "%Y-%m-%d"), "%Y-%m-%d")
  urlDL <- paste0("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=", startDate, "&v_end_date=", endDate, "&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_db_request_id=5753707&v_where_clause=&v_dbkey=", dbkey, "&v_os_code=Unix&v_interval_count=5&v_cutover_datum=1")
                # "http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date="             "&v_end_date="           "&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_db_request_id=5753707&v_where_clause=&v_dbkey="         "&v_os_code=Unix&v_interval_count=5&v_cutover_datum=1"
 
  
  fileLoc <- tempfile()
  httr::GET(urlDL, httr::write_disk(fileLoc, overwrite = TRUE), httr::timeout(99999))
  ### TODO: identify instantaneous data (e.g., dbkey = "65086") and process differently
  ### 
  ### 1. load file, check if line 3: stat = INST or MEAN
  # output_temp <- utils::read.csv(fileLoc, stringsAsFactors = FALSE, skip = 1, header = TRUE, row.names=NULL) 
  # # row names are shifted right 2
  # if (output_temp$UNITS[1] == "DA") {
  #   dataType <- "daily"
  # } else if (output_temp$UNITS[1] == "INST") {
  #   dataType <- "inst"
  # }
  
  output_temp <- utils::read.csv(fileLoc, stringsAsFactors = FALSE, skip = 0, header = FALSE)  
  skip_arg <- min(which(output_temp[, 2] == dbkey)) - 1  ### this is potentially a fragile approach to specifying DBKEY column
  
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
  
  ### remove marginalia at bottom of file, as seen in 'a <- getDBHYDROhydro(dbkey = "VV474")'
  pre_trim <- nrow(output)
  output   <- output[!is.na(output$date), ]
  
  if (!nrow(output) == pre_trim) {
    message(paste(pre_trim - nrow(output), "rows with date = NA trimmed from", dbkey, "dataset \n"))
  }
    
  ### TODO: optionally, replace "value" with name of parameter obtained from DBKey query?
  output <- output[, c("stn", "date", "year", "mo", "day", "value")]
  invisible(output)
}