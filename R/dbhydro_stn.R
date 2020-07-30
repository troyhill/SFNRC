#' @title An API for station-level DBHYDRO water quality data
#'
#' @description Downloads DBHYDRO water quality data for specified stations
#' 
#' @usage dbhydro.stn(destfile = "stn_report_todaysDate.csv",
#' stations = c("ACRA1"),
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
#' @param destfile a character vector file address for output, saved as a comma-delimited text file.
#' @param stations a character vector of station names. If one station in the list returns no data, the entire url request fails.
#' @param rename_proj lorem ipsum
#' @param parameters lorem ipsum
#' @param report_type lorem ipsum
#' @param incl_qc_flags lorem ipsum
#' @param incl_flagged_data lorem ipsum
#' @param destination lorem ipsum
#' @param start_date lorem ipsum
#' @param end_date lorem ipsum
#' @param import_data Logical; should data be returned as R object (and not saved to file)?
#' 
#' @return dataframe \code{dbhydro.stn} saves a csv of DBHYDRO water quality data to disk or returns a dataframe.
#' 
#' @seealso \code{\link{dbhydro.stn.batch}} is preferred for multiple-station downloads.
#' 
#' @examples
#' \dontrun{
#' head(dbhydro.stn())
#' }
#' 
#' @importFrom httr write_disk
#' @importFrom httr GET
#' @importFrom httr timeout
#' @importFrom utils read.csv
#' 
#' @export


dbhydro.stn <- function(destfile = "stn_report_todaysDate.csv",
                         stations = c("ACRA1"), # If one station in the list returns no data, the url request fails.
                         rename_proj = TRUE,
                         parameters = "all",
                         report_type = "full", # full = long dataset - 1 line per sample; crosstab = wide dataset
                         incl_qc_flags = TRUE,
                         incl_flagged_data = FALSE,
                         destination = "file_csv", 
                         start_date = "01-JAN-1960", # note format
                         end_date   = "today",
                         import_data = FALSE) {
  
  ### input checks
  if (!is.logical(rename_proj) || !(length(rename_proj) == 1)) {
    stop("invalid input for 'rename_proj': must be TRUE or FALSE")
  }
  if (!is.logical(incl_qc_flags) || !(length(incl_qc_flags) == 1)) {
    stop("invalid input for 'incl_qc_flags': must be TRUE or FALSE")
  }
  if (!is.logical(incl_flagged_data) || !(length(incl_flagged_data) == 1)) {
    stop("invalid input for 'incl_flagged_data': must be TRUE or FALSE")
  }
  if (!is.logical(import_data) || !(length(import_data) == 1)) {
    stop("invalid input for 'import_data': must be TRUE or FALSE")
  }
  if (!is.character(report_type) || !(length(report_type) == 1)) {
    stop("invalid input for 'report_type': must be a single character vector")
  }
  if (!report_type %in% c("full", "crosstab")) {
    stop("invalid input for 'report_type': must either be 'full' or 'crosstab' ")
  }
  
  
  # nocov start
  if (rename_proj == TRUE) {
    destfile <- gsub(pattern = "stn", replacement = gsub(pattern = "'", replacement = "", x = stations), x = destfile)
  }
  
  
  if (report_type == "full") {
    report_type_text <- paste0("&v_report=ctr_1_true")
  } else if (report_type == "crosstab") {
    report_type_text <- paste0("&v_report=ctr_w")
  }
  if (incl_qc_flags == TRUE) {
    qc_flags_text <- paste0("&v_exc_qc=N")
  } else if (incl_qc_flags == FALSE) {
    qc_flags_text <- paste0("&v_exc_qc=Y")
  } else stop("invalid input for 'incl_qc_flags': must be TRUE/FALSE")
  
  if (incl_flagged_data == FALSE) {
    flagged_data_text <- paste0("&v_exc_flagged=Y")
  } else if (incl_flagged_data == TRUE) {
    flagged_data_text <- paste0("&v_exc_flagged=N")
  } else stop("invalid input for 'incl_flagged_data': must be TRUE/FALSE")
  
  if (!destination == "file_csv") {
    stop("Currently only csv output is supported. Use argument 'destination = 'file_csv'")
  }
  
  if (end_date == "today") {
    end_date <- toupper(format(Sys.Date(), "%d-%b-%Y"))
  }
  
  if (grepl("todaysDate", destfile)) {
    destfile <- gsub(pattern = "todaysDate", replacement = format(Sys.Date(), "%Y%m%d"), x = destfile)
  }
  
  
  if (grepl("%25", stations)) { # if wild cards are used in station references, url must use "like"
    stationRef <- "'%20and%20station_id%20like%20('"
  } else { # if no wild cards are used, url must use "in"
    stationRef <- "'%20and%20station_id%20in%20('"
  }
  
  url.init <- paste0("http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full?v_where_clause=where%20date_collected%20%3e%20'", 
                     start_date, 
                     stationRef, stations, "')",
                     "&v_target_code=", destination
  )
  
  output <- NULL
  
  httr::GET(url.init, httr::write_disk(paste0(tempdir(), destfile), overwrite = TRUE), httr::timeout(99999))
  if (import_data == TRUE) {
    output <- utils::read.csv(paste0(tempdir(), destfile), stringsAsFactors = FALSE)
  }
  
  unlink(paste0(tempdir(), destfile))
  
  invisible(output)
  # nocov end
}




### dbh_stn_data() usage
# input_stns <- c("G300")
# test <- dbh_stn_data(stations = input_stns)
# summary(test[, 1:4])
# 
## CAMB stand-in: 
# input_stns <- c("'G300'") # 'G300','S5A','S5AE','S5AS','S5AU','S5AUS','S5AW'
# test <- dbh_stn_data(stations = input_stns)
# 
# input_stns <- c('G300','S5A','S5AE','S5AS','S5AU','S5AUS','S5AW')
# for (i in 1:length(input_stns)) {
#   test <- dbh_stn_data(stations = input_stns[i])
# }
# summary(test[, 1:4])
# 
# dbh_stn_data_batch(codes = input_stns)
# 
# input_stns2 <- c("'ACRA%25'") # wild cards set by "%25"
# test2 <- dbh_stn_data(stations = input_stns2)
# summary(test2[, 1:4])
# 
# input_stns3 <- c("'ACRA%25','G300'") # database problem: cannot mix wild-cards and exact station names, or do a multiple-query wildcard search (all return empty .xls)
# test3 <- dbh_stn_data(stations = input_stns3)
# summary(test3[, 1:4])
###
