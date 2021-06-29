#' @title DataForEver database API
#'
#' @description Downloads and compiles DataForEver water quality or hydrology data.
#' 
#' @usage getDFE(dbname = "hydrology", stn    = "S333", startDate = NULL,
#' endDate   = NULL, params    = NULL, matricesToExclude = "analyte_free_water",
#' rFriendlyParamNames = FALSE, data_shape = "long", addWaterQuality = FALSE,
#' addWaterQualityParams = NULL)
#' 
#' @param dbname name of the database sought for inquiry. Currently only 'hydrology' and 'waterquality' are supported. A case-insensitive character string.
#' @param stn pattern to be matched in station names ('NULL' or 'all' return all stations). This argument applies only to water quality database. A case-insensitive grep-friendly single character element (e.g., 'S333|S197' to search for multiple stations).
#' @param startDate start of desired date range (if NULL, the first date in period of record is used)
#' @param endDate end of desired date range (if NULL, the latest date in period of record is used)
#' @param params grep-style character vector naming analytes of interest. default is "all". 
#' @param matricesToExclude character vector specifying any sample matrices to be excluded. Spelling/case must be a perfect match to DataForEver entries. For example, "analyte_free_water" indicates field blanks. Advisible to check output using, e.g., \code{unique(wqDat$matrix)}
#' @param rFriendlyParamNames TRUE/FALSE; indicates whether parameter names should be modified to be R-friendly (no special characters, commas, or spaces). Advisable for analysis, as this makes analysis easier and pre-empts changes coerced by, e.g., \code{plyr::ddply}
#' @param data_shape shape of output dataframe. Default is long (one row per date-station-param) but can also be wide (one row per date-station) or really_wide (one row per date). If multiple observations are available for a date, they are averaged if this argument is set to 'wide' or 'really_wide'.
#' @param addWaterQuality if `dbname = 'hydrology'`, setting this to TRUE will allow wq data to be downloaded and merged internally
#' @param addWaterQualityParams if hydrologic and water quality parameters are collected simultaneously, this argument specifies the water quality parameters  
#' 
#' @return dataframe \code{getDFE} returns a dataframe with observations from each station identified in \code{stn} argument.
#' 
#' @seealso \code{\link{getDFE}}
#' 
#' @examples
#' 
#' \dontrun{
#' ### to check hydro parameter names:
#' # getParams_DFE(dbname = "hydrology")$datatype
#' a1 <- getDFE(stn = c("S12A", "S333"), dbname = "hydrology", params = "head_water|flow",
#'              startDate = "2018-01-01")
#' 
#' a2 <- getDFE(stn = c("S12A", "S333"), dbname = "waterquality", startDate = "2018-01-01")
#' unique(a2$stn)
#' 
#' 
#' }
#' 
#' @importFrom odbc dbConnect
#' @importFrom RMySQL MySQL
#' @importFrom DBI dbReadTable
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbListConnections
#' @importFrom DBI dbDriver
#' @export

getDFE <- function(dbname = "hydrology",# hydrology or waterquality
                   stn    = "S333", # setting this to NULL (download all data) is not advisable. There is no internal checking whether station name is valid - verify names are correct with getStns_DFE
                   startDate = NULL,
                   endDate   = NULL,
                   params    = NULL, # recommended to leave this as NULL and post-process yourself (to minimized trial and error guessing of param names). If not NULL, must be a regex-style single-element character string (e.g., 'TURBIDITY|NITROGEN' or 'FLOW|STAGE'). For hydrology database queries, param names must be exact matches with what appears in the database.
                   matricesToExclude = "analyte_free_water",
                   rFriendlyParamNames = FALSE,
                   data_shape  = "long",
                   addWaterQuality = FALSE,
                   addWaterQualityParams = NULL
) {
  if (!is.logical(rFriendlyParamNames)) {
    stop("'rFriendlyParamNames' must be TRUE or FALSE")
  }
  if (any(grepl(x = stn, pattern = "\\|"))) {
    stn <- unlist(strsplit(stn, "\\|"))
  }
  init.connections <- DBI::dbListConnections(DBI::dbDriver(drv = "MySQL"))
  on.exit(lapply(DBI::dbListConnections(DBI::dbDriver(drv = "MySQL"))[!DBI::dbListConnections(DBI::dbDriver(drv = "MySQL")) %in% 
                                                                        init.connections], odbc::dbDisconnect))
  dbname <- tolower(dbname)
  if (dbname %in% "waterquality") {
    hostAddr <- "10.146.112.23"
  } else if (dbname %in% "hydrology") {
    hostAddr <- "10.146.112.14"
  } else {
    stop("dbname not accepted as a valid input")
  }
  if (is.null(startDate)) 
    startDate <- "1900-01-01"
  if (is.null(endDate)) 
    endDate <- as.character(Sys.Date())
  if (all(sapply(X = list(stn, params), FUN = is.null)) && 
      (startDate %in% "1900-01-01") && (endDate %in% 
                                        as.character(Sys.Date()))) {
    stop("to download a full copy of the database, please contact troy_hill@nps.gov")
  }
  con <- odbc::dbConnect(RMySQL::MySQL(), dbname = dbname, 
                         host = hostAddr, port = 3306, user = "read_only", 
                         password = "read_only")
  if (dbname %in% "hydrology") {
    if(is.null(params)) {
      params <- paste0(SFNRC::getParams_DFE(dbname = dbname, stn = stn)$datatype, collapse = "|")
    }
    
    if (!is.null(stn) && (!stn %in% "all")) {
      paramNames <- unlist(strsplit(params, "\\|"))
      for (i in 1:length(stn)) {
        for (j in 1:length(paramNames)) {
          sql <- sprintf("SELECT *\n                 FROM measurement WHERE station='%s'\n                 AND datatype='%s' AND measurement_date BETWEEN\n                 '%s' AND '%s' ", 
                         stn[i], paramNames[j], startDate, endDate)
          suppressWarnings(output.tmp <- DBI::dbGetQuery(con, sql))
          if (i == 1 && j == 1) {
            output <- output.tmp
          }
          else {
            output <- rbind(output, output.tmp)
          }
        }
      }
    }
    odbc::dbDisconnect(con)
    output$date <- as.POSIXct(output$measurement_date, format = "%Y-%m-%d")
    output$datetime <- as.POSIXct(paste0(output$measurement_date, 
                                         output$measurement_time), format = "%Y-%m-%d%H%M")
    names(output)[names(output) %in% "measurement_value"] <- "value"
    names(output)[names(output) %in% "datatype"] <- "parameter"
  }
  if (dbname %in% "waterquality") {
    if (!is.null(stn) && (!stn %in% "all")) {
      for (i in 1:length(stn)) {
        sql <- sprintf("SELECT *\n                   FROM results WHERE station='%s' AND\n                 collection_date BETWEEN '%s' AND '%s'", 
                       stn[i], startDate, endDate)
        suppressWarnings(output.tmp <- DBI::dbGetQuery(con, sql))
        if (i == 1) {
          output <- output.tmp
        }
        else {
          output <- rbind(output, output.tmp)
        }
      }
    }
    odbc::dbDisconnect(con)
    if ((!params %in% "all") && (!is.null(params))) {
      output <- output[grep(x = output$parameter, pattern = params), 
      ]
    }
    if (rFriendlyParamNames) {
      output$parameter <- gsub(x = output$parameter, pattern = " |,", 
                               replacement = "")
      output$parameter <- gsub(x = output$parameter, pattern = "-|[+]", 
                               replacement = ".")
    }
    output$units <- toupper(output$units)
    output$units <- gsub(x = output$units, pattern = " ", 
                         replacement = "")
    output$units[output$units %in% "PPT"] <- "PSU"
    output$units[output$units %in% c("METERS")] <- "METER"
    output$units[output$units %in% "MG/M^3"] <- "MG/M3"
    output$parameter[output$parameter %in% c("AMMONIA, TOTAL AS N", 
                                             "NITROGEN, AMMONIA AS NH4")] <- "AMMONIA-N"
    output$parameter[output$parameter %in% c("temp")] <- "TEMP"
    output <- output[!output$matrix %in% c(matricesToExclude), 
    ]
    output$date <- as.POSIXct(output$collection_date, format = "%Y-%m-%d")
    output$datetime <- as.POSIXct(paste0(output$collection_date, 
                                         output$collection_time), format = "%Y-%m-%d%H%M")
    names(output)[names(output) %in% "concentration"] <- "value"
  }
  if (addWaterQuality) {
    data_shape <- "wide"
  }
  names(output)[names(output) %in% "station"] <- "stn"
  colsToKeep <- which(names(output) %in% c("stn", "date", 
                                           "parameter", "value"))
  if (!grepl(x = data_shape, pattern = "long")) {
    output <- stats::reshape(output[, colsToKeep], idvar = c("stn", 
                                                             "date"), timevar = "parameter", direction = "wide")
    names(output) <- gsub(x = names(output), pattern = "value.", 
                          replacement = "")
  }
  if (grepl(x = data_shape, pattern = "really_wide")) {
    output <- stats::reshape(output, idvar = c("date"), 
                             timevar = c("stn"), direction = "wide")
  }
  output$year <- as.numeric(format(output$date, format = "%Y"))
  output$mo <- as.numeric(format(output$date, format = "%m"))
  output$day <- as.numeric(format(output$date, format = "%d"))
  if (addWaterQuality) {
    wq <- getDFE(dbname = "waterquality", stn = stn, 
                 startDate = startDate, endDate = endDate, params = addWaterQualityParams, 
                 matricesToExclude = "analyte_free_water")
    dd.wq <- plyr::ddply(wq[, names(wq) %in% c("stn", 
                                               "date", "parameter", "value", "minimum_detection_limit")], 
                         c("stn", "date", "parameter"), 
                         summarise, value = mean(get("value"), na.rm = TRUE), 
                         minimum_detection_limit = max(get("minimum_detection_limit"), 
                                                       na.rm = TRUE))
    wq.temp <- stats::reshape(dd.wq, idvar = c("stn", 
                                               "date"), timevar = "parameter", direction = "wide")
    names(wq.temp) <- gsub(x = names(wq.temp), pattern = "value.| |,", 
                           replacement = "")
    wqDatForMerge <- wq.temp[, c(grep(x = names(wq.temp), 
                                      pattern = paste0("stn|date|minimum_detection_limit|", 
                                                       addWaterQualityParams), value = TRUE))]
    output <- plyr::join_all(list(output, wqDatForMerge), 
                             by = c("stn", "date"))
  }
  return(output)
}


