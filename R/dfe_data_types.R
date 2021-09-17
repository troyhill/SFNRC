#' @title DataForEver hydrology database parameter query
#'
#' @description Identifies parameters available from the DataForEver hydrology database. Note that the structure of the returned dataframe can vary depending on the database queried.
#' 
#' @usage getParams_DFE(dbname = "hydrology", stn = "all")
#' 
#' @param dbname name of the database sought for inquiry. Currently only 'hydrology' and 'waterquality' are supported. A case-insensitive character string.
#' @param stn pattern to be matched in station names ('NULL' or 'all' return all stations). This argument applies only to water quality database. A case-insensitive grep-friendly single character element (e.g., 'S333|S197' to search for multiple stations).
#'  
#' @return dataframe \code{getParams_DFE} returns a dataframe of stations and parameters.
#' 
#' 
#' @examples
#' \dontrun{
#' ### station argument applies only to water quality database
#' t1 <- getParams_DFE(stn = "all", dbname = "hydrology")       # all hydrology parameters
#' t2 <- getParams_DFE(stn = "all", dbname = "waterquality")    # all water quality parameters
#' t3 <- getParams_DFE(stn = "S333|S18C", dbname = "hydrology") # station-specific hydro report 
#' t4 <- getParams_DFE(stn = "S333|S18C", dbname = "waterquality") # station-specific WQreport
#' }
#' 
#' 
#' @importFrom odbc dbConnect
#' @importFrom RMySQL MySQL
#' @importFrom DBI dbReadTable
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbListConnections
#' @importFrom DBI dbDriver
#' 
#' @export




getParams_DFE <- function(dbname = "hydrology", stn = "all"# hydrology or waterquality
) {
  dbname <- tolower(dbname)
  if (dbname %in% "waterquality") {
    hostAddr <- "10.146.112.23"
  }
  else if (dbname %in% "hydrology") {
    hostAddr <- "10.146.112.14"
  }
  else {
    stop("dbname not accepted as a valid input")
  }
  init.connections <- DBI::dbListConnections(DBI::dbDriver(drv = "MySQL"))
  on.exit(lapply(DBI::dbListConnections(DBI::dbDriver(drv = "MySQL"))[!DBI::dbListConnections(DBI::dbDriver(drv = "MySQL")) %in% 
                                                                        init.connections], odbc::dbDisconnect))
  if ((stn %in% "all") || is.null(stn)) {
    con <- odbc::dbConnect(RMySQL::MySQL(), dbname = dbname, 
                           host = hostAddr, port = 3306, user = "read_only", 
                           password = "read_only")
    if (dbname %in% "hydrology") {
      suppressWarnings(dfe.type <- DBI::dbReadTable(con, "datatype"))
      dfe.type <- dfe.type[order(dfe.type$datatype), c("datatype", 
                                                       "units", "description")]
    }
    else if (dbname %in% "waterquality") {
      suppressWarnings(dfe.type <- DBI::dbReadTable(con, "parameter"))
    }
    odbc::dbDisconnect(con)
  }
  else {
    if (dbname %in% "hydrology") {
      con <- odbc::dbConnect(RMySQL::MySQL(), dbname = dbname, 
                             host = hostAddr, port = 3306, user = "read_only", 
                             password = "read_only")
      suppressWarnings(dfe.type <- DBI::dbReadTable(con, "datatype"))
      odbc::dbDisconnect(con)
      dfe.type <- dfe.type[order(dfe.type$datatype), c("datatype", 
                                           "units", "description")]
    }
    else if (dbname %in% "waterquality") {
      stnNames <- unlist(strsplit(stn, "\\|"))
      suppressWarnings(a <- getDFE(stn = stnNames, dbname = "waterquality"))
      dfe.type <- stats::aggregate(value ~ stn + parameter, 
                                   data = a, FUN = length)
      names(dfe.type)[3] <- "observations"
      ### sort 
      dfe.type <- dfe.type[order(dfe.type$stn), c("stn", 
                                           "parameter", "observations")]
    }
    
  }
  # names(dfe.type)[names(dfe.type) %in% "station"] <- "stn"
  return(dfe.type)
}
