#' @title DataForEver hydrology database parameter query
#'
#' @description Identifies parameters available from the DataForEver hydrology database
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
#' getParams_DFE(stn = "S333|S18C", dbname = "hydrology") # all possible parameters reported 
#' getParams_DFE(stn = "S333|S18C", dbname = "waterquality") # a station-specific report
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
  } else if (dbname %in% "hydrology") {
    hostAddr <- "10.146.112.14"
  } else {
    stop("dbname not accepted as a valid input")
  }
  
  ### prep for failure. Goal here is to close all connections opened during the function.
  init.connections <- DBI::dbListConnections( DBI::dbDriver( drv = "MySQL"))
  on.exit(lapply( DBI::dbListConnections( DBI::dbDriver( drv = "MySQL"))[!DBI::dbListConnections( DBI::dbDriver( drv = "MySQL")) %in% init.connections], odbc::dbDisconnect))
  
  
  if ((stn %in% "all") || is.null(stn)) {
    con <- odbc::dbConnect(RMySQL::MySQL(),
                           dbname   = dbname,
                           host     = hostAddr,
                           port     = 3306,
                           user     = 'read_only',
                           password = 'read_only')
    if (dbname %in% "hydrology") {
      dfe.type <- DBI::dbReadTable(con,'datatype') # all parameters.
    } else if (dbname %in% "waterquality") {
      dfe.type <- DBI::dbReadTable(con,'parameter')
    }
    odbc::dbDisconnect(con)
  } else {
    ### run query for specific stations and return available parameters and number of samples based on POR query
    if (dbname %in% "hydrology") {
      con <- odbc::dbConnect(RMySQL::MySQL(),
                             dbname   = dbname,
                             host     = hostAddr,
                             port     = 3306,
                             user     = 'read_only',
                             password = 'read_only')
      dfe.type <- DBI::dbReadTable(con,'datatype') # TODO: identify params available at a specific station?
      odbc::dbDisconnect(con)
    } else if (dbname %in% "waterquality") {
      stnNames <- unlist(strsplit(stn, "\\|"))
      a        <- getDFE(stn = stnNames, dbname = "waterquality")
      dfe.type <- stats::aggregate(value ~ stn + parameter, data = a, FUN = length)
      names(dfe.type)[3] <- "observations"
    }
  }
  # if (!is.null(stn)) {
  #   dfe.sta <- dfe.sta[grepl(x = dfe.sta$station, pattern = stn), ]
  # }
  # 
  # sql <- sprintf("SELECT station, datatype
  #               	order by station, datatype")
  # output <- dbGetQuery(con,sql)
  # odbc::dbDisconnect(con)
  
  ### change 'station' column to 'stn' to match DBHYDRO output
  names(dfe.type)[names(dfe.type) %in% "station"] <- "stn"
  
  return(dfe.type[order(dfe.type$datatype), c("datatype", "units", "description")])
}
