#' @title DataForEver station query
#'
#' @description Identifies DataForEver stations in hydrology or water quality databases
#' 
#' @usage getStn_DFE(pattern  = NULL, dbname = "hydrology")
#' 
#' @param dbname name of the database sought for inquiry. Currently only 'hydrology' and 'waterquality' are supported. A case-insensitive character string.
#' @param pattern pattern to be matched in station names ('NULL' returns all stations). A case-insensitive grep-friendly single character element (e.g., 'S333|S197' to search for multiple stations).
#' 
#' @return dataframe \code{getStn_DFE} returns a dataframe with stations and all associated information (coordinates, notes)
#' 
#' 
#' @examples
#' \dontrun{
#'   getStn_DFE(pattern = "S333")
#'   getStn_DFE(pattern = "S333", dbname = "waterquality")
#' }
#' 
#' @importFrom odbc dbConnect
#' @importFrom RMySQL MySQL
#' @importFrom DBI dbReadTable
#' @importFrom odbc dbDisconnect
#' @importFrom DBI dbListConnections
#' @importFrom DBI dbDriver
#' 
#' @export

  getStn_DFE <- function(pattern  = NULL, # optional regex string to use as query
                         dbname = "hydrology" # hydrology or waterquality
                         
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
    
    con <- odbc::dbConnect(RMySQL::MySQL(),
                           dbname   = dbname,
                           host     = hostAddr,
                           port     = 3306,
                           user     = 'read_only',
                           password = 'read_only')
    suppressWarnings(dfe.sta <- DBI::dbReadTable(con,'station'))
    
    if (!is.null(pattern)) {
      dfe.sta <- dfe.sta[grepl(x = tolower(dfe.sta$station), pattern = tolower(pattern)), ]
    }
    odbc::dbDisconnect(con)
    
    ### change 'station' column to 'stn' to match DBHYDRO output
    names(dfe.sta)[names(dfe.sta) %in% "station"] <- "stn"
    return(dfe.sta[, c("stn", "lat_nad83", "long_nad83", "utm_x", "utm_y")])
  }
  
