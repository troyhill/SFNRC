#' @title An API for project-level DBHYDRO water quality data
#'
#' @description Downloads DBHYDRO water quality data for a specified project
#' 
#' @usage dbhydro.proj(destfile = "proj_report_todaysDate.csv",
#' project_codes = "ACMEB", 
#' start_date = "01-JAN-1960", # note format
#' destination = "file_csv", 
#' rename_proj = TRUE,
#' import_data = FALSE)
#' 
#' @param destfile a character vector file address for output, saved as a comma-delimited text file.
#' @param project_codes a character vector of station names. If one station in the list returns no data, the entire url request fails.
#' @param start_date lorem ipsum
#' @param destination lorem ipsum
#' @param rename_proj indicates whether destination file be re-named to replace "proj" with project name
#' @param import_data lorem ipsum
#' 
#' @return dataframe \code{dbhydro.proj} saves a csv of DBHYDRO water quality data to disk and has an option (\code{import = TRUE}) to load a dataframe of data into working environment.
#' 
#' @seealso \code{\link{dbhydro.proj.batch}} is preferred for multiple-project downloads.
#' 
#' @examples
#' \dontrun{
#' # save data as a csv:
#' dbhydro.proj(project_codes = "CAMB")
#' 
#' # or load into working environment
#' dbhydro.proj(project_codes = "CAMB", import = TRUE)
#' }
#' 
#' @importFrom httr write_disk
#' @importFrom httr GET
#' @importFrom httr timeout
#' @importFrom utils read.csv
#' 
#' @export

dbhydro.proj <- function(destfile = "proj_report_todaysDate.csv",
                          project_codes = "ACMEB",
                          start_date = "01-JAN-1960", # note format
                          destination = "file_csv",
                          rename_proj = TRUE, 
                          import_data = FALSE
) {
  
  ### input checks
  if (!is.character(destfile) || !(length(destfile) == 1)) {
    stop("invalid input for 'destfile': must be a single character string")
  }
  if (!is.character(project_codes) || !(length(project_codes) == 1)) {
    stop("invalid input for 'project_codes': must be a character vector of length 1")
  }
  if (!is.character(start_date) || !(length(start_date) == 1)) {
    stop("invalid input for 'start_date': must be a character vector of length 1 (e.g., '01-JAN-1960')")
  }
  if (!is.character(destination) || !(length(destination) == 1)) {
    stop("invalid input for 'destination': must be a character vector of length 1")
  }
  if (!is.logical(rename_proj) || !(length(rename_proj) == 1)) {
    stop("invalid input for 'rename_proj': must be TRUE or FALSE")
  }
  if (!is.logical(import_data) || !(length(import_data) == 1)) {
    stop("invalid input for 'import_data': must be TRUE or FALSE")
  }
  
  
  
  if (grepl("todaysDate", destfile)) {
    destfile <- gsub(pattern = "todaysDate", replacement = format(Sys.Date(), "%Y%m%d"), x = destfile)
  }
  if (rename_proj == TRUE) {
    destfile <- gsub(pattern = "proj", replacement = gsub(pattern = "'", replacement = "", x = project_codes), x = destfile)
  }
  
  if (grepl("%25", project_codes)) { 
    projectRef <- "project_code='"
  } else { 
    projectRef <- "project_code='"
  }
  
  
  # nocov start
  url.init <- paste0("http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full?v_where_clause=where%20", 
                     #"date_collected%20%3e%20='"start_date, 
                     projectRef, project_codes, "'",
                     "&v_target_code=", destination
  )
  #download.file(url = url.init, destfile)  # timeout problems even after setting options(timeout=24000000000000000000000000000000000000000)
  #RCurl::getURL(url = url.init) # crashed RStudio
  httr::GET(url.init, httr::write_disk(paste0(tempDir(), destfile), overwrite = TRUE), httr::timeout(99999))
  if (import_data == TRUE) {
    output <- utils::read.csv(paste0(tempDir(), destfile))
  }
  
  # nocov end
}
