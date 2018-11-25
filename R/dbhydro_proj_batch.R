#' @title A wrapper for \code{dbhydro.proj}
#'
#' @description Downloads DBHYDRO water quality data for specified projects. Avoids the problem of complete failure if a single project is invalid.
#' 
#' 
#' @param codes a character vector of DBHYDRO project names. 
#' @param ... additional arguments supplied to \code{\link{dbhydro.proj}}
#' 
#' @return dataframe \code{dbhydro.proj.batch} saves a csv of DBHYDRO water quality data to disk, with option to import data into global environment.
#' 
#' @seealso \code{\link{dbhydro.proj}}
#' 
#' @examples
#' \dontrun{
#'STA1W_project_codes <- c("ENRM", "ENRP", "ENRU") 
#'
#'dbh.proj.batch(codes = STA1W_project_codes)
#' }
#' 
#' @export

dbhydro.proj.batch <- function(codes, ...) {
  if (!is.character(codes)) {
    stop("'codes' must be a character vector")
  }
  
  for (i in 1:length(codes)) {
      tryCatch({
        print(codes[i])
        dbhydro.proj(project_codes = codes[i], ...)
      }, error = function(e) {print("Error in project ", codes[i], ": ", conditionMessage(e), "\n", quote = FALSE)})
    }
  }
  
  ### dbh.proj.batch() usage
  # STA1E_project_codes <- c("ENRR", 'ST1E', 'ST1W', 'CAMB', "EAAP", "ST1EM",
  #                          "ST1EG", "RAIN", "X", "TM", "PEST", "EVPA", "HGLE",
  #                          "ST1M", "ST1G", "LAB", "L8RT" # , "ST1F" appears to be fish data
  # )
  # STA1W_project_codes <- c("ENRM", "ENRP", "ENRU") # duplicates that overlap with STA1E: "ENRR", "LAB", "CAMB", "ST1E", "ST1W", "ST1G", "ST1M", "ST1F", "ST1EM")
  # STA2_project_codes <- c("STA2", "ST2M", "ST2G", "EAA", "USGS", "HGOS") # duplicates: c("CAMB", "EAAP", "EVPA", "PEST", "RAIN", "X") "ST2F" appears to be fish data
  # STA34_project_codes <- c("ST34", "ST34G", "ST34M", "A1FEB", "HOLY") # duplicates: c("CAMB", "EAA", "EVPA", "HGOS", "PEST", "RAIN", "USGS", "X") "ST34F" appears to be fish data
  # STA56_project_codes <- c("ST5R", "STA5", "STA6", "ST6M", "SEMI", "RTBG", "ST5M") # duplicates: c("PEST", "CAMB") "ST6F" appears to be fish data
  # 
  # dbh_proj_data_batch(codes = STA1E_project_codes)
  # dbh_proj_data_batch(codes = STA1W_project_codes)
  # dbh_proj_data_batch(codes = STA2_project_codes)
  # dbh_proj_data_batch(codes = STA34_project_codes)
  # dbh_proj_data_batch(codes = STA56_project_codes)
  ###
  ### 