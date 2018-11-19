#' @title DataForEver hydrology database parameter query
#'
#' @description Identifies parameters available from the DataForEver hydrology database
#' 
#' @usage getDataTypes(parameter = "all", stn = "all", fixed = FALSE)
#' 
#' @param parameter a character string specifying the parameter(s) used to constrain the query.
#' @param stn a character string specifying the station(s) used to constrain the query.
#' @param fixed If \code{TRUE}, station name must be an exact match with the \code{stn} argument
#'  
#' @return dataframe \code{getDataTypes} returns a vector of stations and parameters.
#' 
#' 
#' @examples
#' \dontrun{
#' ### search by parameter:
#' getDataTypes(parameter = "salinity")
#' 
#' ### search by station:
#' getDataTypes(stn = "S333")
#' getDataTypes(stn = "S333", fixed = TRUE) # note that the function does not use exact matches
#'      # unless requested to do so using \code{fixed = TRUE}
#' }
#' 
#' 
#' @importFrom utils read.delim
#' 
#' @export




getDataTypes <- function(parameter = "all", stn = "all", fixed = FALSE) {
  
  searchParam <- parameter
  searchStn   <- toupper(stn)
  
  list.loc     <- file.path(tempdir(), "stn_temp.lst")
  bash.script.loc  <- file.path(tempdir(), "bash_stnLike.sh")
  
  ########################
  ### bash script taken from: /opt/physical/util/station_datatype_list_all.sh
  ### NOTE: This still has an opt/physical dependency -  bindir=/opt/physical/appaserver/src_hydrology
  ########################
  
  fileConn<-file(bash.script.loc)
  writeLines(c("#!/bin/sh
                # ---------------------------------------------
                # station_datatype_list_all.sh
                # ---------------------------------------------
                #
                # Freely available software: see Appaserver.org
                # ---------------------------------------------

                if [ \"$#\" -ne 1 ]
                                 then
                               echo \"Usage: $0 application\" 1>&2
                               exit 1
                               fi
                               
                               application=$1
                               
                               station_datatype=`get_table_name $application station_datatype`
                               
                               echo \"	select station, datatype					\\
                	from $station_datatype						\\
                	order by station, datatype;\"					|
                                 sql '|'									|
                                 cat"), fileConn)
  close(fileConn)
  
  system(paste0('chmod 777 ', bash.script.loc))
  bash.cmd <- paste0('bash -c "
                     . set_project hydrology
                     ', bash.script.loc, ' hydrology ', ' > ', list.loc, '
                     
                     "')
  system(bash.cmd)
  
  Sys.sleep(1) # to avoid odd behavior from loading data before downloads finish
  
  ########################
  ### load downloaded files into R
  ########################
  
  ### stopped working on this here. 
  stnDat <- utils::read.delim(list.loc, stringsAsFactors = FALSE, header = FALSE, na.strings = "null", skip = 1, dec = " ",
                col.names = c("stn", "datetime_modified"), colClasses = c("character"))

  stnDatReturn  <- sapply(strsplit(x = stnDat$stn, split = " "), "[", 1)
  stnDatFinal <- data.frame(matrix(unlist(strsplit(stnDatReturn, "\\Q|\\E")), ncol=2, byrow=TRUE))
  
  names(stnDatFinal) <- c("stn", "parameter")
  
  ### Not sure how these get converted to factors
  stnDatFinal$stn       <- as.character(stnDatFinal$stn)
  stnDatFinal$parameter <- as.character(stnDatFinal$parameter)
  
  if (!searchStn %in% "ALL") {
    stnDatFinal <- stnDatFinal[grep(x = stnDatFinal$stn, pattern = searchStn, fixed = fixed), ]
  }
  if (!searchParam %in% "all") {
    stnDatFinal <- stnDatFinal[stnDatFinal$parameter %in% searchParam, ]
  }
  stnDatFinal
  
  ########################
  ### clean up the temp folder
  ########################
  # newFiles <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)[!list.files(tempdir(), recursive = TRUE) %in% files.in.tmp]
  # invisible(file.remove(newFiles))
  
}
