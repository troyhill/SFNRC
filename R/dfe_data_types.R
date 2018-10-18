#' @title DataForEver hydrology database parameter query
#'
#' @description Identifies parameters available from the DataForEver hydrology database
#' 
#' @usage dfe.data.types(query = "flow")
#' 
#' @param query a character string
#' 
#' @return dataframe \code{dfe.data.types} returns a vector of stations
#' 
#' 
#' @examples
#' a <- dfe.data.types(query = "flow")
#' 
#' @importFrom utils read.delim
#' 
#' @export




dfe.data.types <- function(query = "flow") {
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
                
                echo \"Starting: $0 $*\" 1>&2
                
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

  stnDatReturn <- sapply(strsplit(x = stnDat$stn, split = " "), "[", 1)
  invisible(stnDatReturn) 
  
  ########################
  ### clean up the temp folder
  ########################
  # newFiles <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)[!list.files(tempdir(), recursive = TRUE) %in% files.in.tmp]
  # invisible(file.remove(newFiles))
  
}
