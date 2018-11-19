#' @title DataForEver station query
#'
#' @description Identifies DataForEver stations matching query
#' 
#' @usage getStn(query = "S33")
#' 
#' @param query a character string. Case insensitive.
#' 
#' @return dataframe \code{getStn} returns a vector of stations
#' 
#' 
#' @examples
#' a <- getStn(query = "S33")
#' a <- getStn(query = "s333")
#' 
#' @importFrom utils read.delim
#' 
#' @export




getStn <- function(query = "S33") {
  stn.list.loc     <- file.path(tempdir(), "stn_temp.lst")
  bash.script.loc  <- file.path(tempdir(), "bash_stnLike.sh")
  
  ########################
  ### bash script taken from: /opt/physical/util/stationLike.sh
  ### NOTE: This still has an opt/physical dependency -  bindir=/opt/physical/appaserver/src_hydrology
  ########################
  
  fileConn<-file(bash.script.loc)
  writeLines(c("#!/bin/ksh
               # ---------------------------------------------
               # station.sh
               # ---------------------------------------------
               #
               # Check to see if a station exists...
               # ---------------------------------------------
               
               if [ \"$#\" -ne 1 ]
                 then
               echo \"Usage: $0 station\" 1>&2
               exit 1
               fi
               
               station=$1
               
               if [[ $DATABASE == hydrology ]]; then
               
               echo \"DATABASE is set to:\" $DATABASE
               dbResponse=$(echo \"select station, comments, last_change 			\\
            		from station                                                \\
            		where station like '%${station}%';\"		                    \\
                                        | sql ' ' )					
                           
                           else
                             echo \"DATABASE is set to:\" $DATABASE
                           dbResponse=$(echo \"select station last_change 			            \\
            		from station                                                \\
            		where station like '%${station}%';\"		                    \\
                                        | sql ' ' )					
                           
               fi
               
               if [[ $dbResponse = \"\" ]]; then
               print  \"No Station named: $station\"
               else
                 
                 print  \"$dbResponse\"
               
               fi"), fileConn)
  close(fileConn)
  
  system(paste0('chmod 777 ', bash.script.loc))
  bash.cmd <- paste0('bash -c "
                     . set_project hydrology
                     ', bash.script.loc, ' ', toupper(query), ' > ', stn.list.loc, '

                     "')
  system(bash.cmd)
  
  Sys.sleep(1) # to avoid odd behavior from loading data before downloads finish
  
  ########################
  ### load downloaded files into R
  ########################
  stnDat <- utils::read.delim(stn.list.loc, stringsAsFactors = FALSE, header = FALSE, na.strings = "null", skip = 1, dec = " ",
                col.names = c("stn", "datetime_modified"), colClasses = c("character"))

  stnDatReturn <- sapply(strsplit(x = stnDat$stn, split = " "), "[", 1)
  stnDatReturn
  
  ########################
  ### clean up the temp folder
  ########################
  # newFiles <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)[!list.files(tempdir(), recursive = TRUE) %in% files.in.tmp]
  # invisible(file.remove(newFiles))
  
}
