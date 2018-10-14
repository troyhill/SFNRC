#' @title DataForEver hydrology database API
#'
#' @description Downloads and compiles DataForEver hydrology data
#'
#' @details This function Works only on linux machines on the SFNRC network with access to the opt/physical drive. Code issues system commands, runs shell scripts, and modifies files in a temp folder on the local hard drive.
#' 
#' @usage dfe.hydro(stns, parameter_list = c("flow", "tail_water", "head_water", "stage"),
#' data_shape = "long")
#' 
#' @param stns a single-column dataframe of DataForEver station names. 
#' @param parameter_list vector of desired parameters. 
#' @param data_shape shape of output dataframe. Default is \code{long} (one row per date-station-param) but can also be \code{wide} (one row per date-station) or \code{really_wide} (one row per date)
#' 
#' @return dataframe \code{dfe.hydro} returns a dataframe with water quality measurements from each station identified in \code{stns}.
#' 
#' @seealso \code{\link{dfe.wq}}
#' 
#' @examples
#' \dontrun{
#' stations <- c("S333", "S12A", "S12B", "S12C", "S12D")
#' 
#'   # usage examples: 
#'   hydro.test <- dfe.hydro(stns = stations)
#'   hydro.test2 <- dfe.hydro(stns = stations, data_shape = "wide")
#'   hydro.test3 <- dfe.hydro(stns = stations, data_shape = "really_wide")
#' }
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom stats reshape
#' 
#' @export




dfe.hydro <- function(stns, 
                      parameter_list = c("flow", "tail_water", "head_water", "stage"),
                      data_shape = "long") {
  # I think this still requires access to NPS servers and opt/physical drive
  
  stn.list.loc     <- file.path(tempdir(), "stn_temp.lst")
  folder_with_data <- file.path(tempdir(), "data")
  bash.script.loc  <- file.path(tempdir(), "bash_hydro_tmp.sh") # wrapper for bash_hydro_sql.sh
  sql.script.loc   <- file.path(tempdir(), "hydro_data.sh") # interfaces with hydro database
  utils::write.table(stns, file = stn.list.loc, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE)
  
  ########################
  ### build bash script for interfacing with hydro database
  ### NOTE: This still has an opt/physical dependency -  bindir=/opt/physical/appaserver/src_hydrology
  ########################
  
  fileConn<-file(sql.script.loc)
  writeLines(c("#!/bin/ksh
               
               if [[ $# -ne 8 ]]; then
               echo \"\"
               echo \"Usage: $0 station datatype startdate enddate aggregate_level aggregate_statistic validation_level units_converted\"
               echo \"\"
               exit 1
               fi
               
               bindir=/opt/physical/appaserver/src_hydrology
               
               station=$1
               datatype=$2
               start_date=$3
               end_date=$4
               agg_level=$5
               agg_stat=$6
               validation_level=$7
               units_converted=$8
               
               $bindir/output_measurement_data hydrology               \\
               $station		    \\
               $datatype 		    \\
               $start_date		    \\
               $end_date	 	    \\
               email_address 		\\
               $agg_level 		    \\
               $validation_level 	\\
               $agg_stat	 	    \\
               n  			        \\
               n 			        \\
               $units_converted	\\
               n             		\\
               stdout             	\\
               2>/dev/null						    \\
               |grep -v '#'						\\
               |grep -v '^$'						
               
               "), fileConn)
  close(fileConn)
  
  
  ### still dependent on hydro_data.sh
  fileConn2<-file(bash.script.loc)
  writeLines(paste0("#!/bin/bash
                    
                    hydroParam=${3:-flow}
                    
                    while read station; do  
                    echo \"reading $3 data from $station\"
                    echo \"redirecting stdout to: ${station}_${hydroParam}.dat\"
                    ", sql.script.loc, " $station $hydroParam 1960-01-01 $(date +%Y-%m-%d) daily aggregate_statistic validation_level units_converted 		\\
                    | gawk -F \"|\" '{printf\"%s\\t%s\\t%s\\t%s\\n\", $1, $2, $3, $4}' 				\\
                    > $1/${station}_${hydroParam}.dat;   
                    done < $2
                    
                    "
  ), fileConn2)
  close(fileConn2)
  
  system(paste0('chmod 777 ', bash.script.loc))
  system(paste0('chmod 777 ', sql.script.loc))
  
  system(paste0('bash -c "
                mkdir ', folder_with_data, '
                . set_project hydrology
                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' flow
                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' stage
                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' head_water
                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' tail_water
                find ', folder_with_data,' -name \'*.dat\' -size -2k
                find ', folder_with_data,' -name \'*.dat\' -size -2k -delete
                
                "'))
  Sys.sleep(2) # to avoid odd behavior from loading data before downloads finish
  
  ########################
  ### load downloaded files into R
  ########################
  hydro_col_names <- c("stn", "param", "date", "value")
  dat.files.flow   <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_flow", value = TRUE)
  dat.files.stg    <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_stage", value = TRUE)
  dat.files.hw     <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_head_water", value = TRUE)
  dat.files.tw     <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_tail_water", value = TRUE)
  
  flow.list <- do.call(rbind, lapply(dat.files.flow, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE, na.strings = "null",
                                                                            col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  flow.list$value[is.na(flow.list$value)] <- 0
  hw.list <- do.call(rbind, lapply(dat.files.hw, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
                                                                        col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  hw.list$value <- as.numeric(trimws(hw.list$value))
  tw.list <- do.call(rbind, lapply(dat.files.tw, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
                                                                        col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  hw.list$value <- as.numeric(trimws(hw.list$value))
  stg.list <- do.call(rbind, lapply(dat.files.stg, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
                                                                          col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  
  ########################
  ### merge all the data
  ########################
  tempDat <- do.call(rbind, list(flow.list, hw.list, tw.list, stg.list))
  tempDat <- tempDat[tempDat$param %in% parameter_list, ]
  
  if (data_shape %in% "long") {
    
  } else if (grep(x = data_shape, pattern = "wide")) {
    tempDat <- stats::reshape(tempDat, idvar = c("stn", "date"), timevar = "param", direction = "wide")
    names(tempDat) <- gsub(x = names(tempDat), pattern = "value.", replacement = "")
    # names(tempDat) <- c(hydro_col_names[c(1, 3)], "cfs", "hw.ft", "tw.ft", "stg.ft") # changes depending on parameters selected. should define by mapping new names to value.stage etc.
  }
  
  if (data_shape %in% "really_wide") {
    tempDat <- stats::reshape(tempDat, idvar = c("date"), timevar = c("stn"), direction = "wide")
  }
  
  
  ########################
  ### clean up the data
  ########################
  tempDat$year     <- substr(tempDat$date, 1, 4) 
  tempDat$mo       <- substr(tempDat$date, 6, 7) 
  tempDat$day      <- substr(tempDat$date, 9, 10)
  tempDat$date     <- as.POSIXct(tempDat$date, format = "%Y-%m-%d")
  
  tempDat <- tempDat[order(tempDat$date), ]
  invisible(tempDat) 
  
  ########################
  ### clean up the temp folder
  ########################
  newFiles <- list.files(tempdir(), full.names = TRUE)[!list.files(tempdir()) %in% files.in.tmp]
  invisible(file.remove(newFiles))
  
}