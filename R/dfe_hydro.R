#' @title DataForEver hydrology database API
#'
#' @description Downloads and compiles DataForEver hydrology data. This function Works only on linux machines on the SFNRC network with access to the opt/physical drive. Code issues system commands, runs shell scripts, and modifies files in a temp folder on the local hard drive.
#' 
#' @usage getHydro(stns, parameter_list = c("flow", "tail_water", "head_water", "stage"),
#' data_shape = "long", getWaterQuality = FALSE, ...
#' )
#' 
#' @param stns a single-column dataframe of DataForEver station names. 
#' @param parameter_list vector of desired parameters. 
#' @param data_shape shape of output dataframe. Default is \code{long} (one row per date-station-param) but can also be \code{wide} (one row per date-station) or \code{really_wide} (one row per date)
#' @param getWaterQuality if \code{TRUE}, water quality data are also downloaded and combined into a single dataframe. If \code{getWaterQuality = TRUE}, then \code{data_shape} is automatically set to \code{wide}.
#' @param ... additional arguments sent to \code{dfe.wq}
#' 
#' @return dataframe \code{getHydro} returns a dataframe with water quality measurements from each station identified in \code{stns}.
#' 
#' @seealso \code{\link{getWQ}}
#' 
#' @examples
#' \dontrun{
#' stations <- c("S333", "S12A", "S12B", "S12C", "S12D")
#' 
#'   # usage examples: 
#'   hydro.test  <- getHydro(stns = stations)
#'   hydro.test2 <- getHydro(stns = stations, data_shape = "wide")
#'   hydro.test3 <- getHydro(stns = stations, data_shape = "really_wide")
#'   
#'   # simultaneously grab water quality data:
#'   hydro.test4 <- getHydro(stns = stations, getWaterQuality = TRUE, 
#'        parameter_list = c("flow", "head_water"), target_analytes = "PHOSPHATE, TOTAL AS P|TURBIDITY")
#'        
#'  plot(PHOSPHATETOTALASP ~ flow, data = hydro.test4[hydro.test4$flow > 0, ], pch = 19, las = 1)
#'  plot(PHOSPHATETOTALASP ~ head_water, data = hydro.test4[hydro.test4$flow > 0, ], pch = 19, las = 1)
#'   
#'   
#'   ### to generate the hydDat dataframe included in package:
#'   targetStns <- c("AC01", "BB02", "BB04", "BB05A", "BB06", "BB09", "BB10", "BB11", 
#' "BB14", "BB15", "BB16", "BB17", "BB19", "BB22", "BB23", "BB24", 
#' "BB25", "BB26", "BB27", "BB28", "BB29", "BB31", "BB32", "BB34", 
#' "BB35", "BB36", "BB37", "BB38", "BB39A", "BB41", "BB42", "BB43", 
#' "BB44", "BB45", "BB47", "BB48", "BB52", "BB53", "BISC101", "BISC102", 
#' "BISC103", "BISC104", "BISC105", "BISC106", "BISC107", "BISC108", 
#' "BISC109", "BISC110", "BISC111", "BISC112", "BISC113", "BISC114", 
#' "BISC115", "BISC116", "BISC117", "BISC119", "BISC120", "BISC121", 
#' "BISC122", "BISC123", "BISC124", "BISC125", "BISC126", "BISC127", 
#' "BISC128", "BISC129", "BISC130", "BISC131", "BISC132", "BISC133", 
#' "BISC134", "BISC135", "BL01", "BS01", "LR01", "MI01", "TPBBSW-1", 
#' "TPBBSW-10", "TPBBSW-14", "TPBBSW-1B", "TPBBSW-1T", "TPBBSW-2", 
#' "TPBBSW-2B", "TPBBSW-3", "TPBBSW-3B", "TPBBSW-4", "TPBBSW-4B", 
#' "TPBBSW-4T", "TPBBSW-5", "TPBBSW-5B", "TPBBSW-5T", "TPGW-11D", 
#' "TPGW-11M", "TPGW-11S", "TPGW-14D", "TPGW-14M", "TPGW-14S", "UPKEYS201", 
#' "UPKEYS202", "UPKEYS203", "UPKEYS204", "UPKEYS205", "UPKEYS206", 
#' "UPKEYS207", "UPKEYS208", "UPKEYS210", "S20", "S20F", "S20G", 
#' "S21A", "S21", "S22", "S25", "S25A", "S25B", "S26", "S27", "S28", 
#' "G58", "S700", "G93", "S123", "S197")
#' 
#' hyd.df <- getHydro(stns = targetStns, parameter_list = c("flow", "head_water", "salinity", 
#'      "temperature", "tail_water", "stage", "rainfall", "precipitation", "ppt"), 
#'      data_shape = "wide")
#' }
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' @importFrom stats reshape
#' @importFrom plyr join_all
#' 
#' @export




getHydro <- function(stns, 
                      parameter_list = c("flow", "tail_water", "head_water", "stage"),
                      data_shape = "long", getWaterQuality = FALSE, ...) {
  ### TODO: write bash script using contents of parameter_list rather than post-dl filtering
  
  ### input checks
  if (!is.character(stns)) {
    stop("'stns' must be a character vector. For viable stations, use `masterCoords` or `getStn()`")
  }
  if (!is.character(parameter_list)) {
    stop("'parameter_list' must be a character vector. Use getDataTypes() to check which parameters are available for your stations.")
  }
  if (!is.character(data_shape) || !(length(data_shape) == 1)) {
    stop("'data_shape' must be a character string")
  }
  if (!data_shape %in% c("long", "wide", "really_wide")) {
    stop("'data_shape' must be either 'long', 'wide', or 'really_wide'")
  }
  if (!is.logical(getWaterQuality) || !(length(getWaterQuality) == 1)) {
    stop("'getWaterQuality' must be TRUE or FALSE")
  }
  
  # nocov start
  
  ### temporary hack. will make function more robust
  if(getWaterQuality) {
    data_shape <- "wide"
  }
  
  
  files.in.tmp     <- list.files(tempdir(), recursive = TRUE)
  stn.list.loc     <- file.path(tempdir(), "stn_temp.lst")
  folder_with_data <- file.path(tempdir(), "data")
  bash.script.loc  <- file.path(tempdir(), "bash_hydro_tmp.sh") # wrapper for bash_hydro_sql.sh
  sql.script.loc   <- file.path(tempdir(), "hydro_data.sh") # interfaces with hydro database
  utils::write.table(toupper(stns), file = stn.list.loc, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE)
  
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
                    echo \"Saving $3 data from $station to ${station}_${hydroParam}.dat, if data are available\"
                    ", sql.script.loc, " $station $hydroParam 1960-01-01 $(date +%Y-%m-%d) daily aggregate_statistic validation_level units_converted 		\\
                    | gawk -F \"|\" '{printf\"%s\\t%s\\t%s\\t%s\\n\", $1, $2, $3, $4}' 				\\
                    > $1/${station}_${hydroParam}.dat;   
                    done < $2
                    
                    "
  ), fileConn2)
  close(fileConn2)
  
  system(paste0('chmod 777 ', bash.script.loc))
  system(paste0('chmod 777 ', sql.script.loc))
  ### working version:
  # bash.cmd <- paste0('bash -c "
  #                mkdir ', folder_with_data, '
  #                . set_project hydrology
  #                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' flow
  #                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' stage
  #                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' head_water
  #                /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' tail_water
  #                find ', folder_with_data,' -name \'*.dat\' -size -2k
  #                find ', folder_with_data,' -name \'*.dat\' -size -2k -delete
  # 
  #                "')
  
  ### experimental version: 
  # parameter_list = c("flow", "tail_water", "head_water", "stage", "salinity", "giraffe_sightings")
  
  inner_cmds <- paste0('/bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' ', parameter_list, ' 
                       ')
  
  bash.cmd <- paste0('bash -c "
                     mkdir ', folder_with_data, '
                     . set_project hydrology
                     ', paste(inner_cmds, collapse=""), ' 
                     find ', folder_with_data,' -name \'*.dat\' -size -2k
                     find ', folder_with_data,' -name \'*.dat\' -size -2k -delete
                     
                     "', collapse = "")
  system(bash.cmd)
  
  # system(paste0('bash -c "
  #               mkdir ', folder_with_data, '
  #               . set_project hydrology
  #               /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' flow
  #               /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' stage
  #               /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' head_water
  #               /bin/bash ', bash.script.loc, ' ', folder_with_data, ' ', stn.list.loc, ' tail_water
  #               find ', folder_with_data,' -name \'*.dat\' -size -2k
  #               find ', folder_with_data,' -name \'*.dat\' -size -2k -delete
  #               
  #               "'))
  Sys.sleep(2) # to avoid odd behavior from loading data before downloads finish
  
  ########################
  ### load downloaded files into R
  ########################
  
  ### identify unique params by extracting file names
  # paramNames <- unique(lapply(strsplit(x = rev(gsub(x = list.files(folder_with_data), pattern = ".dat", replacement = "")), split = "_"), "[", 1))
  hydro_col_names <- c("stn", "param", "date", "value")
  dat.files   <- grep(list.files(folder_with_data, full.names = TRUE), pattern = paste0("_", parameter_list, collapse = "|"), value = TRUE)
  
  dat.list <- do.call(rbind, lapply(dat.files, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE, na.strings = "null",
              col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
                                                                                   
  
  # dat.files.flow   <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_flow", value = TRUE)
  # dat.files.stg    <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_stage", value = TRUE)
  # dat.files.hw     <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_head_water", value = TRUE)
  # dat.files.tw     <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_tail_water", value = TRUE)
  # 
  # flow.list <- do.call(rbind, lapply(dat.files.flow, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE, na.strings = "null",
  #                                                                                  col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  # flow.list$value[is.na(flow.list$value)] <- 0
  # hw.list <- do.call(rbind, lapply(dat.files.hw, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
  #                                                                              col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  # hw.list$value <- as.numeric(trimws(hw.list$value))
  # tw.list <- do.call(rbind, lapply(dat.files.tw, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
  #                                                                              col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  # hw.list$value <- as.numeric(trimws(hw.list$value))
  # stg.list <- do.call(rbind, lapply(dat.files.stg, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE,  na.strings = "null",
  #                                                                                col.names = hydro_col_names, colClasses = c("character", "character", "character", "numeric"))))
  
  ########################
  ### merge all the data
  ########################
  # tempDat <- do.call(rbind, list(flow.list, hw.list, tw.list, stg.list))
  # tempDat <- tempDat[tempDat$param %in% parameter_list, ]
  tempDat <- dat.list[dat.list$param %in% parameter_list, ] # seems duplicative
  
  ### data are output in long form. manipulate to wide or really wide if desired.
  if (!grepl(x = data_shape, pattern = "long")) {
    tempDat <- stats::reshape(tempDat, idvar = c("stn", "date"), timevar = "param", direction = "wide")
    names(tempDat) <- gsub(x = names(tempDat), pattern = "value.", replacement = "")
    # names(tempDat) <- c(hydro_col_names[c(1, 3)], "cfs", "hw.ft", "tw.ft", "stg.ft") # changes depending on parameters selected. should define by mapping new names to value.stage etc.
  }
  
  if (grepl(x = data_shape, pattern = "really_wide")) {
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
  mergDat <- tempDat
  ########################
  ### merge with water quality data if TRUE
  ########################
  if (getWaterQuality == TRUE) {
    wq <- getWQ(stns = stns, ...) # target_analytes = toupper(target_analytes)

    wq.temp <- stats::reshape(wq, idvar = c("stn", "date", "year", "mo", "day", "time", "datetime"), timevar = "param", direction = "wide")
    names(wq.temp) <- gsub(x = names(wq.temp), pattern = "value.| |,", replacement = "")
    
    wqDatForMerge <- wq.temp[, c(grep(x = names(wq.temp), pattern = "matrix", value = TRUE, invert = TRUE))] # exclude matrix columns
    mergDat <- plyr::join_all(list(tempDat, wqDatForMerge), by = c("stn", "date"))
  }

  invisible(mergDat) 
  
  ########################
  ### clean up the temp folder
  ########################
  # newFiles <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)[!list.files(tempdir(), recursive = TRUE) %in% files.in.tmp]
  # invisible(file.remove(newFiles))
  
  # nocov end
  
}
