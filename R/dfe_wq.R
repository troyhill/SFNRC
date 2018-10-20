#' @title DataForEver water quality database API
#'
#' @description Downloads and compiles DataForEver water quality data. This function Works only on linux machines on the SFNRC network with access to the opt/physical drive. Code issues system commands, runs shell scripts, and modifies files in a temp folder on the local hard drive.
#' 
#' @usage dfe.wq(stns, target_analytes = "all", 
#' matricesToExclude = "analyte_free_water",
#' output_colNames = c("stn", "date", "time", "param", "units", 
#' "matrix", "mdl", "value"),
#' output_colClasses = c("character", "character", "character", 
#' "character", "character", "character", "numeric", "numeric"),
#' rFriendlyParamNames = FALSE)
#' 
#' @param stns a single-column dataframe of DataForEver station names. Case insensitive.
#' @param matricesToExclude character vector specifying any sample matrices to be excluded. Spelling/case must be a perfect match to DataForEver entries. For example, "analyte_free_water" indicates field blanks. Advisible to check output using, e.g., \code{unique(wqDat$matrix)}
#' @param target_analytes grep-style character vector naming analytes of interest. default is "all". e.g., # target_analytes <- c("PHOSPHATE|NITROGEN|AMMONI|SUSPENDED|DISSOLVED OXYGEN|CALCIUM|POTASSIUM|HARDNESS|SODIUM|CHLORIDE|TEMP|CONDUCTIVITY, FIELD|SILICA|LEAD, TOTAL|MAGNESIUM|TURBIDITY|CHLOROPHYLL|MERCURY, TOTAL|SULFATE|ZINC, TOTAL|CHLORDANE|MALATHION|CARBOPHENOTHION|PH, FIELD")
#' @param output_colNames names of columns in output (changing this is not recommended; this argument is more of a placeholder)
#' @param output_colClasses classes of columns in output (changing this is not recommended; this argument is more of a placeholder)
#' @param rFriendlyParamNames TRUE/FALSE; indicates whether parameter names should be modified to be R-friendly (no special characters, commas, or spaces). Advisable for analysis, as this makes analysis easier and pre-empts changes coerced by, e.g., \code{plyr::ddply}
#' @return dataframe \code{dfe.wq} returns a dataframe with water quality measurements from each station identified in \code{stns}.
#' 
#' @seealso \code{\link{dfe.hydro}}
#' 
#' @examples
#' \dontrun{
#' stations <- c("S333", "S12A", "S12B", "S12C", "S12D")
#' 
#' head(dfe.wq(stns = stations, target_analytes = c("PHOSPHATE|NITROGEN|AMMONI")))
#' }
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' 
#' @export


### DataForEver water quality data: downloads data and compiles a dataframe of water quality data based on a list of stations. 
### Works only on linux, issues system commands and modifies files on disk
dfe.wq <- function(stns, target_analytes = "all", 
                   matricesToExclude = "analyte_free_water",
                   output_colNames = c("stn",         "date",      "time",      "param",     "units",     "matrix",    "mdl",       "value"),
                   output_colClasses = c("character", "character", "character", "character", "character", "character", "numeric", "numeric"),
                   rFriendlyParamNames = FALSE) {
  # stns = a single-column dataframe of DataForEver station names
  # target_analytes = grep-style character vector naming analytes of interest. default is "all". e.g., # target_analytes <- c("PHOSPHATE|NITROGEN|AMMONI|SUSPENDED|DISSOLVED OXYGEN|CALCIUM|POTASSIUM|HARDNESS|SODIUM|CHLORIDE|TEMP|CONDUCTIVITY, FIELD|SILICA|LEAD, TOTAL|MAGNESIUM|TURBIDITY|CHLOROPHYLL|MERCURY, TOTAL|SULFATE|ZINC, TOTAL|CHLORDANE|MALATHION|CARBOPHENOTHION|PH, FIELD")
  # output_colNames = names of columns in output (should correspond to extractDataForEver_WQ_wMDL.sh)
  # output_colClasses = classes of columns in output (should correspond to extractDataForEver_WQ_wMDL.sh)
  # requires permissions on the shell script, access to NPS processes etc. 
  # TODO: select date ranges, option for outputting wide data
  
  files.in.tmp     <- list.files(tempdir(), recursive = TRUE)
  stn.list.loc     <- file.path(tempdir(), "stn_temp.lst")
  folder_with_data <- file.path(tempdir(), "data")
  bash.script.loc  <- file.path(tempdir(), "bash_temp.sh")
  
  utils::write.table(toupper(stns), file = stn.list.loc, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE)
  
  
  ### create bash script to download water quality data
  fileConn<-file(bash.script.loc)
  writeLines(c("#!/bin/bash
               while read station; do  
               echo \"reading nutrient data from $station\"
               echo \"redirecting stdout to: $station.dat\"
               echo \"select * from results where station='$station'\"  			\\
               | sql \'|\' \\
               | gawk -F \"|\" '{printf\"%s\\t%s\\t%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n\", $1, $2, $3, $4, $5, $6, $7, $8}' 	\\
               > $1/\"$station\"_wq.dat;  
               done < $2
               "
  ), fileConn)
  close(fileConn)
  
  system(paste0('chmod 777 ', bash.script.loc))
  
  system(paste0('bash -c "
                mkdir ', folder_with_data, '
                . set_project waterquality
                /bin/bash ', bash.script.loc, ' ', folder_with_data,' ', stn.list.loc, '
                find ', folder_with_data,' -name \'*.dat\' -size -2k
                find ', folder_with_data,' -name \'*.dat\' -size -2k -delete
                
                "')) 
  
  ### It would be ideal to include QC flags with the data.
  
  Sys.sleep(2) # This seems necessary to avoid loading data prior to download completion
  # now load downloaded files into R
  dat.files   <- grep(list.files(folder_with_data, full.names = TRUE), pattern = "_wq", value = TRUE)
  # unique_stns <- unique(sapply(strsplit(list.files(folder_with_data), split = "_"), "[", 1))
  
  # https://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
  tempDat <- do.call(rbind, lapply(dat.files, function(x) utils::read.delim(x, stringsAsFactors = FALSE, header = FALSE, 
                                                                     col.names = output_colNames,
                                                                     colClasses = output_colClasses)))
  
  if (!target_analytes %in% "all") {
    tempDat <- tempDat[tempDat$param %in% target_analytes, ]
  }
  
  if (rFriendlyParamNames) {
    # modify column with parameter names to not have commas, +/- signs
    tempDat[, output_colNames[4]] <-  gsub(x = tempDat[, output_colNames[4]], pattern = " |,", replacement = "")
    tempDat[, output_colNames[4]] <-  gsub(x = tempDat[, output_colNames[4]], pattern = "-|[+]", replacement = ".")
  }
  
  ########################
  ### clean up the temp folder
  ########################
  # newFiles <- list.files(tempdir(), full.names = TRUE, recursive = TRUE)[!list.files(tempdir(), recursive = TRUE) %in% files.in.tmp]
  # file.remove(newFiles)
           
                                   
  ########################
  ### clean up the data
  ########################
  tempDat$year     <- substr(tempDat$date, 1, 4) #as.numeric(sapply(strsplit(tempDat$date, "-"), "[", 1))
  tempDat$mo       <- substr(tempDat$date, 6, 7) #as.numeric(sapply(strsplit(tempDat$date, "-"), "[", 2))
  tempDat$day      <- substr(tempDat$date, 9, 10) #as.numeric(sapply(strsplit(tempDat$date, "-"), "[", 3))
  tempDat$datetime <- as.POSIXct(paste0(tempDat$date, tempDat$time), format = "%Y-%m-%d%H%M")
  tempDat$date     <- as.POSIXct(tempDat$date, format = "%Y-%m-%d")
  
  tempDat$units <- toupper(tempDat$units)
  tempDat$units <- gsub(x = tempDat$units, pattern = " ", replacement = "")
  tempDat$units[tempDat$units %in% "PPT"]       <- "PSU"
  tempDat$units[tempDat$units %in% c("METERS")] <- "METER"
  tempDat$units[tempDat$units %in% "MG/M^3"]    <- "MG/M3"
  
  tempDat$param[tempDat$param %in% c("AMMONIA, TOTAL AS N", "NITROGEN, AMMONIA AS NH4")] <- "AMMONIA-N"
  tempDat$param[tempDat$param %in% c("temp")] <- "TEMP"
  
  ### remove QC samples
  tempDat <- tempDat[!tempDat$matrix %in% c(matricesToExclude), ]
  
  invisible(tempDat) 
  
}

