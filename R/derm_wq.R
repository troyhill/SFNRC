#' @title Process water quality data from Miami-Dade Department of Environmental Resources Management
#'
#' @description Processes DERM data to facilitate analysis and integration with DataForEver data. Adjusts parameter names and units to match DataForEver.
#'
#' @details This function is tailored specifically for DERM datasets.
#' 
#' @usage procDERM(filename = "/opt/physical/troy/RDATA/biscayne/data/data_BBWQall_20180517.txt",
#' remove_QC_codes = c("B|H|J|K|Q|Y|\\\\?|\\\\*"), 
#' new_names = c("stn", "date", "time", "param", "method", "units", 
#' "QCcode", "mdl", "value", "dilution.factor", "depth", "depth.code", 
#' "x", "y", "matrix", "labComments", "project", "genComments", "lab", 
#' "year", "mo", "day", "datetime"))
#' 
#' @param filename a character vector file address for DERM data, saved as a tab-delimited text file.
#' @param remove_QC_codes Quality control codes to remove from the dataset.
#' @param new_names names of columns in output (changing this is not recommended)
#' 
#' @return dataframe \code{procDERM} returns a dataframe with water quality measurements, with parameter names and units that match DataForEver data.
#' 
#' @seealso \code{none}
#' 
#' @examples
#' \dontrun{
#' a <- procDERM()
#' head(a)
#' 
#' ### merge two DERM datasets, keeping duplicates that appear in the more recent dataset
#' b <- procDERM(filename = "/opt/physical/troy/RDATA/biscayne/data/data_DERM_20190401.txt")
#' 
#' a$id <- paste(a$datetime, a$stn, a$param, sep = "-")
#' b$id <- paste(b$datetime, b$stn, b$param, sep = "-")
#' 
#' bbDat <- rbind(b, a[!a$id %in% b$id, ])
#' 
#' # write.csv(bbDat, file = "/opt/physical/troy/RDATA/biscayne/data_for_Donatto_DERM.csv")
#' 
#' }
#' 
#' @importFrom utils write.table
#' @importFrom utils read.delim
#' 
#' @export





procDERM <- function(filename = "/opt/physical/troy/RDATA/biscayne/data/data_BBWQall_20180517.txt",
                    remove_QC_codes = c("B|H|J|K|Q|Y|\\?|\\*"), 
                    new_names = c("stn", "date", "time", "param", "method", 
                                  "units", "QCcode", "mdl", "value", "dilution.factor", 
                                  "depth", "depth.code", "x", "y", "matrix", 
                                  "labComments", "project", "genComments",
                                  "lab", "year", "mo", "day", "datetime")) {
  # nocov start
  
  ### function loads and cleans water quality data from DERM, which are input as tab-delimited text file
  ### removes QC-flagged samples, processes to make it play well with DataForEver output
  ### this code could easily be made irrelevant and may be imperfect
  bbTemp <- utils::read.delim(filename, stringsAsFactors = FALSE, encoding = "latin1")
  
  
  ### Remove QC-flagged samples before averaging etc, 
  ###
  # B = Results based on colony counts outside the acceptable range
  # H = value determined using a field kit not recognized by the DEP as equivalent to lab methods
  # I = reported value is between the practical quantitation limit and MDL ### TDH: CODE NOT EXCLUDED IN MY ANALYSIS (change value to MDL?)
  # J = sample value estimated. see notes to infer usability
  # K = off-scale low value. actual value is known to be less than the value given.
  # M = presence of analyte verified but not quantified. Reported value = practical quantitation limit ### TDH: CODE NOT EXCLUDED IN MY ANALYSIS (change value to MDL?)
  # Q = storage time exceeds QC criteria
  # Y = sample improperly preserved
  # ? = Data rejected on QC grounds and should not be used
  # * = Not reported due to interference
  ###
  bbTemp <- bbTemp[grep(x = bbTemp$LabQualCode,  pattern = remove_QC_codes, invert = TRUE), ] 
  
  ### TODO: correct LabQualCode == "T" to set values to mdl
  
  
  bbTemp$ParamName <- toupper(bbTemp$ParamName)
  
  bbTemp$Units <- gsub(x = bbTemp$Units, pattern = " ", replacement = "")
  bbTemp$Units <- gsub(x = bbTemp$Units, pattern = "%", replacement = "pct")
  # unique(bbTemp$Units)
  bbTemp$Units <- toupper(bbTemp$Units)
  ### fix some typos
  bbTemp$Units[bbTemp$Units %in% "CFU100ML"]                  <- "CFU/100ML"
  bbTemp$Units[bbTemp$Units %in% "MPN100ML"]                  <- "MPN/100ML"
  bbTemp$Units[bbTemp$Units %in% c("METRES", "METERS" )]      <- "METER"
  bbTemp$Units[bbTemp$Units %in% c("^C", "C")]                <- "DEGC" # "^CL" may be a typo? 
  bbTemp$Units[bbTemp$Units %in% "PHUNITS"]                   <- "UNITS"
  bbTemp$Units[bbTemp$ParamName %in% c("PH, FIELD", "PH (FIELD)")]   <- "UNITS"
  bbTemp$Units[bbTemp$ParamName %in% c("CHLOROPHYLL-A")]      <- "MG/M3" # same as ug/L but these are units used in dataForEver
  bbTemp$Units[bbTemp$ParamName %in% c("SALINITY")]           <- "PSU" # salinity is reported in both PSU and PPT
  bbTemp$Units[(bbTemp$ParamName %in% toupper("Dissolved Oxygen (Fld)"))]        <- "MG/L" # one DO value has units of "PPT" - value similar (but not identical!) to the corresponding value currently in DataForEver
  bbTemp$Units[bbTemp$ParamName %in% c("TURBIDITY")]          <- "NTU" # DERM dataset has some "mg/L" values that appear identical to values with units of NTU in the DFE dataset
  
  bbTemp$Units[bbTemp$Units %in% c("UMHMOS/CM", "UMHOS/CMS")] <- "UMHOS/CM" # "uS/cm" is also used
  
  # sort(unique(bbTemp$Units)) # why are there two "MG/L" entries?
  
  bbTemp$year              <- as.numeric(sapply(strsplit(as.character(bbTemp$DateCollected), "/"), "[", 3))
  bbTemp$mo                <- as.numeric(sapply(strsplit(as.character(bbTemp$DateCollected), "/"), "[", 1))
  bbTemp$day               <- as.numeric(sapply(strsplit(as.character(bbTemp$DateCollected), "/"), "[", 2))
  bbTemp$datetime          <- as.POSIXct(paste0(as.character(bbTemp$DateCollected), as.character(bbTemp$TimeCollected)), format = "%m/%d/%Y%H:%M")
  bbTemp$DateCollected     <- as.POSIXct(as.character(bbTemp$DateCollected), format = "%m/%d/%Y")
  
  # remove stations without coordinates or a name
  bbTemp          <- bbTemp[!is.na(bbTemp$Y_COORD) & !is.na(bbTemp$X_COORD) & !is.na(bbTemp$StationUniqueID), ]
  
  ### make some adjustments to DERM parameter names so they match DataForEver
  bbTemp$ParamName[bbTemp$ParamName %in% toupper("Ammonia Nitrogen")]       <- "AMMONIA-N"
  bbTemp$ParamName[bbTemp$ParamName %in% toupper("Phosphorus, Total (TP)")] <- "PHOSPHATE, TOTAL AS P"
  bbTemp$ParamName[bbTemp$ParamName %in% toupper("Ortho Phosphate (OPO4)")] <- "PHOSPHATE, ORTHO AS P"
  bbTemp$ParamName[bbTemp$ParamName %in% toupper("Nitrate/Nitrite (NOX)")]  <- "NITRATE+NITRITE-N"
  bbTemp$ParamName[bbTemp$ParamName %in% toupper("Dissolved Oxygen (Fld)")] <- "DISSOLVED OXYGEN"
  bbTemp$ParamName[bbTemp$ParamName %in% "PH (FIELD)"]                      <- "PH, FIELD"
  bbTemp$ParamName[bbTemp$ParamName %in% "TOTAL SUSPENDED SOLIDS (TSS)"]     <- "TOTAL SUSPENDED SOLIDS"
  bbTemp$ParamName[bbTemp$ParamName %in% "NITROGEN, ORGANIC"]                <- "KJELDAHL NITROGEN, TOTAL"
  
  bbTemp$ParamName[bbTemp$ParamName %in% c("AMMONIA NITROGEN (DISSOLVED)")]  <- "AMMONIA-N"
  bbTemp$ParamName[bbTemp$ParamName %in% c("SILICA- DISSOLVED")]             <- "SILICA"
  bbTemp$ParamName[bbTemp$ParamName %in% c("TEMPERATURE (FIELD)")]           <- "TEMP"
  bbTemp$ParamName[bbTemp$ParamName %in% c("ZINC")]                          <- "ZINC, TOTAL"
  bbTemp$ParamName[bbTemp$ParamName %in% c("TOTAL SUSPENDED SOLIDS (TSS)" )] <- "TOTAL SUSPENDED SOLIDS"
  bbTemp$ParamName[bbTemp$ParamName %in% c("COPPER")]                        <- "COPPER, TOTAL"
  bbTemp$ParamName[bbTemp$ParamName %in% c("LEAD")]                          <- "LEAD, TOTAL"
  bbTemp$ParamName[bbTemp$ParamName %in% c("NICKEL")]                        <- "NICKEL, TOTAL"
  bbTemp$ParamName[bbTemp$ParamName %in% c("CADMIUM")]                       <- "CADMIUM, TOTAL"
  bbTemp$ParamName[bbTemp$ParamName %in% c("DEPTH")]                         <- "DEPTH, TOTAL"
  
  names(bbTemp) <- new_names
  
  invisible(bbTemp)
  
  # nocov end

}

