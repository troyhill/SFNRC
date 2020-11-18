#' Convert DataForEver output to EGRET-compatible input
#'
#' @param stn target station. Not case-sensitive.
#' @param target_analyte Water quality parameter of interest. Internally converted to R-friendly form (no commas, hyphens, spaces).
#' @param wq_data water quality dataframe. Product of \code{\link{getDFE} or \link{getDBHYDRO}}. In the \code{convertEgret} output, the date range in \code{wq_data} is modified to be the intersection of \code{wq_data} and \code{flow_data}
#' @param flow_data flow dataframe. Product of \code{\link{getDFE} or \link{getDBHYDROhydro}}. If set to NA, a dataframe of flow = 1.1 m3/s is created and used for analysis. This workaround is designed to allow WRTDS on stations without discharge data (e.g., open-water stations) but may not be mathematically sound. 
#' @param qConvert a conversion factor applied to the flow data. Default is 1 (no conversion). Use 1/0.0283168 to convert cfs to cubic meters per second.
#' @param interact logical Option for interactive mode. If true, there is user interaction for error handling and data checks. FALSE by default
#' @param paStart Starting month of period of analysis. Defaults to 10. Used in most EGRET functions
#' @param paLong Length in number of months of period of analysis. Defaults to 12. Used in most EGRET functions
#' @param watershedKm Watershed area in km2, used to calculate runoff. Defaults to 1
#' @param removeNegativeFlow logical, defaults to TRUE. Indicates whether negative flow data should be removed during pre-processing. EGRET tools do not accommodate negative flows.
#' 
#' @return a list as created by \code{\link[EGRET]{mergeReport}}
#' @export
#'
#' @importFrom EGRET populateDaily
#' @importFrom EGRET populateSampleColumns
#' @importFrom EGRET mergeReport
#
#' @examples
#' targStn <- "S333"
#' targAnalyte <- "PHOSPHATE, TOTAL AS P"
#' 
#' ### subsetting occurs inside the function, so this prep is not necessary
#' # wq_dat <- wqDat[(wqDat$station %in% targStn) & (wqDat$param %in% targAnalyte), ]
#' # flow_dat <- hydDat[hydDat$station %in% targStn, ]
#' \dontrun{ # 20190404: this example causes Appveyor to fail
#' eList <- convertToEgret(stn = targStn, target_analyte = targAnalyte, 
#'      wq_data = wqDat, flow_data = hydDat)
#'      
#' eList_NA <- convertToEgret(stn = targStn, target_analyte = targAnalyte, 
#'      wq_data = wqDat, flow_data = NA)
#'
#' est        <- modelEstimation(eList)
#' est_woFlow <- modelEstimation(eList_NA)
#' 
#' ### not quite the same
#' plotConcPred(est)
#' plotConcPred(est_woFlow)
#'      
#'      }

convertToEgret <- function(stn, target_analyte, wq_data = NULL, flow_data = NULL, qConvert = 1, 
                           interact = FALSE,
                           paStart = 10, paLong = 12, watershedKm = 1, removeNegativeFlow = TRUE) {
  ### function converts DataForEver data to EGRET format for WRTDS analysis
  
  stn <- toupper(stn)
  
  ### setting some column names up top
  ### potential problem if something expects a 'value' column
  concentrationColumn <- "value"
  paramColumn         <- "parameter"
  stationColumn       <- "stn"
  dateColumn          <- "date"
  flowColumn          <- "flow"
  
  ### download data if either dataset is not provided by user
  if (is.null(wq_data)) {
    wq_data <- getDBHYDRO(stn = stn, outputType = "long", parameters = target_analyte)
    wq_data <- DBHYDROToDFE(data = wq_data, type = "waterquality")
    # wq_data     <- data.frame(getDFE(stn = stn, dbname = "waterquality", params = target_analyte, rFriendlyParamNames = TRUE))
  }
  ### identify data sources - DFE or DBHYDRO
  ### WQ data based on ncol == 9 in DBHYDRO (but much wider in DFE)
  if (ncol(wq_data) == 9) { 
    wq_data <- DBHYDROToDFE(data = wq_data, type = "waterquality")
  }
  ### stop if WQ dataset doesn't meet needs.
  if (sum(c(concentrationColumn, paramColumn, stationColumn, dateColumn) %in% names(wq_data)) < 4) {
    stop ("water quality dataset does not appear to have the needed columns: date, stn, parameter, value")
  }
  
  
  ### make analyte name rFriendly - apply same treatment to target_analyte and wq_data$param
  target_analyte     <- gsub(x = target_analyte, pattern = " |,",   replacement = "")
  wq_data[, paramColumn]  <- gsub(x = wq_data[, paramColumn],  pattern = " |,",   replacement = "")
  target_analyte     <- gsub(x = target_analyte, pattern = "-|[+]", replacement = ".")
  wq_data[, paramColumn]  <- gsub(x = wq_data[, paramColumn],  pattern = "-|[+]", replacement = ".")
  
  wq_data       <- data.frame(wq_data[(wq_data[, stationColumn] %in% stn) & (wq_data[, paramColumn] %in% target_analyte), ])
  ### prep sample dataframe (water quality data)
  # ConcLow	 numeric	 Lower limit of concentration
  # ConcHigh	 numeric	 Upper limit of concentration
  # Uncen	 integer	 Uncensored data (1=TRUE, 0=FALSE)
  wq_data$dateTime <- as.Date(wq_data[, dateColumn]) # non-integers were produced when using as.character(as.Date(wq_data$date))
  wq_data$ConcLow  <- wq_data$ConcHigh <- wq_data$ConcAve <- wq_data[, concentrationColumn]  # area for improvement
  wq_data$Uncen    <- 1 # area for improvement
  Sample.data      <- EGRET::populateSampleColumns(rawData = wq_data)
  
  
  
  ### download data if either dataset is not provided by user
  if (is.null(flow_data)) {
    # flow_data   <- data.frame(getDFE(stn = stn, dbname = "hydrology", params = "flow"))
    ### TODO: if above line fails, use getDBHYDROhydro
    ### identify best dbkey
    dbkeys <- getDBkey(stn = stn, type = "FLOW", freq = "DA")
    message ("Flow data was not provided by user. DBHYDRO dataset selected: \n", names(dbkeys), "\n", dbkeys[which.max(dbkeys$`End Date`), ])
    selectedKey <- dbkeys[which.max(dbkeys$`End Date`), ][, 1] # first column has dbkey
    flow_data <- getDBHYDROhydro(dbkey = selectedKey)
    flow_data <- DBHYDROToDFE(data = flow_data, type = "hydrology")
  }
  ### identify data sources - DFE or DBHYDRO
  ### flow data based on ncol == 9 in DBHYDRO (but much wider in DFE)
  if (ncol(flow_data) == 9) { 
    flow_data <- DBHYDROToDFE(data = flow_data, type = "hydrology")
  }
  ### stop if WQ dataset doesn't meet needs.
  if (sum(c(flowColumn, stationColumn, dateColumn) %in% names(flow_data)) < 3) {
    stop ("hydrology dataset does not appear to have the needed columns: date, stn, flow")
  }
  
  
  if (length(flow_data) == 1 && is.na(flow_data)) { 
    # for open-water stations with no discharge, create a series of flow = 1 for every day covering 
    # time span of water quality data
    dateVector_tmp <- unique(wq_data[(wq_data[, stationColumn] %in% stn) & (wq_data[, paramColumn] %in% target_analyte), "date"])
    dateVector <- seq(from = min(dateVector_tmp, na.rm = TRUE), to = max(dateVector_tmp, na.rm = TRUE), by = "day")
    flow_data <- data.frame(stn = rep(stn, times = length(dateVector)),
                            date = dateVector, 
                            flow = rep(0.001, times = length(dateVector)), # negligible but positive flow
                            year = substr(as.character(dateVector), 1, 4),
                            mo   = substr(as.character(dateVector), 6, 7),
                            day  = substr(as.character(dateVector), 9, 10)
                            ) 
  }
  flow_data     <- data.frame(flow_data[flow_data[, stationColumn] %in% stn, ])
  
  ### EGRET doesn't support negative flow days - remove them here and announce to user
  ### a more bespoke user-defined approach is preferred
  if (removeNegativeFlow) {
    if (nrow(flow_data[which(flow_data[, flowColumn] < 0), ]) > 0 ) {
      cat(nrow(flow_data[which(flow_data[, flowColumn] < 0), ]) , " negative discharge measurements were removed from the dataset in pre-processing \n")
      flow_data     <- flow_data[flow_data[, flowColumn]  >= 0, ]
    }
  }
  # nrow(flow_data)
  # nrow(flow_data[!which(flow_data$flow < 0), ])
  # nrow(df[which(df$number1 < df$number2), ])
  # nrow(flow_data[c(as.numeric(flow_data$flow)  < 0), ])
  # flow_dat[c(flow_data$flow  < 0), ]
  ### prep daily dataframe (flow data)
  flow_data$code     <- ""
  flow_data$dateTime <- as.Date(flow_data[, dateColumn]) # as.character(as.Date(flow_data$date)) ### dateTime column is critical
  
  # remove rows with NAs for dates, or else suffer the wrath of a POSIXlt error in EGRET::populateDaily(populateDateColumns())
  flow_data <- flow_data[!is.na(flow_data$dateTime), ]
  names(flow_data)[names(flow_data) %in% c(flowColumn)] <- c("value")
  flow.daily         <- EGRET::populateDaily(rawData = flow_data, qConvert = qConvert, 
                                             verbose = interact) # convert cubic feet per second to cubic meters per second
  
  ### generate INFO metadata 
  INFO.data <- createInfo(wq_data = wq_data, paStart = paStart, # see output for ?EGRET::INFOdataframe. starting month for analysis
                          paLong = paLong, watershedKm = watershedKm, stationColumn = stationColumn)
  
  ### identify intersection of date ranges in sample and flow data
  minDate <- max(min(flow.daily$Date, na.rm = TRUE), min(Sample.data$Date, na.rm = TRUE))
  maxDate <- min(max(flow.daily$Date, na.rm = TRUE), max(Sample.data$Date, na.rm = TRUE))
  
  
  ### merge them
  eList_orig <- EGRET::mergeReport(INFO = INFO.data, 
                                   Daily = flow.daily[(flow.daily$Date >= minDate) & (flow.daily$Date <= maxDate), ], 
                                   Sample = Sample.data[(Sample.data$Date >= minDate) & (Sample.data$Date <= maxDate), ],
                                   surfaces = NA, verbose = interact)
  eList_orig
}
