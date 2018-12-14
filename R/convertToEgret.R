#' Convert DataForEver output to EGRET-compatible input
#'
#' @param stn target station. Not case-sensitive.
#' @param target_analyte Water quality parameter of interest. Internally converted to R-friendly form (no commas, hyphens, spaces).
#' @param wq_data water quality dataframe. Product of \code{\link{getWQ}}
#' @param flow_data flow dataframe. Product of \code{\link{getHydro}}
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
#' # wq_dat <- wqDat[(wqDat$stn %in% targStn) & (wqDat$param %in% targAnalyte), ]
#' # flow_dat <- hydDat[hydDat$stn %in% targStn, ]
#' 
#' eList <- convertToEgret(stn = targStn, target_analyte = targAnalyte, 
#'      wq_data = wqDat, flow_data = hydDat)

convertToEgret <- function(stn, target_analyte, wq_data = NULL, flow_data = NULL, interact = FALSE,
                           paStart = 10, paLong = 12, watershedKm = 1, removeNegativeFlow = TRUE) {
  ### function converts DataForEver data to EGRET format for WRTDS analysis
  
  stn <- toupper(stn)
  ### make analyte name rFriendly
  target_analyte <-  gsub(x = target_analyte, pattern = " |,",   replacement = "")
  target_analyte <-  gsub(x = target_analyte, pattern = "-|[+]", replacement = "")
  
  ### download data if either dataset is not provided by user
  if (is.null(wq_data)) {
    wq_data     <- getWQ(stns = stn, target_analytes = target_analyte, rFriendlyParamNames = TRUE)
  }
  if (is.null(flow_data)) {
    flow_data   <- getHydro(stns = stn, parameter_list = "flow", data_shape = "wide")
  }
  wq_data$param <- gsub(x = wq_data$param, pattern = " |,", replacement = "")
  wq_data$param <- gsub(x = wq_data$param, pattern = "-|[+]", replacement = ".")
  wq_data       <- wq_data[(wq_data$stn %in% stn) & (wq_data$param %in% target_analyte), ]
  flow_data     <- flow_data[flow_data$stn %in% stn, ]
  
  ### EGRET doesn't support negative flow days - remove them here and announce to user
  ### a more bespoke user-defined approach is preferred
  if (removeNegativeFlow) {
    if (nrow(flow_data[c(flow_data$flow  < 0), ]) > 0 ) {
      cat(nrow(flow_data[c(flow_data$flow  < 0), ]), " negative discharge measurements were removed from the dataset in pre-processing")
      flow_data     <- flow_data[-c(flow_data$flow  < 0), ]
    }
  }
  
  ### prep daily dataframe (flow data)
  flow_data$code     <- ""
  flow_data$dateTime <- as.Date(flow_data$date) # as.character(as.Date(flow_data$date)) ### dateTime column is critical
  names(flow_data)[names(flow_data) %in% c("flow")] <- c("value")
  flow.daily         <- EGRET::populateDaily(rawData = flow_data, qConvert = 1/0.0283168, 
                                             interactive = interact) # convert cubic feet per second to cubic meters per second
  
  ### prep sample dataframe (water quality data)
  # ConcLow	 numeric	 Lower limit of concentration
  # ConcHigh	 numeric	 Upper limit of concentration
  # Uncen	 integer	 Uncensored data (1=TRUE, 0=FALSE)
  wq_data$dateTime <- as.Date(wq_data$date) # non-integers were produced when using as.character(as.Date(wq_data$date))
  wq_data$ConcLow  <- wq_data$ConcHigh <- wq_data$ConcAve <- wq_data$value  # area for improvement
  wq_data$Uncen    <- 1 # area for improvement
  Sample.data      <- EGRET::populateSampleColumns(rawData = wq_data)
  
  ### generate INFO metadata 
  INFO.data <- createInfo(wq_data = wq_data, paStart = paStart, # see output for ?EGRET::INFOdataframe. starting month for analysis
                          paLong = paLong, watershedKm = watershedKm)
  
  ### merge them
  eList_orig <- EGRET::mergeReport(INFO = INFO.data, Daily = flow.daily, Sample = Sample.data,
                                   surfaces = NA, interactive = interact)
  eList_orig
}