#' @title Extract useful information from bootstrapped WRTDS output
#'
#' @description Summarizes concentration and/or flux trends identified by EGRETci utilities.
#' 
#' @usage eBootReport(data, eList = NULL,
#' flux = FALSE, conc = TRUE, text = FALSE)
#' 
#' @param data eBoot results from EGRETci
#' @param eList a list (of data frames, models, etc.)
#' @param flux logical, indicating whether output is desired for fluxes
#' @param conc logical, indicating whether output is desired for concenctrations
#' @param text logical, indicates whether text output should be printed to the console
#' 
#' @return list \code{eBootReport} returns a list the same length as list1 and list2
#' 
#' 
#' @examples
#' \dontrun{
#' registerDoParallel(cl)
#' nitro <- lapply(targStns, function(stnSelect)  
#'   modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte,  
#'                                    wq_data = wqTemp, flow_data = hydDat)))
#'                                    stopCluster(cl)
#' nitro <- lapply(nitro, setPA, paStart = WaterYearStart, paLong = 12)
#' 
#' caseSetUp.tkn <- mapLists(trendSetUp, nitro, list2 = NULL, 
#'     year2 = list(2017, 2017, 2015, 2015, 2007, 2013), year1 = startDate, 
#'     nBoot = nBoot_var, min = 100, blockLength = blockLength_var, bootBreak = 100)
#' 
#' eBoot.tkn <- mapLists(wBT, nitro, caseSetUp.tkn)
#' 
#' eBootReport(data = eBoot.tkn[[1]]$pConc)
#' }
#'  
#' @importFrom stats ecdf
#' @importFrom stats median
#' @importFrom stats mad
#' 
#' @export



eBootReport <- function(data, eList = NULL,
                        flux = FALSE, conc = TRUE, text = FALSE) {
  if (!is.null(eList)) {
    stn   <- eList$INFO$staAbbrev
    param <- eList$INFO$paramShortName
  } else {
    stn   <- NA
    param <- NA
  }
  
  xconcDat <- data$xConc[!is.na(data$xConc)]
  xfluxDat <- data$xFlux[!is.na(data$xFlux)]
  concDat  <- data$pConc[!is.na(data$pConc)]
  fluxDat  <- data$pFlux[!is.na(data$pFlux)]
  
  
  if (flux) {
    if (text) {
      cat(paste0("Probability of a decreasing trend in flux: ", round(stats::ecdf(fluxDat)(0), 2), "\n\n")) # odds of a decline in consituent X
      cat(summary(fluxDat))
      cat("\n")
    }
  } 
  if (conc) { 
    if (text) {
      cat(paste0("Probability of a decreasing trend in concentration: ", round(stats::ecdf(concDat)(0), 2), "\n\n")) # odds of a decline in consituent X
      summary(concDat)
    }
  }
  a <- data.frame(
    stn          = stn,
    param        = param,
    xConcSubZero = stats::ecdf(xconcDat)(0),
    #xConcMean    = mean(xconcDat, na.rm = TRUE), 
    xConcMed     = stats::median(xconcDat, na.rm = TRUE), 
    xConcMAD     = stats::mad(xconcDat, na.rm = TRUE, constant = 1), 
    xFluxSubZero = stats::ecdf(xfluxDat)(0),
    xFluxMed     = stats::median(xfluxDat, na.rm = TRUE),
    xFluxMAD     = stats::mad(xfluxDat, na.rm = TRUE, constant = 1), 
    
    pConcSubZero = stats::ecdf(concDat)(0),
    pConcMed     = stats::median(concDat), 
    pFluxSubZero = stats::ecdf(fluxDat)(0),
    pFluxMed     = stats::median(fluxDat))
  
  a
}