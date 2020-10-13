#' @title A parallel-processing version of EGRETci's bootAnnual 
#'
#' @description Runs EGRETci::bootAnnual in parallel. 
#' 
#' @usage ciCalculationsParallel(eList, probs, clusterObject, nBoot = 100, 
#'     blockLength = 200, widthCI = 90, seed_var = 23, ...)
#' 
#' @param eList eList object returned from EGRET's WRTDS
#' @param probs a vector of probabilities
#' @param clusterObject name of cluster object
#' @param nBoot number of bootstrap replicates to run
#' @param blockLength length of block used to identify neighboring water quality samples
#' @param widthCI width of confidence interval (percentiles)
#' @param seed_var seed to use for replicable results
#' @param ... placeholder. Additional	arguments are not used at this time,
#' 
#' @return list \code{ciCalculationsParallel} returns a list of dataframes
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
#' closeAllConnections()
#' CIAnnualResults.tkn <- lapply(nitro, ciCalculations, nBoot = nBoot_CI, 
#'     blockLength = blockLength_var, widthCI = 90)
#' closeAllConnections()
#' }
#' 
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom EGRETci ciBands
#' @importFrom EGRETci bootAnnual
#'  
#' @export



ciCalculationsParallel <- function(eList, probs, clusterObject, nBoot = 100, 
                                    blockLength = 200, widthCI = 90, seed_var = 23, ...) {
  doParallel::registerDoParallel(clusterObject)
  repAnnual <- foreach::foreach(n = 1:nBoot, .packages=c('EGRETci')) %dopar% {
    annualResults <- EGRETci::bootAnnual(eList, 
                                blockLength,
                                startSeed = seed_var)  
  }
  parallel::stopCluster(clusterObject)   
  
  EGRETci::ciBands(eList, repAnnual, probs)
  
}
