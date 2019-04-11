### Weighted regression using time, discharge, and season
### Application of EGRET tools to DataForEver data

# library(plyr)
library(SFNRC)
library(EGRET)
library(EGRETci)
library(doParallel)

mapLists <- function(fns = plotConcHistBoot, list1 = tp, list2 = CIAnnualResults, ...) {
  if (!is.null(list2)) {
    Map(
    function(fn, value1, value2, ...)
    {
      fn(value1, value2, ...)
    },
    list(fns),
    list1, 
    list2, ...
  )
  } else if (is.null(list2)) {
    Map(
      function(fn, value1, ...)
      {
        fn(value1, ...)
      },
      list(fns),
      list1, 
      ...
    )   
  }
}

ciCalculations.parallel <- function(eList, probs = probs, clusterObject = cl, nBoot = 100, blockLength = 200, widthCI = 90, seed_var = 23, ...) {
  registerDoParallel(clusterObject)
  repAnnual <- foreach(n = 1:nBoot,.packages=c('EGRETci')) %dopar% {
    annualResults <- bootAnnual(eList, 
                                blockLength,
                                startSeed = seed_var)  
  }
  stopCluster(clusterObject)   
  
  ciBands(eList, repAnnual, probs)
  
}

t.start <- Sys.time()
startDate <- 1992 # year of consent decree
QCdateColors <- c("gray85", "gray60", "black")

nCores <- parallel::detectCores() - 1
seed_var <- seed   <- 23
nBoot_var <- 100
nBoot_CI  <- 50
blockLength_var <- 200

coreOut <- 1 #Number of cores to leave out of processing tasks
widthCI <- 90
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)
WaterYearStart <- 05 # May

# TP -----------------------------------------------------------

targStns    <- c("S333", "S12A", "S12B", "S12C", "S12D", "S151") #, "S343", "S332")
targAnalyte <- "PHOSPHATE, TOTAL AS P"
 
# remove 8ppm TP data point
# wqDat[(wqDat$stn %in% "S12A") & (wqDat$param %in% "PHOSPHATE, TOTAL AS P") & (wqDat$value > 8) & complete.cases(wqDat), ]
wqDat[(wqDat$stn %in% "S12A") & (wqDat$param %in% "PHOSPHATE, TOTAL AS P") & (wqDat$value > 8) & complete.cases(wqDat), "value"] <- NA

cl <- makePSOCKcluster(nCores)
registerDoParallel(cl)
tp <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                 wq_data = wqDat, flow_data = hydDat), run.parallel = TRUE))
stopCluster(cl)

### set water year (default is 10, 12)
tp <- lapply(tp, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(tp, plotConcPred)
lapply(tp, plotFluxPred)
lapply(tp, plotResidPred)
lapply(tp, plotResidQ)


# figure 1
lapply(tp, plotConcHist, concMax = 0.04, yearStart = 1978)
lapply(tp, plotFluxHist, fluxMax = 0.016, yearStart = 1978)

caseSetUp <- mapLists(trendSetUp, tp, list2 = NULL, year2 = list(2017, 2017, 2017, 2017, 2017, 2013), year1 = startDate, 
         nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
         bootBreak = 100)


# caseSetUp.S12C <- trendSetUp(tp[[5]], year1 = startDate, year2 = 2017, 
#         nBoot = 10, min = 100, blockLength = 100,
#         bootBreak = 100)
# eBoot.12c <- wBT(tp[[5]], caseSetUp.S12C)

### an attempt to batch process confidence intervals for all stations
# eBoot <- lapply(tp[[4]], wBT, caseSetUp[[4]]) # goal: get this to work for full list!
fns <- list(wBT)
arg1 <- tp
arg2 <- caseSetUp
eBoot <- Map(
  function(fn, value1, value2)
  {
    fn(value1, value2)
  },
  fns,
  arg1, 
  arg2
)

closeAllConnections()
CIAnnualResults <- lapply(tp, ciCalculations, nBoot = nBoot_CI, blockLength = blockLength_var, widthCI = 90) # 1.66 hours for nBoot = 10
closeAllConnections()


# b <- Sys.time()
# CIAnnualResults2 <- lapply(tp, ciCalculations.parallel, nBoot = 10)
# c <- Sys.time()

mapLists()
mapLists(plotConcHistBoot, tp, CIAnnualResults, yearStart = startDate, concMax = 0.08)
mapLists(plotFluxHistBoot, tp, CIAnnualResults, yearStart = startDate)

             
#Concentration an initial run:
# dashed line = ordinary WRTDS estimate of trend magnitude (approximates the median value for bootstrap replicates)
mapLists(plotHistogramTrend, tp, eBoot, caseSetUp, flux = FALSE) #, xStep = 10, xMin = -100, xMax = 100)
mapLists(plotHistogramTrend, tp, eBoot, caseSetUp, flux = TRUE)


save("tp", "eBoot", "caseSetUp", "CIAnnualResults", file = paste0("TP_data_", Sys.Date(), ".RData"))

# registerDoParallel(cl)
# repAnnual <- foreach(n = 1:nBoot, .packages=c('EGRETci')) %dopar% {
#   annualResults <- lapply(tp, bootAnnual,
#                               blockLength,
#                               startSeed = seed)  
# }
# stopCluster(cl) 
# 
# CIAnnualResults <- ciBands(eList, repAnnual, probs)





# Change in concentration at different discharge ------------------------


yearEnd <- 2000
yearStart <- 1970
q1 <- 3  # 100 cfs
q2 <- 14 # 500 cfs
q3 <- 28 # 1000 cfs

### this looks great
lapply(tp, plotConcTimeSmooth, q1, q2, q3, centerDate = "01-01", yearStart, yearEnd, 
       legendTop = 0.06, legendLeft = yearStart, concMax = 0.06)
lapply(tp, plotConcTimeSmooth, q1, q2, q3, centerDate = "05-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)
lapply(tp, plotConcTimeSmooth, q1, q2, q3, centerDate = "07-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)
lapply(tp, plotConcTimeSmooth, q1, q2, q3, centerDate = "10-01", yearStart, yearEnd, 
       legendTop = 0.16, legendLeft = yearStart, concMax = 0.06)



# Contour plots -----------------------------------------------------------
# mdiff     <- 0.04
# yearStart <- 1985
# yearEnd   <- 2015
# 
# lapply(tp, plotDiffContours, yearStart, yearEnd, 
#        qBottom = 0.1, qTop = 2000, qUnit = 1, maxDiff = mdiff)

# plotConcTimeSmooth(eList, q1, q2, q3, centerDate = centerDate, yearStart = yearStart, yearEnd = yearEnd)
clevel    <- seq(0, 0.15, 0.01)
clevel2    <- seq(0, 0.05, 0.005)
maxDiff   <- 0.1
yearStart1 <- 2009
yearEnd1   <- 2019

qBottom   <- 0.1
qTop      <- 2500

lapply(tp[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd1, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)
yearStart2 <- 1979
yearEnd2   <- 1989
lapply(tp[-2], plotContours, 
       yearStart = yearStart2, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel, qUnit=1)


lapply(tp[-2], plotContours, 
       yearStart = yearStart2, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)
lapply(tp[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)

lapply(tp[-2], plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)

# plots difference between two years
lapply(tp[-2], plotDiffContours, 
       year0 = startDate, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)




# lapply(tp, fluxBiasMulti)
# lapply(tp, plotConcPred)
# lapply(tp, plotFluxPred)
# lapply(tp, boxResidMonth)
# lapply(tp, multiPlotDataOverview)
# lapply(tp, tableChange)

annualSeries <- lapply(tp, makeAnnualSeries)
monthlyResults <- lapply(tp, calculateMonthlyResults)


plot15(tp[[4]], yearStart = yearStart, yearEnd = yearEnd)
tableResults(tp[[1]])
tableFlowChange(tp[[1]])
# plotDiffContours(tp[[1]], yearStart, yearEnd, qBottom, qTop, qUnit = 1, maxDiff = mdiff)
lapply(tp, tableFlowChange)


# Discharge-concentration relationships -----------------------------------

date1 <- "1985-09-01"
date2 <- "2000-09-01"
date3 <- "2015-09-01"
qBottom <- 0.2
qTop    <- 35
lapply(tp, plotConcQSmooth, date1, date2, date3,  qLow = 1, qTop, 
                concMax= 0.08,legendTop = 0.85, colors = QCdateColors)

# Discharge-time plots ----------------------------------------------------

# Multi-line plots:
q1 <- 3
q2 <- 14
q3 <- 28
centerDate <- "08-01"
yearStart <- startDate
yearEnd   <- 2017
lapply(tp, plotConcTimeSmooth, q1, q2, q3, centerDate = centerDate, legendTop = 0.06, legendLeft = 1970,
       yearStart = yearStart, yearEnd = yearEnd, concMax = 0.06, concMin = 0)

lapply(tp, plotConcTimeDaily)
lapply(tp, plotFluxTimeDaily)










# nitrogen ------------------------------------------------------------------
# grep(x = unique(wqDat$param), pattern = "NITROG|NITRATE|NITRITE|AMMONI", value = TRUE)
targAnalyte <- "KJELDAHL NITROGEN, TOTAL" # "TOTAL NITROGEN" subset of sites have data
registerDoParallel(cl)
nitro <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))
stopCluster(cl)

### set water year (default is 10, 12)
nitro <- lapply(nitro, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(nitro, plotConcPred)
lapply(nitro, plotFluxPred)
lapply(nitro, plotResidPred)
lapply(nitro, plotResidQ)

# figure 1
lapply(nitro, plotConcHist, concMax = 2.5, yearStart = startDate)
lapply(nitro, plotFluxHist, fluxMax = 1.4, yearStart = startDate)


lapply(nitro, plotDiffContours, year0 = 1988,
                 year1 = 2017,
                 qBottom=0.001,
                 qTop=30,
                 maxDiff=1)

lapply(nitro, plotConcQSmooth, date1, date2, date3,  qLow = 1, qTop, 
       concMax= NA,legendTop = 0.85, colors = QCdateColors)



caseSetUp.tkn <- mapLists(trendSetUp, nitro, list2 = NULL, year2 = list(2017, 2017, 2015, 2015, 2007, 2013), year1 = startDate, 
         nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
         bootBreak = 100)
# lapply(nitro, trendSetUp, year1 = startDate, year2 = 2007, # TODO: set all to 2017 except S151 (2007)
#        nBoot = 50, min = 100, blockLength = blockLength_var,
#        bootBreak = 100)

eBoot.tkn <- mapLists(wBT, nitro, caseSetUp.tkn) # S333 and S12C deserve attention - odd behavior - Flux value/post_p returns NA trends after 30 runs
closeAllConnections()
CIAnnualResults.tkn <- lapply(nitro, ciCalculations, nBoot = nBoot_CI, blockLength = blockLength_var, widthCI = 90)
closeAllConnections()

mapLists(plotConcHistBoot, nitro, CIAnnualResults.tkn, yearStart = startDate)
mapLists(plotFluxHistBoot, nitro, CIAnnualResults.tkn, yearStart = startDate)

#Concentration an initial run:
mapLists(plotHistogramTrend, nitro, eBoot.tkn, caseSetUp.tkn, flux = TRUE)
mapLists(plotHistogramTrend, nitro, eBoot.tkn, caseSetUp.tkn, flux = FALSE)



save("nitro", "eBoot.tkn", "caseSetUp.tkn", "CIAnnualResults.tkn", file = paste0("TKN_data_", Sys.Date(), ".RData"))




# geogenic solute ---------------------------------------------------------


targAnalyte <- "HARDNESS AS CACO3"
hist(wqDat[(wqDat$stn %in% "S12C") & (wqDat$param %in% targAnalyte), "value"])
summary(wqDat[(wqDat$stn %in% "S12C") & (wqDat$param %in% targAnalyte), "value"])
hist(hydDat[(hydDat$stn %in% "S12C") , "flow"])

registerDoParallel(cl)
Ca <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))
stopCluster(cl)

test <- getDaily(Ca[[4]])
summary(test)
### set water year (default is 10, 12)
Ca <- lapply(Ca, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(Ca, plotConcPred)
lapply(Ca, plotFluxPred)
lapply(Ca, plotResidPred)
lapply(Ca, plotResidQ)

# figure 1
lapply(Ca, plotConcHist, concMax = 300, yearStart = startDate)
lapply(Ca, plotFluxHist, fluxMax = 200, yearStart = startDate)



# caseSetUp.ca <- lapply(Ca, trendSetUp, year1 = startDate, year2 = 2007, 
#                         nBoot = 50, min = 100, blockLength = blockLength_var,
#                         bootBreak = 100)

caseSetUp.ca <- mapLists(trendSetUp, Ca, list2 = NULL, year2 = list(2017, 2017, 2017, 2017, 2007, 2013), year1 = startDate, 
                       nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
                       bootBreak = 100)
# wbt.s12c <- wBT(Ca[[4]], caseSetUp.ca[[4]])

eBoot.ca <- mapLists(wBT, Ca, caseSetUp.ca) # S12C flux trend is NA wtf
closeAllConnections()
CIAnnualResults.ca <- lapply(Ca, ciCalculations, nBoot = nBoot_CI, blockLength = blockLength_var, widthCI = 90)
closeAllConnections()

mapLists(plotConcHistBoot, Ca, CIAnnualResults.ca, yearStart = startDate)
mapLists(plotFluxHistBoot, Ca, CIAnnualResults.ca, yearStart = startDate)

#Concentration an initial run:
mapLists(plotHistogramTrend, Ca, eBoot.ca, caseSetUp.ca, flux = TRUE)
mapLists(plotHistogramTrend, Ca, eBoot.ca, caseSetUp.ca, flux = FALSE)



save("Ca", "eBoot.ca", "caseSetUp.ca", "CIAnnualResults.ca", file = paste0("Ca_data_", Sys.Date(), ".RData"))







level_Ca    <- seq(0, 0.15, 0.01)
maxDiff_Ca  <- 150

lapply(Ca, plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff_Ca, qUnit=1)



lapply(Ca, plotDiffContours, year0 = 1988,
       year1 = 2017,
       qBottom=0.001,
       qTop=30,
       maxDiff=maxDiff_Ca)



lapply(Ca, plotConcQSmooth, date1, date2, date3,  qLow = 1, qTop, 
       concMax= NA,legendTop = 0.85, colors = QCdateColors)

# Turbidity ---------------------------------------------------------------

targAnalyte <- "TURBIDITY"
registerDoParallel(cl)
ntu <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))
stopCluster(cl)

### set water year (default is 10, 12)
ntu <- lapply(ntu, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(ntu, plotConcPred)
lapply(ntu, plotResidPred)
lapply(ntu, plotResidQ)

# figure 1
lapply(ntu, plotConcHist, concMax = NA, yearStart = startDate)

caseSetUp.ntu <- mapLists(trendSetUp, ntu, list2 = NULL, year2 = list(2017, 2017, 2007, 2007, 2007, 2013), year1 = startDate, 
         nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
         bootBreak = 100)

eBoot.ntu <- mapLists(wBT, ntu, caseSetUp.ntu)
closeAllConnections()
CIAnnualResults.ntu <- lapply(ntu, ciCalculations, nBoot = nBoot_CI, blockLength = blockLength_var, widthCI = 90)
closeAllConnections()

mapLists(plotConcHistBoot, ntu, CIAnnualResults.ntu, yearStart = startDate)
mapLists(plotFluxHistBoot, ntu, CIAnnualResults.ntu, yearStart = startDate)

#Concentration an initial run:
mapLists(plotHistogramTrend, ntu, eBoot.ntu, caseSetUp.ntu, flux = TRUE)
mapLists(plotHistogramTrend, ntu, eBoot.ntu, caseSetUp.ntu, flux = FALSE)



lapply(ntu, plotDiffContours, year0 = 1988,
       year1 = 2017,
       qBottom=0.001,
       qTop=30,
       maxDiff = 10)

save("ntu", "eBoot.ntu", "caseSetUp.ntu", "CIAnnualResults.ntu", file = paste0("ntu_data_", Sys.Date(), ".RData"))


# sodium ------------------------------------------------------------------

targAnalyte <- "SODIUM" # "SODIUM"  or "SODIUM, TOTAL"

registerDoParallel(cl)
sodium <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))
stopCluster(cl)

### set water year (default is 10, 12)
sodium <- lapply(sodium, setPA, paStart = WaterYearStart, paLong = 12)

# diagnostics
lapply(sodium, plotConcPred)
lapply(sodium, plotFluxPred)
lapply(sodium, plotResidPred)
lapply(sodium, plotResidQ)

# figure 1
lapply(sodium, plotConcHist, concMax = 80, yearStart = startDate)
lapply(sodium, plotFluxHist, fluxMax = 60, yearStart = startDate)


lapply(sodium, plotDiffContours, year0 = 1988,
       year1 = 2017,
       qBottom=0.001,
       qTop=30,
       maxDiff = 60)


# lapply(sodium, plotConcQSmooth, date1, date2, date3, qLow = 1, qTop, 
#        concMax= 75,legendTop = 80, legendLeft = 1, colors = QCdateColors)

# caseSetUp.na <- lapply(sodium, trendSetUp, year1 = startDate, year2 = 2007, 
#                         nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
#                         bootBreak = 100)
caseSetUp.na <- mapLists(trendSetUp, sodium, list2 = NULL, year2 = list(2017, 2017, 2017, 2017, 2007, 2013), year1 = startDate,
         nBoot = nBoot_var, min = 100, blockLength = blockLength_var,
         bootBreak = 100)

eBoot.na <- mapLists(wBT, sodium, caseSetUp.na) # S333 flux breaks down but may not cause error
# wbt.s12c <- wBT(sodium[[4]], caseSetUp.na[[4]])

closeAllConnections()
CIAnnualResults.na <- lapply(sodium, ciCalculations, nBoot = 10, blockLength = blockLength_var, widthCI = 90)
closeAllConnections()

mapLists(plotConcHistBoot, sodium, CIAnnualResults.na, yearStart = startDate, concMax = 80)
mapLists(plotFluxHistBoot, sodium, CIAnnualResults.na, yearStart = startDate)

#Concentration an initial run:
mapLists(plotHistogramTrend, sodium, eBoot.na, caseSetUp.na, flux = TRUE)
mapLists(plotHistogramTrend, sodium, eBoot.na, caseSetUp.na, flux = FALSE)

save("sodium", "eBoot.na", "caseSetUp.na", "CIAnnualResults.na", file = paste0("Na_data_", Sys.Date(), ".RData"))



t.tot <-  Sys.time() - t.start

# Open-water station  -----------------------------------------------------
### sampling isn't frequent enough...
# nrow(finDat[(finDat$stn %in% "BL02") & (finDat$param %in% "CHLOROPHYLL-A"), ])

eList_BB <- modelEstimation(convertToEgret(stn = "TPBBSW-1B", target_analyte = "PHOSPHATE, ORTHO AS P",
                           wq_data = finDat, flow_data = NA))


# Figures  -----------------------------------------------------                 

wid <- 4
hgt <- 3.5

for(i in 1:length(tp)) {
  par(mar = c(1, 1, 1, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("TP_conc_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotConcHistBoot(tp[[i]], CIAnnualResults[[i]], yearStart = startDate, concMax = 0.04, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
  par(mar = c(2, 2, 2, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("TP_flux_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotFluxHistBoot(tp[[i]], CIAnnualResults[[i]], yearStart = startDate, fluxMax = 0.016, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
}

for(i in 1:length(nitro)) {
  par(mar = c(1, 1, 1, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("TKN_conc_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotConcHistBoot(nitro[[i]], CIAnnualResults.tkn[[i]], yearStart = startDate, concMax = 2.5, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
  par(mar = c(2, 2, 2, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("TKN_flux_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotFluxHistBoot(nitro[[i]], CIAnnualResults.tkn[[i]], yearStart = startDate, fluxMax = 1.4, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
}
                 
for(i in 1:length(Ca)) {
  par(mar = c(1, 1, 1, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("Ca_conc_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotConcHistBoot(Ca[[i]], CIAnnualResults.ca[[i]], yearStart = startDate, concMax = 300, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
  par(mar = c(2, 2, 2, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("Ca_flux_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotFluxHistBoot(Ca[[i]], CIAnnualResults.ca[[i]], yearStart = startDate, fluxMax = 180, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
}

for(i in 1:length(ntu)) {
  par(mar = c(1, 1, 1, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("NTU_conc_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotConcHistBoot(ntu[[i]], CIAnnualResults.ntu[[i]], yearStart = startDate, concMax = 4, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
}
                 

for(i in 1:length(sodium)) {
  par(mar = c(1, 1, 1, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("Na_conc_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotConcHistBoot(sodium[[i]], CIAnnualResults.na[[i]], yearStart = startDate, concMax = 80, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
  par(mar = c(2, 2, 2, 0.5), fig = c(0,1,0,1))
  png(filename = paste0("Na_flux_", i, ".png"), width = wid, height = hgt, units = "in", res = 150)
  plotFluxHistBoot(sodium[[i]], CIAnnualResults.na[[i]], yearStart = startDate, fluxMax = 50, col.pred = "cornflowerblue", cex.axis = 0.9, cex.main = 0.6)
  dev.off()
}

                 
                 
png(filename = "boxPlot_S12D.png", width = 5, height = 5.5, units = "in", res = 150)
boxQTwice(tp[[5]], logScale = TRUE, pars = list(staplewex = 0), cex = 0.5)
dev.off()

png(filename = "wrtds_conc_S333.png", width = 9, height = 5, units = "in", res = 150)
plotConcTimeDaily(tp[[1]], concMax = 0.25)
dev.off()

png(filename = "wrtds_flux_S333.png", width = 9, height = 5, units = "in", res = 150)
plotFluxTimeDaily(tp[[1]])
dev.off()

png(filename = "wrtds_plotConcPred_S333.png", width = 5, height = 5.5, units = "in", res = 150)
plotConcPred(tp[[1]])
dev.off()

png(filename = "wrtds_plotFluxPred_S333.png", width = 5, height = 5.5, units = "in", res = 150)
plotFluxPred(tp[[1]])
dev.off()

png(filename = "wrtds_fluxBiasMulti_S333.png", width = 9, height = 10, units = "in", res = 150)
fluxBiasMulti(tp[[1]])
dev.off()
             

# simulate S12 operations - TP concentrations -----------------------------
daily <- lapply(tp, getDaily)
for( i in seq_along(daily)){
  daily[[i]]$stn <- targStns[i]
}



daily.long <- rbind.fill(daily)
daily <- join_all(daily, by = c("Date"))
head(daily)
head(daily.long)

ggplot(daily.long, aes(y = ConcDay, x = LogQ, col = stn)) + geom_point() + theme_classic() + xlim(c(1, 5)) + ylim(c(0, 0.09)) + facet_grid(stn ~ Month)

stn.nos <- grep(names(daily), pattern = "stn")
for (i in 1:length(targStns)) {
  if (i == 1) {
    start.point <- 2
    end.point <- stn.nos[i]
  } else {
    start.point <- stn.nos[i - 1] + 1
    end.point <- stn.nos[i]
  }
  names(daily)[start.point:end.point] <- paste0(names(daily)[start.point:end.point], targStns[i])
}

# plot
resh1 <- reshape2::melt(daily, id.vars = Date, measure.vars = grep(names(daily), pattern = "ConcDay"))

boxplot1 <- ggplot()

