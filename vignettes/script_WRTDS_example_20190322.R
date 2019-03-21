### Weighted regression using time, discharge, and season
### Application of EGRET tools to DataForEver data
library(plyr)
library(SFNRC)

# Meta-analysis -----------------------------------------------------------

targStns    <- c("S333", "S12A", "S12B", "S12C", "S12D", "S151") #, "S343", "S332")
targAnalyte <- "PHOSPHATE, TOTAL AS P"


test <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                 wq_data = wqDat, flow_data = hydDat)))

lapply(test, fluxBiasMulti)
lapply(test, plotConcPred)
lapply(test, plotFluxPred)
lapply(test, plotConcHist)
lapply(test, plotFluxHist)
lapply(test, boxResidMonth)
lapply(test, multiPlotDataOverview)
lapply(test, tableChange)

annualSeries <- lapply(test, makeAnnualSeries)
monthlyResults <- lapply(test, calculateMonthlyResults)

mdiff     <- 0.04
yearStart <- 1985
yearEnd   <- 2015

lapply(test, plotDiffContours, yearStart, yearEnd, qBottom, qTop, qUnit = 1, maxDiff = mdiff)


plot15(test[[4]], yearStart = yearStart, yearEnd = yearEnd)
tableResults(test[[1]])
tableFlowChange(test[[1]])
# plotDiffContours(test[[1]], yearStart, yearEnd, qBottom, qTop, qUnit = 1, maxDiff = mdiff)
lapply(test, tableFlowChange)



# Discharge-time plots ----------------------------------------------------

# Multi-line plots:
q1 <- 100
q2 <- 500
q3 <- 1000
centerDate <- "08-01"
yearStart <- 1980
yearEnd   <- 2017
lapply(test, plotConcTimeSmooth, q1, q2, q3, centerDate = centerDate, yearStart = yearStart, yearEnd = yearEnd)

# plotConcTimeSmooth(eList, q1, q2, q3, centerDate = centerDate, yearStart = yearStart, yearEnd = yearEnd)
clevel    <- seq(0, 0.15, 0.01)
clevel2    <- seq(0, 0.05, 0.005)
maxDiff   <- 0.1
yearStart1 <- 2008
yearEnd1   <- 2013
yearStart2 <- 2014
yearEnd2   <- 2019

qBottom   <- 0.1
qTop      <- 2500

lapply(test[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd1, qBottom = qBottom, qTop =qTop, 
             contourLevels = clevel2, qUnit=1)
lapply(test[-2], plotContours, 
       yearStart = yearStart2, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)
lapply(test[-2], plotContours, 
       yearStart = yearStart1, yearEnd = yearEnd2, qBottom = qBottom, qTop =qTop, 
       contourLevels = clevel2, qUnit=1)

lapply(test[-2], plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)

# plots difference between two years
lapply(test[-2], plotDiffContours, 
       year0 = 1980, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)



# geogenic solute ---------------------------------------------------------


targAnalyte <- "TURBIDITY"
targAnalyte <- "HARDNESS AS CACO3"


Ca <- lapply(targStns, function(stnSelect) 
  modelEstimation(convertToEgret(stn = stnSelect, target_analyte = targAnalyte, 
                                 wq_data = wqDat, flow_data = hydDat)))

level_Ca    <- seq(0, 0.15, 0.01)
maxDiff_Ca  <- NA

lapply(Ca, plotDiffContours, 
       year0 = yearStart1, year1 = yearEnd1, qBottom = qBottom, qTop =qTop, 
       maxDiff = maxDiff, qUnit=1)
