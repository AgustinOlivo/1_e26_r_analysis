library(REddyProc)
library(dplyr)
library(bigleaf)

# Gap-filling and partitioning --------------------------------------------
###import half-hourly data
##change years 
setwd("C:/Users/daphn/OneDrive - University of Saskatchewan/Micromet_CSV/2023")

EddyData <- fLoadTXTIntoDataframe("sorted_CO2flux.txt")
EddyData <- filterLongRuns(EddyData, "NEE")

#Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YMDH',Year = 'Year', Month = 'month', Day = 'day', Hour = 'hour') %>% 
  filterLongRuns("NEE")

#Convert CO2flux ug/m2s to NEE umol/m2s
EddyDataWithPosix$NEE <- EddyDataWithPosix$NEE / 44.01

#Calculate VPD
EddyDataWithPosix$VPD <- fCalcVPDfromRHandTair(EddyDataWithPosix$rH, EddyDataWithPosix$Tair)

EProc <- sEdEddyDataWithPosixEProc <- sEddyProc$new(
  'Saskatoon', EddyDataWithPosix, c('NEE','Rg', 'Tair', 'VPD','Ustar'))

EProc$sSetLocationInfo(LatDeg = 52.17, LongDeg = -106.72, TimeZoneHour = -6)  #changed to Stoon site

#create crop ustar seasons - load correct one
seasonStarts <- as.data.frame(do.call(rbind, list(c(1,2021), c(138,2021), c(302,2021))))
seasonStarts <- as.data.frame(do.call(rbind, list(c(1,2022), c(146,2022), c(243,2022))))
seasonStarts <- as.data.frame(do.call(rbind, list(c(1,2023), c(149,2023), c(264,2023))))
seasonStarts <- as.data.frame(do.call(rbind, list(c(1,2024), c(149,2024), c(214,2024), c(301,2024))))

seasonFactor <- usCreateSeasonFactorYdayYear(EddyDataWithPosix$DateTime, starts = seasonStarts)
plot(NEE ~ DateTime, EddyDataWithPosix)
seasonStartsDate <- fConvertTimeToPosix(data.frame(Year = seasonStarts[,2], 
                                                   DoY = seasonStarts[,1], Hour = 0.5), 
                                                  'YDH', Year = "Year", Day = "DoY", Hour = "Hour")
abline(v = seasonStartsDate$DateTime)
summary(seasonStartsDate)

EProc$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 100L, probs = c(0.05, 0.50, 0.95))
EProc$useSeaonsalUStarThresholds()
EProc$sGetUstarScenarios()

###gap-filling
#filling the gaps in NEE using information of the valid data
#use the same annual u∗ threshold estimate in each season, and decide to compute uncertainty also for valid records (FillAll).
EProc$sMDSGapFillUStarScens('NEE', FillAll = TRUE)
grep("NEE_.*_f$",colnames(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",colnames(EProc$sExportResults()), value = TRUE)

###partioning net flux into GPP and Reco
#distinguish carefully between night-time and day-time. 
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)   
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     

EProc$sMRFluxPartitionUStarScens()
EProc$sApplyUStarScen(EProc$sMRFluxPartition )

grep("GPP.*_f$|Reco", names(EProc$sExportResults()), value = TRUE)

#view night-based flux partitioning fingerprint
EProc$sPlotFingerprintY('GPP_U50_f', Year = 2024)
EProc$sPlotFingerprintY('Reco_U50', Year = 2024)
EProc$sPlotFingerprintY('NEE_U50_f', Year = 2024)

# Uncertainty -------------------------------------------------------------
#NEE uncertainty (random and systematic)
results <- EProc$sExportResults() %>% 
  mutate(
    resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA)
    ,NEE_orig_sd = ifelse(
      is.finite(.data$NEE_uStar_orig), .data$NEE_uStar_fsd, NA))

acf(results$resid, na.action = na.pass, main = "")

autoCorr <- lognorm::computeEffectiveAutoCorr(results$resid)
nEff <- lognorm::computeEffectiveNumObs(results$resid, na.rm = TRUE)
c(nEff = nEff, nObs = sum(is.finite(results$resid)))

resRand <- results %>% summarise(
  nRec = sum(is.finite(NEE_orig_sd))
  , NEEagg = mean(NEE_uStar_f, na.rm = TRUE)
  , varMean = sum(NEE_orig_sd^2, na.rm = TRUE) / nRec / (!!nEff - 1)
  , seMean = sqrt(varMean) 
  , seMeanApprox = mean(NEE_orig_sd, na.rm = TRUE) / sqrt(!!nEff - 1)
) %>% select(NEEagg, seMean, seMeanApprox)

resRand

computeMeanNEE <- function(ds, suffix){
  column_name <- paste0("NEE_",suffix,"_f")
  mean(ds[[column_name]])
}
FilledEddyData <- EProc$sExportResults()
NEEagg <- unlist(EProc$sApplyUStarScen(computeMeanNEE, FilledEddyData))
NEEagg

sdNEEagg_ustar <- sd(NEEagg)
sdNEEagg_ustar

###combined aggregated NEE + uncertainty  
sdAnnual <- data.frame(
  sdRand = resRand$seMean,
  sdUstar = sdNEEagg_ustar,
  sdComb = sqrt(resRand$seMean^2 + sdNEEagg_ustar^2) 
)
sdAnnual
sdAnnual$sdComb

#RESULTS
#2021
NEEAggsd <- 0.0233 * 1e-6 * 12.011 * 3600*24*365.25
NEEAgg <- 0.234 * 1e-6 * 12.011 * 3600*24*365.25
print(NEEAgg)
print(NEEAggsd)
#2022
NEEAggsd <- 0.0417 * 1e-6 * 12.011 * 3600*24*365.25
NEEAgg <- -0.569 * 1e-6 * 12.011 * 3600*24*365.25
print(NEEAgg)
print(NEEAggsd)
#2023
NEEAggsd <- 0.0489 * 1e-6 * 12.011 * 3600*24*365.25
NEEAgg <- -0.292 * 1e-6 * 12.011 * 3600*24*365.25
print(NEEAgg)
print(NEEAggsd)
#2024
NEEAggsd <-  * 1e-6 * 12.011 * 3600*24*365.25
NEEAgg <-  * 1e-6 * 12.011 * 3600*24*365.25
print(NEEAgg)
print(NEEAggsd)

##aggregated GPP + u* uncertainty
FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
GPPAgg <- GPPAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPAgg)

#2021
GPPAgg <- GPPAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPAgg)
GPPustar <- (max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
GPPustar <- 0.0819 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPustar)
#2022
GPPAgg <- GPPAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPAgg)
GPPustar <- (max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
GPPustar <- 0.0518 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPustar)
#2023
GPPAgg <- GPPAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPAgg)
GPPustar <- (max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
GPPustar <- 0.0869 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPustar)
#2024
GPPAgg <- GPPAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPAgg)
GPPustar <- (max(GPPAgg) - min(GPPAgg)) / median(GPPAgg)
GPPustar <- 0.2692 * 1e-6 * 12.011 * 3600*24*365.25
print(GPPustar)

##aggregated Reco + u* uncertainty
FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]
ReAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  ReHalfHour <- FilledEddyData[[paste0("Reco_",suffix)]]
  mean(ReHalfHour, na.rm = TRUE)
})
ReAgg <- ReAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(ReAgg)

#2021
ReAgg <- ReAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(ReAgg)
Reustar <- (max(ReAgg) - min(ReAgg)) / median(ReAgg)
Reustar <- Reustar * 1e-6 * 12.011 * 3600*24*365.25
print(Reustar)
#2022
ReAgg <- ReAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(ReAgg)
Reustar <- (max(ReAgg) - min(ReAgg)) / median(ReAgg)
Reustar <- 0.111 * 1e-6 * 12.011 * 3600*24*365.25
print(Reustar)
#2023
ReAgg <- ReAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(ReAgg)
Reustar <- (max(ReAgg) - min(ReAgg)) / median(ReAgg)
Reustar <- 0.103 * 1e-6 * 12.011 * 3600*24*365.25
print(Reustar)
#2024
ReAgg <- ReAggCO2 * 1e-6 * 12.011 * 3600*24*365.25
print(ReAgg)
Reustar <- (max(ReAgg) - min(ReAgg)) / median(ReAgg)
Reustar <- 0.0066 * 1e-6 * 12.011 * 3600*24*365.25
print(Reustar)


##Gaussian propagation rule - NEE + Cremoved
#2021
x <- 89
sigma_x <- 5
y <- 5
sigma_y <- 0.1
sigma_f <- sqrt(sigma_x^2 + sigma_y^2)
f <- x + y
cat("Result:", f, "±", sigma_f, "\n")

#2022
x <- -216
sigma_x <- 16
y <- 120
sigma_y <- 4
sigma_f <- sqrt(sigma_x^2 + sigma_y^2)
f <- x + y
cat("Result:", f, "±", sigma_f, "\n")

#2023
x <- -111
sigma_x <- 19
y <- 67
sigma_y <- 2
sigma_f <- sqrt(sigma_x^2 + sigma_y^2)
f <- x + y
cat("Result:", f, "±", sigma_f, "\n")


# Export results ----------------------------------------------------------
###store results in csv file
FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
fWriteDataframeToFile(CombinedData, 'Filled2023.txt', Dir =  "C:/Users/daphn/OneDrive - University of Saskatchewan/Micromet_CSV")
