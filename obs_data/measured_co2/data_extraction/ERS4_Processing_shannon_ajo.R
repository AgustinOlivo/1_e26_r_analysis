#+++ Maybe this will run ReddyProc. 

library(REddyProc)
library(dplyr)

#filename <- 'C:\\Users\\shan_\\Reddy\\P3_tower_2020_R.txt'
filename <- 'C:\\Users\\aolivo\\OneDrive - University of Guelph\\Desktop\\P12_tower_2022_R.txt'

EddyData <- fLoadTXTIntoDataframe(filename)

EddyData <- filterLongRuns(EddyData, "NEE")

#+++ Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later

EProc <- sEddyProc$new(
  'Plot1_2021', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))

seasonStarts <- as.data.frame( do.call( rbind, list(
  c(1,2018),
  c(138,2018), # emergence
  c(303,2018), # harvest
  c(365,2018), # end of year
  c(1,2019),
  c(150,2019), # emergence
  c(273,2019), # harvest
  c(365,2019), # end of year
  c(1,2020),
  c(143,2020), # emergence
  c(267,2020), # harvest
  c(365,2020), # end of year
  c(1,2021),
  c(134,2021), # emergence
  c(307,2021), # harvest
  c(365,2021), # end of year
  c(1,2022),
  c(138,2022), # emergence
  c(276,2022), # harvest
  c(365,2022), # end of year
  c(1,2023),
  c(136,2023), # emergence
  c(277,2023), # harvest
  c(365,2023) # end of year
  )))
seasonFactor <- usCreateSeasonFactorYdayYear(
  EddyDataWithPosix$DateTime - 15*60, starts = seasonStarts) #this could go to 30

seasonStartsDate <- fConvertTimeToPosix( data.frame(Year = seasonStarts[,2]
  , DoY = seasonStarts[,1], Hour = 0.50), 'YDH'
  , Year = "Year", Day = "DoY", Hour = "Hour")

(uStarTh <- EProc$sEstUstarThold(seasonFactor = seasonFactor))

EProc$useSeaonsalUStarThresholds()
EProc$sMDSGapFillAfterUstar('NEE', FillAll =TRUE, isVerbose = FALSE)

EProc$sMDSGapFillUStarScens('NEE', FillAll = FALSE)
grep("^NEE.*_f$", colnames( EProc$sExportResults()), value = TRUE )

EProc$sMDSGapFillUStarScens('NEE')
grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)

EProc$sSetLocationInfo(LatDeg = 43.6837, LongDeg = -80.4305, TimeZoneHour = -5)  # fix? -4?
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

EProc$sMRFluxPartitionUStarScens()

FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]

GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25

EProc$sEstimateUstarScenarios( 
  nSample = 200, probs = seq(0.025,0.975,length.out = 39) )

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
fWriteDataframeToFile(CombinedData, 'Plot3_2020_Sept_results.txt', 
                      Dir = 'C:\\Users\\shan_\\Reddy')

CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    nee = mean(GPP_uStar_f)*-1
  ) %>% 
  ggplot(aes(x = DoY, y = nee))+
  geom_line()+
  theme_bw()

CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    resp = mean(Reco_uStar)
  ) %>% 
  ggplot(aes(x = DoY, y = resp))+
  geom_line()+
  theme_bw()

