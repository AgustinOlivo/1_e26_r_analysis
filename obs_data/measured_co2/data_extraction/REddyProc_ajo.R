# readyproc data processing
# 06/12/2024
# agustin olivo
# aolivo@uoguelph.ca

###################################################################
####################### general ###################################

# install packages

install.packages("REddyProc")
help('REddyProc-package')
install.packages("psych")
install.packages("openair")

# load libraries

library(REddyProc)
library(dplyr)
library(writexl)
library(ggplot2)
library(psych)
library(openair)

# setting working directory

getwd()
setwd("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis")

# key sources of information:

# source 1: https://rdrr.io/rforge/REddyProc/man/sEddyProc.example.html
# source 2: https://github.com/EarthyScience/REddyProc/blob/master/vignettes/useCase.md
# source 3 (initial REddyProc paper): https://bg.copernicus.org/articles/15/5015/2018/bg-15-5015-2018.html
# source 4 (definition of different friction velocities across the year - u*): https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html
# source 5: https://rstudio-pubs-static.s3.amazonaws.com/84133_4c4347b1ba5e4067980787a85e27d68f.html
# source 6 (video): https://www.youtube.com/watch?v=-b0vc4u8kls

###################################################################

########################################################################
###### option 1: data gapfilling  using user defined uStar-Seasons #####

# there are a few different options on how to conduct this analysis; the one below considers generating different ustar seasons, and also estimating the probability associated with those; in another files that Shannon had compiled, a slightly different approach is used where the seasons are generated, but the probability associated wit the ustar tresholds are not estimated.

# key source of information: https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html

# importing the data

# The workflow starts with importing the half-hourly data. 

EddyData_2018 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2018_R.txt")
EddyData_2019 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2019_R.txt")
EddyData_2020 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2020_R.txt")
EddyData_2021 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2021_R.txt")
EddyData_2022 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2022_R.txt")
EddyData_2023 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2023_R.txt")

EddyData <- rbind(EddyData_2018, EddyData_2019, EddyData_2020, EddyData_2021, EddyData_2022, EddyData_2023) # merging all years in a single dataset
# co2 fluxes are in umolm-2s-1

# filtering out values outside of the range (mostly based on source 5)

EddyData <- mutate(EddyData, 
          NEE = ifelse(abs(NEE) > 60, NA, NEE), # this was not in the original code from source 5, but added it based on my discussion with Patrick. 
          #PAR = pmin(PAR, 2500),
          #PAR = pmax(PAR, 0),
          VPD = ifelse(VPD > 50, NA, VPD), # this was in the original code
          VPD = ifelse(VPD < 0, NA, VPD), # this was not in the original code from source 5, but REddyProc gave me a warning so I added it.
          Rg = pmin(Rg, 1200),
          Rg = pmax(Rg, 0),
          RH = ifelse(abs(RH) > 100, NA, RH), # I added this after realizing there were a few observations with values among 3000s and 6000s 
          )

# for some reason the original file had a doy == 367 for 2020, the seems to correspond to the first measurement of 2021

EddyData <- EddyData %>%
  mutate(Year = ifelse(Year == 2020 & DoY == 367, 2021, Year),
         DoY = ifelse(Year == 2021 & DoY == 367, 1, DoY))

# Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")

# Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")

# initialize R5 reference class sEddyProc for post-processing of eddy data
EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg')) # this command creates an object of class "sEddyProc" with the data from EddyDataWithPosix, and verifies if there are any values outside of pre-defined ranges (via warning message; for NEE it uses values that are less than -50, and for VPD negative values)

# check structure of the data

# estrutura da classe de dados 
str(EProc$sDATA)

# statistical summary of the data

round(describe(EProc$sDATA[,-1]), 2)

# based on source 5 (listed above)

# fingerprint plot: a fingerprint-plot is a color-coded image of the half-hourly fluxes by daytime on the x and and day of the year on the y axis.This may show several gaps

#plots for specific years

EProc$sPlotFingerprintY('NEE', Year = 2019, valueLimits = c(-200,200))
EProc$sPlotFingerprintY('LE', Year = 2019, valueLimits = c(-100,900))
EProc$sPlotFingerprintY('H', Year = 2019, valueLimits = c(-200,200))
EProc$sPlotHHFluxesY('NEE', Year = 2020)

# plots across multiple years (from source 5)

EddyDataWithPosix_oa <- EddyDataWithPosix
EddyDataWithPosix_oa <- select(EddyDataWithPosix_oa, -(Year:Hour))
EddyDataWithPosix_oa <- rename(EddyDataWithPosix_oa, date = DateTime)
timePlot(EddyDataWithPosix_oa, 
         names(EddyDataWithPosix_oa)[-1], 
         ylab = "variables", 
         date.format = "%b\n%Y",
         date.breaks = 16, 
         date.pad = TRUE,
         plot.type = "h",
         scales = "free",
         key = FALSE)

# plor for nee only
EddyDataWithPosix_oa <- EddyDataWithPosix_oa %>%
  mutate(Year = format(date, "%Y"),  # Extract year
         DoY = as.integer(format(date, "%j")))  # Extract day of the year

EddyDataWithPosix_oa %>%
  select(date, NEE, Year, DoY) %>%
  ggplot(aes(x = DoY, y = NEE))+
  geom_line(colour = "red")+
  facet_wrap(Year~., nrow = 4)+
  theme_bw()

#### data gap filling

# definition of season periods 

seasonStarts <- as.data.frame( do.call( rbind, list(
  c(1,2018),
  c(138,2018), # planting
  c(303,2018), # harvest
  c(365,2018), 
  c(1,2019),
  c(150,2019), # planting
  c(273,2019), # harvest
  c(365,2019), 
  c(1,2020),
  c(143,2020), # planting
  c(267,2020), # harvest
  c(365,2020), 
  c(1,2021),
  c(134,2021), # planting
  c(307,2021), # harvest
  c(365,2021), 
  c(1,2022),
  c(138,2022), # planting
  c(276,2022), # harvest
  c(365,2022), 
  c(1,2023),
  c(136,2023), # planting
  c(277,2023), # harvest
  c(365,2023) 
)))

seasonFactor <- usCreateSeasonFactorYdayYear(
  EddyDataWithPosix$DateTime - 15*60, starts = seasonStarts) # in Daphnee's code there is no "-15*60; not sure what this is exactly
seasonStartsDate <- fConvertTimeToPosix( data.frame(Year = seasonStarts[,2]
                                                    , DoY = seasonStarts[,1], Hour = 0.50), 'YDH'
                                         , Year = "Year", Day = "DoY", Hour = "Hour")
# plotting NEE values
plot(NEE ~ DateTime, EddyDataWithPosix)
# plotting the lines for each season
abline(v = seasonStartsDate$DateTime)
# summary of different seasons
summary(seasonStartsDate)

#### data gap filling

# estimating the ustar_threshold distribution

# the estimation of the distribution of uStar thresholds follows, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.

# option 1.a
(uStarTh <- EProc$sEstUstarThold(seasonFactor = seasonFactor))

# option 1.b

# estimation the distribution

EProc$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 100L, probs = c(0.50)) # i kept these scenarios only for 0.5 to make it run quicker, but i should use: c(0.05, 0.50, 0.95)
EProc$sGetEstimatedUstarThresholdDistribution()

#The output reports annually aggregated uStar estimates for the original data and lower, median, and upper quantile of the estimated distribution. The threshold can vary between periods of different surface roughness, or pre-defined seasons. Therefore, there are estimates for different time periods, called seasons (those were defined in previous step)

# The subsequent post processing steps will be repeated using the four $u_$ threshold scenarios (non-resampled and tree quantiles of the bootstrapped distribution). They require to specify a $u_$-threshold for each season and a suffix to distinguish the outputs related to different thresholds. By default the annually aggregated estimates are used for each season within the year.

# calling sEddyProc_useSeaonsalUStarThresholds so the code uses different U start tresholds for different seasons.
# (uStarScens <- usGetSeasonalSeasonUStarMap( EProc$sGetEstimatedUstarThresholdDistribution()))
# EProc$sSetUstarScenarios(uStarScens)
EProc$useSeaonsalUStarThresholds()
EProc$sGetUstarScenarios()

# actual gap-filling

# with option 1.a

EProc$sMDSGapFillAfterUstar('NEE', FillAll = FALSE, isVerbose = FALSE)

# with option 1.b

EProc$sMDSGapFillUStarScens('NEE', FillAll = TRUE)
grep("^NEE.*_f$", colnames( EProc$sExportResults()), value = TRUE) # extract name of the variable that will be used
EProc$sPlotFingerprintY('NEE_uStar_f', Year = 2021) # plotting the gap-filled data for a specific year

# plot to look at the raw data + gap-filled data
FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData) # combining this files helps bring back the doy 
CombinedData$doy <- CombinedData$DoY

# checking the quality of the data after filtering and gap-filling

# NEE = original NEE value
# Ustar = original Ustar value
# Ustar_uStar_Thres = ustar treshold defined for each season
# NEE_uStar_orig = NEE value after ustar filtering
# NEE_uStar_f = NEE value after filtering and gap-filling

# checking values that have Ustar lower than thresholds, but are not excluded for some reason
CombinedData %>%
  filter(Ustar < Ustar_U50_Thres) %>%
  #filter(NEE == NEE_U50_orig) %>%
  select(Ustar, Ustar_uStar_Thres, NEE, NEE_uStar_orig, NEE_uStar_f, Year, DoY) %>%
  group_by(Year) %>% 
  summarise(
    n = n()
  )

CombinedData %>%
  filter(Ustar < Ustar_U50_Thres) %>%
  filter(NEE == NEE_U50_orig) %>%
  filter(Year == 2020) %>% 
  select(Ustar, Ustar_uStar_Thres, NEE, NEE_uStar_orig, NEE_uStar_f, Year, DoY, Ustar_U50_Thres, NEE_U50_orig, NEE_U50_f, season) %>%
  ggplot(aes(x = DoY, y = NEE_U50_orig))+
  geom_point(color = "red")+
  geom_point(data = filter(CombinedData, Year == 2020 & Ustar > Ustar_U50_Thres), aes(x = DoY, y = NEE_U50_orig), color = "black")

CombinedData %>%
  filter(Ustar < Ustar_U50_Thres) %>%
  filter(NEE == NEE_U50_orig) %>%
  filter(Year == 2020) %>% 
  select(Ustar, Ustar_uStar_Thres, NEE, NEE_uStar_orig, NEE_uStar_f, Year, DoY, Ustar_U50_Thres, NEE_U50_orig, NEE_U50_f, season, Hour) %>%
  ggplot(aes(x = DoY, y = NEE_U50_orig))+
  geom_point(data = filter(CombinedData, Year == 2020 ), aes(x = DoY, y = NEE_U50_f), color = "gray")+
  geom_point(color = "red")+
  geom_point(data = filter(CombinedData, Year == 2020 & Ustar > Ustar_U50_Thres & NEE == NEE_U50_orig), aes(x = DoY, y = NEE_U50_orig), color = "black")+
  theme_bw()

CombinedData %>%
  filter(Ustar < Ustar_U50_Thres) %>%
  filter(NEE == NEE_U50_orig) %>%
  filter(Year == 2020) %>% 
  filter(DoY == 183) %>% 
  select(Ustar, Ustar_uStar_Thres, NEE, NEE_uStar_orig, NEE_uStar_f, Year, DoY, Ustar_U50_Thres, NEE_U50_orig, NEE_U50_f, season, Hour) %>%
  ggplot(aes(x = Hour, y = NEE_U50_orig))+
  geom_point(data = filter(CombinedData, Year == 2020 & DoY == 183), aes(x = Hour, y = NEE_U50_f), color = "gray")+
  geom_point(color = "red")+
  geom_point(data = filter(CombinedData, Year == 2020 & DoY == 183 & Ustar > Ustar_U50_Thres), aes(x = Hour, y = NEE_U50_orig), color = "black")+
  theme_bw()

# checking values that have Ustar lower than thresholds, but are excluded
CombinedData %>%
  filter(Ustar < Ustar_uStar_Thres) %>%
  filter(NEE != NEE_uStar_f) %>%
  select(Ustar, Ustar_uStar_Thres, NEE, NEE_uStar_orig, NEE_uStar_f, Year, DoY) %>% 
  group_by(Year) %>% 
  summarise(
    n = n()
  )

# estimating total dapoints for each year
CombinedData %>%
  group_by(Year) %>% 
  summarise(
    n = n()
  )

# partitioning the NEE data into gpp and respiration

EProc$sSetLocationInfo(LatDeg = 43.64079, LongDeg = -80.41301, TimeZoneHour = -5)  
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA) # Gap-filled Tair (and NEE) needed for partitioning     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA) # Gap-filled Tair (and NEE) needed for partitioning     
EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

# night-time method

EProc$sMRFluxPartitionUStarScens() # night time partitioning -> Reco, GPP
EProc$sApplyUStarScen(EProc$sMRFluxPartition )
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE) # extract the name of the variables that will be used

# plot to look at the raw data + gap-filled data
CombinedData %>%
  ggplot(aes(x = doy, y = NEE_uStar_f))+
  geom_line(colour = "black")+
  geom_line(aes(x= doy, y = NEE), colour = "red")+
  facet_wrap(Year~., nrow = 4)+
  theme_bw()








## plotting the results and comparing other partitioning and gap-filling methods

# bring in the data Shannon separated for CO2 for 2018-2021 using Barr et al. (2004)

seasonStarts2 <- seasonStarts
colnames(seasonStarts2) <- c("doy", "year")

# bring in dataset from other partitioning and gap-filling method.

new_co2 <- read.csv("obs_data/measured_co2/co2_jevans_folder.csv", header = FALSE)
colnames(new_co2) <- new_co2[2, ]
new_co2 <- new_co2[-c(1:3),]
new_co2$nee_obs <- as.numeric(new_co2$NEE)
new_co2$gpp_obs <- as.numeric(new_co2$GPP)
new_co2$resp_obs <- as.numeric(new_co2$Re)
new_co2$year <- as.numeric(new_co2$year)
new_co2$source <- "co2_jevans_other"
new_co2 <- new_co2 %>%
  mutate(doy = as.numeric(doy))

# creating a dataset with the reddyproc data 
filled <- CombinedData %>% 
  group_by(doy, Year) %>% 
  summarise(
    nee = mean(NEE_uStar_f),
    resp = mean(Reco_uStar),
    gpp = mean(GPP_uStar_f)
  ) %>% 
  mutate(year = Year)

# combine datasets to be compared

combined <- filled %>% 
  #filter(plot == 1) %>% 
  left_join(filter(new_co2, plot == 1), by = c("year", "doy")) # The key feature of left_join() is that it keeps all the rows from the left data frame and only the matching rows from the right data frame. If no match is found in the right data frame, NA values are inserted in the corresponding columns.

# comparison graphs for nee

combined %>%
  #filter(year == 2020) %>% 
  ggplot(aes(x = doy, y = nee))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = nee_obs), color = "#0072B2", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 4)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# comparison graphs for respiration

combined%>% 
  ggplot(aes(x = doy, y = resp))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = resp_obs), color = "#D55E00", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2018:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# comparison graphs for gpp

combined%>% 
  ggplot(aes(x = doy, y = gpp*(-1)))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = gpp_obs), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2018:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# graphing the sum of the fluxes across the entire year

# new dataset

combined_inner <- new_co2 %>% 
  filter(plot == 1) %>% 
  inner_join(filled, by = c("year", "doy")) # full join keeps only those points that are present in both datasets.

# merged graph

combined_inner %>% 
  filter(year %in% c(2018:2021)) %>%
  group_by(year) %>% 
  summarise(
    nee = sum(nee),
    gpp = sum(gpp)*(-1),
    resp = sum(resp),
    nee_obs = sum(nee_obs),
    gpp_obs = sum(gpp_obs),
    resp_obs = sum(resp_obs)
  ) %>%
  pivot_longer(
    cols = -year,  # All columns except 'year'
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    source = ifelse(grepl("_obs$", variable), "shannon", "reddyproc"),  # Add "old" or "new"
    variable = str_remove(variable, "_obs$")
  ) %>% 
  mutate(variable = factor(variable, levels = c("gpp", "resp", "nee"))) %>%  # Set the desired order
  ggplot(aes(x = source, y = value, fill = interaction(source, variable))) +  # Color based on interaction between source and variable
  geom_col(colour = "black") +
  scale_fill_manual(
    values = c(
      "reddyproc.gpp" = "gray", "shannon.gpp" = "#009E73", 
      "reddyproc.resp" = "gray", "shannon.resp" = "#D55E00", 
      "reddyproc.nee" = "gray", "shannon.nee" = "#0072B2", 
      "reddyproc" = "gray", "shannon" = "white"
    )
  ) +  # Set custom colors for the interaction between source and variable
  facet_grid(vars(variable), vars(year)) +
  theme_bw() +
  geom_text(aes(label = round(value, digits = 0)), vjust = 0.5) +  # Adjust text position if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylab("Annual cumulative C flux (g/m2/year)")


########################################################################

########################################################################
###### option 2: data gapfilling  using ustar_threshold #####

# importing the data

# The workflow starts with importing the half-hourly data. 

EddyData <- fLoadTXTIntoDataframe("data\\measured_co2\\data_extraction\\2018\\REddyProc\\P12_tower\\Input\\P12_tower_2018_R.txt")

EddyData <- fLoadTXTIntoDataframe("data\\measured_co2\\data_extraction\\REddyProc_2019\\P12_tower\\Input\\P12_tower_2019_R.txt")

# Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")

# Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")

# initalize R5 reference class sEddyProc for post-processing of eddy data
EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg'))

#fingerprint

# A fingerprint-plot is a color-coded image of the half-hourly fluxes by daytime on the x and and day of the year on the y axis.This may show several gaps

EProc$sPlotFingerprintY('NEE', Year = 2018, valueLimits = c(-200,200))
EProc$sPlotFingerprintY('LE', Year = 2018, valueLimits = c(-100,900))
EProc$sPlotFingerprintY('H', Year = 2018, valueLimits = c(-200,200))
EProc$sPlotHHFluxesY('NEE', Year = 2018)

#### data gap filling

# estimating the ustar_threshold distribution

# The second step, is the estimation of the distribution of uStar thresholds, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.

# The friction velocity, uStar, needs to be in column named "Ustar" of the input dataset.

EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))
EProc$sGetEstimatedUstarThresholdDistribution()

#The output reports annually aggregated uStar estimates for the original data and lower, median, and upper quantile of the estimated distribution. The threshold can vary between periods of different surface roughness, e.g. before and after harvest. Therefore, there are estimates for different time periods, called seasons. These season-estimates are by default aggregated to entire years.

# The subsequent post processing steps will be repeated using the four $u_$ threshold scenarios (non-resampled and tree quantiles of the bootstrapped distribution). They require to specify a $u_$-threshold for each season and a suffix to distinguish the outputs related to different thresholds. By default the annually aggregated estimates are used for each season within the year.

EProc$sGetUstarScenarios()

#Gap-filling

#The second post-processing step is filling the gaps in NEE using information of the valid data. 

EProc$sMDSGapFillUStarScens('NEE')
grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE) # "_f" denotes the filled value 
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE) # _fsd" denotes the estimated standard deviation of its uncertainty.

EProc$sPlotFingerprintY('NEE_U50_f', Year = 2019)

# partitioning
EProc$sSetLocationInfo(LatDeg = 43.6410, LongDeg = -80.4057, TimeZoneHour = -5)  
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

# night-time method
EProc$sMRFluxPartitionUStarScens() 
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE)
EProc$sPlotFingerprintY('GPP_U50_f', Year = 2019)

# day-time method
#EProc$sGLFluxPartitionUStarScens() #is this correct?

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
CombinedData$doy <- CombinedData$DoY

# or without relying on data.frame EddyData
# with replacing column DateTime by Year, DoY, and Hour:
# fWriteDataframeToFile(
#   cbind(EProc$sExportData(), EProc$sExportResults()), 'DE-Tha-Results_ydh.txt', 
#   isSplitDatetime=TRUE, Dir = tempdir())
# tmp <- fLoadTXTIntoDataframe(file.path(tempdir(),'DE-Tha-Results_ydh.txt'))
# write_xlsx(CombinedData,"clstwr_combineddata.xlsx")

## plotting the results and comparing other sources.

# bring in the data Shannon separated for CO2 for 2018-2021

new_co2 <- read.csv("data/measured_co2/co2_jevans_folder.csv", header = FALSE)
colnames(new_co2) <- new_co2[2, ]
new_co2 <- new_co2[-c(1:3),]
new_co2$nee <- as.numeric(new_co2$NEE)
new_co2$gpp <- as.numeric(new_co2$GPP)
new_co2$resp <- as.numeric(new_co2$Re)
new_co2$year <- as.numeric(new_co2$year)
new_co2$source <- "co2_jevans_other"
new_co2 <- new_co2 %>%
  mutate(doy = as.numeric(doy))

filled <- CombinedData %>% 
  group_by(doy, Year) %>% 
  summarise(
    nee = mean(NEE_U50_f),
    resp = mean(Reco_U50),
    gpp = mean(GPP_U50_f)
  ) %>% 
  mutate(year = Year)

combined <- new_co2 %>% 
  filter(plot == 1) %>% 
  left_join(filled, by = c("year", "doy"))

combined %>%
  ggplot(aes(x = doy, y = nee))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = nee_obs), color = "#0072B2", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 4)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2018:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 



CombinedData %>% 
  group_by(doy) %>% 
  summarise(
    nee = mean(NEE_U50_f)
  ) %>% 
  ggplot(aes(x = doy, y = nee))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(data = filter(new_co2, plot == 1 & year == 2019), aes(x = doy, y = nee ), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  theme_bw()

CombinedData %>% 
  group_by(doy) %>% 
  summarise(
    resp = mean(Reco_U50)
  ) %>% 
  ggplot(aes(x = doy, y = resp))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(data = filter(new_co2, plot == 1 & year == 2019), aes(x = doy, y = resp ), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  theme_bw()

CombinedData %>%
  group_by(doy) %>% 
  summarise(
    gpp = mean(GPP_U50_f)
  ) %>% 
  ggplot(aes(x = doy, y = gpp*(-1)))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(data = filter(new_co2, plot == 1 & year == 2019), aes(x = doy, y = gpp ), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  theme_bw()


########################################################################

########################################################################
###### option 3: data gapfilling not using ustar_threshold #####

EddyData <- fLoadTXTIntoDataframe("C:\\Users\\aolivo\\OneDrive - University of Guelph\\Desktop\\P12_tower_2022_R.txt")
EddyData <- filterLongRuns(EddyData, "NEE")

#+++ Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")

EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg'))
EProc$sSetLocationInfo(Lat_deg.n=51.0, Long_deg.n=13.6, TimeZone_h.n=1)  #Location of DE-Tharandt

#fingerprint
EProc$sPlotFingerprintY('NEE', Year = 2022, valueLimits = c(-200,200))
EProc$sPlotFingerprintY('LE', Year = 2022, valueLimits = c(-100,900))
EProc$sPlotFingerprintY('H', Year = 2022, valueLimits = c(-200,200))
EProc$sPlotHHFluxesY('NEE', Year = 2022)

# data gap filling
EProc$sMDSGapFill('NEE', FillAll.b=TRUE) 
EProc$sMDSGapFill('Rg', FillAll.b=FALSE)

#Partition NEE into GPP and respiration
EProc$sMDSGapFill('Tair', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
EProc$sMDSGapFill('VPD', FillAll.b=FALSE)  	# Gap-filled Tair (and NEE) needed for partitioning 
EProc$sMRFluxPartition()	# night time partitioning -> Reco, GPP
EProc$sGLFluxPartition()	# day time partitioning -> Reco_DT, GPP_DT

#+++ Example plots of calculated GPP and respiration 
EProc$sPlotFingerprintY('GPP_f', Year.i=2022)
EProc$sPlotHHFluxesY('Reco', Year.i=2022)

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
# fWriteDataframeToFile(CombinedData, 'DE-Tha-Results.txt', Dir = tempdir())
# # or without relying on data.frame EddyData
# # with replacing column DateTime by Year, DoY, and Hour:
# fWriteDataframeToFile(
#   cbind(EProc$sExportData(), EProc$sExportResults()), 'DE-Tha-Results_ydh.txt', 
#   isSplitDatetime=TRUE, Dir = tempdir())
# # tmp <- fLoadTXTIntoDataframe(file.path(tempdir(),'DE-Tha-Results_ydh.txt'))
# 
# write_xlsx(CombinedData,"clstwr_combineddata.xlsx")


CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    nee = mean(NEE_f)
  ) %>% 
  ggplot(aes(x = DoY, y = nee))+
  geom_line()+
  theme_bw()

CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    resp = mean(Reco)
  ) %>% 
  ggplot(aes(x = DoY, y = resp))+
  geom_line()+
  theme_bw()

CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    nee = mean(Reco_DT)
  ) %>% 
  ggplot(aes(x = DoY, y = nee))+
  geom_line()+
  theme_bw()

CombinedData %>% 
  group_by(DoY) %>% 
  summarise(
    gpp = mean(GPP_f)
  ) %>% 
  ggplot(aes(x = DoY, y = gpp*(-1)))+
  geom_line()+
  theme_bw()

########################################################################
