# readyproc data processing
# 03/18/2025
# agustin olivo
# aolivo@uoguelph.ca

#######################################################################
####################### general information ###################################

# install packages
install.packages("REddyProc")
help('REddyProc-package')
install.packages("psych")
install.packages("openair")
install.packages("lognorm") # for uncertainty analysis

# load libraries
library(REddyProc)
library(dplyr)
library(writexl)
library(ggplot2)
library(psych)
library(openair)
library(tidyverse)
library(lognorm) # for uncertainty analysis

# setting working directory
getwd()
setwd("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis")

# key sources of information:

# source 1: https://rdrr.io/rforge/REddyProc/man/sEddyProc.example.html
# source 2: https://github.com/EarthyScience/REddyProc/blob/master/vignettes/useCase.md
# source 3 (initial REddyProc paper): https://bg.copernicus.org/articles/15/5015/2023/bg-15-5015-2023.html
# source 4 (definition of different friction velocities across the year - u*): https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html
# source 5: https://rstudio-pubs-static.s3.amazonaws.com/84133_4c4347b1ba5e4067980787a85e27d68f.html
# source 6 (video): https://www.youtube.com/watch?v=-b0vc4u8kls
# source 7: uncertainty aggregation, https://cran.r-project.org/web/packages/REddyProc/vignettes/aggUncertainty.html

# key papers/documents:

# source 7: Wutzler, T., Lucas-Moffat, A., Migliavacca, M., Knauer, J., Sickel, K., Šigut, L., Menzer, O., and Reichstein, M.: Basic and extensible post-processing of eddy covariance flux data with REddyProc, Biogeosciences, 15, 5015–5030, https://doi.org/10.5194/bg-15-5015-2018, 2018.
# source 8: reference manual in https://cran.r-project.org/web/packages/REddyProc/index.html

# general info

# before starting, please clean everything from your global environment.Proceed with step 1, one plot at a time. Then with step 2. Once you finish estimating everything for one plot, clean everything in your environment again, and go back to Step 1 for the new plot.


#######################################################################

#######################################################################
####################### STEP 1: load data for each plot ###############

### plot 1-2 

# The workflow starts with importing the half-hourly data. 

# older data I was using

# EddyData_2018_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2018_R.txt")
# EddyData_2018_p3 <- EddyData_2018_p3 %>%
#   filter(DoY < 195) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1
# EddyData_2018_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2018_R.txt")
# EddyData_2018_p1 <- EddyData_2018_p1 %>%
#   filter(DoY > 194)
# EddyData_2019_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2019_R.txt")
# EddyData_2020_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2020_R.txt")
# EddyData_2021_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2021_R.txt")
# EddyData_2022_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2022_R.txt")
# EddyData_2023_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2023_R.txt")
# EddyData_2024_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2024_R.txt")

# new data
EddyData_2018_p3 <- fLoadTXTIntoDataframe("ec_18_p3")
EddyData_2018_p3 <- EddyData_2018_p3 %>%
   filter(DoY < 194) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1

EddyData_2018_p1 <- fLoadTXTIntoDataframe("ec_18_p12") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2018_p1 <- EddyData_2018_p1 %>%
     filter(DoY > 193)
EddyData_2019_p1 <- fLoadTXTIntoDataframe("ec_19_p12")
EddyData_2020_p1 <- fLoadTXTIntoDataframe("ec_20_p12")
EddyData_2021_p1 <- fLoadTXTIntoDataframe("ec_21_p12")
EddyData_2022_p1 <- fLoadTXTIntoDataframe("ec_22_p12")
EddyData_2023_p1 <- fLoadTXTIntoDataframe("ec_23_p12")
EddyData_2024_p1 <- fLoadTXTIntoDataframe("ec_24_p12")

EddyData_p1 <- rbind(EddyData_2018_p3, EddyData_2018_p1, EddyData_2019_p1, EddyData_2020_p1, EddyData_2021_p1, EddyData_2022_p1, EddyData_2023_p1,EddyData_2024_p1) # merging all years in a single dataset

# co2 fluxes are in umolm-2s-1

# definition of u* seasons for each plot (periods of time for which u* will be calculated separately); this will then be used for u* filtering of the data 

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
  c(365,2023),
  c(1,2024),
  c(143,2024), # planting
  c(307,2024), # harvest: this harvest date is from 2021; it has to be replaced with the harvest date for 2024
  c(365,2024)
  
)))

### plot 3-4 

# old data I was using

# EddyData_2018_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2018_R.txt")
# EddyData_2019_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2019_R.txt")
# EddyData_2020_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2020_R.txt")
# EddyData_2021_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2021_R.txt")
# EddyData_2022_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2022_R.txt")
# EddyData_2023_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2023_R.txt")
# EddyData_2024_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2024_R.txt")

# new data

EddyData_2018_p3 <- fLoadTXTIntoDataframe("ec_18_p3") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2019_p3 <- fLoadTXTIntoDataframe("ec_19_p3")
EddyData_2020_p3 <- fLoadTXTIntoDataframe("ec_20_p3")
EddyData_2021_p3 <- fLoadTXTIntoDataframe("ec_21_p3")
EddyData_2022_p3 <- fLoadTXTIntoDataframe("ec_22_p3")
EddyData_2023_p3 <- fLoadTXTIntoDataframe("ec_23_p3")
EddyData_2024_p3 <- fLoadTXTIntoDataframe("ec_24_p3")

EddyData_p3 <- rbind(EddyData_2018_p3, EddyData_2019_p3, EddyData_2020_p3, EddyData_2021_p3, EddyData_2022_p3, EddyData_2023_p3,EddyData_2024_p3) # merging all years in a single dataset
# co2 fluxes are in umolm-2s-1

# definition of u* seasons for each plot (periods of time for which u* will be calculated separately); this will then be used for u* filtering of the data 

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
  c(213,2020), # harvest ww
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
  c(221,2023), # harvest ww
  c(1,2024),
  c(143,2024), # planting
  c(307,2024), # harvest: this harvest date is from 2021; it has to be replaced with the harvest date for 2024
  c(365,2024)
)))

#######################################################################

#######################################################################
####################### STEP 2: main processing ###################################

# there are a few different options on how to conduct this analysis; the one below considers generating different ustar seasons, different ustar thresholds for them, and also estimating the probability associated with those; in another files that Shannon had compiled, a slightly different approach is used where the seasons are generated, but the probability associated wit the ustar tresholds are not estimated.

# key source of information: https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html

#### filter the data ####

# filtering out values outside of the range (mostly based on source 5 listed above and conversations with Patrick)

EddyData <- mutate(EddyData_p1, # this needs to be changed based on plot
          NEE_raw = NEE,
          NEE = ifelse(NEE < -100, NA, NEE), # added based on discussion with Patrick and Claudia.
          NEE = ifelse(NEE > 60, NA, NEE), # added based on discussion with Patrick and Claudia.
          VPD = ifelse(VPD > 50, NA, VPD), # this was in the original code
          VPD = ifelse(VPD < 0, NA, VPD), # this was not in the original code from source 5, but REddyProc gave me a warning so I added it.
          Rg = pmin(Rg, 1200),
          Rg = pmax(Rg, 0),
          RH = ifelse(abs(RH) > 100, NA, RH), # I added this after realizing there were a few observations with values among 3000s and 6000s
          H = ifelse(H < -300, NA, H), # I noticed in 2024 there were a few H and LE values, so looking at the rest of the distribution I set these thresholds
          LE = ifelse(LE < -500, NA, LE), # I noticed in 2024 there were a few H and LE values, so looking at the rest of the distribution I set these thresholds
        )

# Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")

# Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")

# initialize R5 reference class sEddyProc for post-processing of eddy data - change the name of the plot after "("
EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg')) # this command creates an object of class "sEddyProc" with the data from EddyDataWithPosix, and verifies if there are any values outside of pre-defined ranges (via warning message; for NEE it uses values that are less than -50, and for VPD negative values); it is important to pay attention to these warnings, as they may signal issues with units, or the order of the observations in the dataset. 
# for nee it may show warnings for values with absolute number larger than 50, but we set it at 100 so we do not need to worry about that warning specifically.

#### additional data quality check steps ####

# check structure of the data
str(EProc$sDATA)

# statistical summary of the data
round(describe(EProc$sDATA[,-1]), 2)

# plots across multiple years and variables to check the units are consistent
EddyData_initial_explo <- EddyData %>% # converting data to long format (needed for plotting)
  pivot_longer(
    cols = c(LE, H, Ustar,Tair, RH, VPD, Rg, NEE),
    names_to = "variables",
    values_to = "values"
  )
summary_stats <- EddyData_initial_explo %>%
  group_by(variables, Year) %>%
  summarize(
    mean_val = mean(values, na.rm = TRUE),
    sd_val = sd(values, na.rm = TRUE),
    .groups = "drop"
  )

mgmt_dates_p1  <- seasonStarts # bringing in the management dates to plot them as vertical lines in the graph
colnames(mgmt_dates_p1) <- c("doy", "Year")

EddyData_initial_explo %>% # actual plot using ggplot
  ggplot(aes(x = DoY, y = values, colour = variables)) +
  geom_point(alpha = 0.5, size = 1) +
  facet_grid(vars(variables), vars(Year), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_vline(data = mgmt_dates_p1, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  geom_text(data = summary_stats,
            aes(x = Inf, y = Inf,
                label = paste0("Mean: ", round(mean_val, 1), "\nSD: ", round(sd_val, 1))),
            hjust = 1.1, vjust = 1.0,
            inherit.aes = FALSE,
            size = 3)+
  labs(
    title = "REddyProc input data E26 - Plot1 (Half-hourly data)",
    subtitle = "Units: NEE (umolm-2s-1), LE (Wm-2), H (Wm-2), Ustar (ms-1), Tair (degC), RH (%), VPD (hPa), Rg (Wm-2)"
)

ggsave("basic_p1.png", plot = last_plot(), width = 11, height = 7.5) # saving the figure as png to the working directory


# plot for nee only (halfhourly)

# EddyData_initial_explo_nee_old <- EddyData_old %>% 
#   pivot_longer(
#     cols = c(NEE), #NEE_raw could be added
#     names_to = "variables",
#     values_to = "values"
#   )

EddyData_initial_explo_nee <-EddyData %>% 
  pivot_longer(
    cols = c(NEE), #NEE_raw could be added
    names_to = "variables",
    values_to = "values"
  )

EddyData_initial_explo_nee %>% 
  ggplot(aes(x = DoY, y = values, fill = variables))+
  #geom_point(data = EddyData_initial_explo_nee_old ,aes(x = DoY, y = values ), color = "#D55E00", alpha = 0.3, size = 0.75)+
  geom_point(alpha = 0.3, color = "#009E73",  size = 1)+
  facet_grid(vars(variables), vars(Year), scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )+
  geom_vline(data = mgmt_dates_p1, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  labs(title = "REddyProc input data E26 - Plot1 -  NEE (Half-hourly)",
       subtitle = "Red lines represent beginning and end of year, plus planting and harvest")+
  ylab("NEE (umolm-2s-1)")

ggsave("basic_nee_halfhourly_p1.png", plot = last_plot(), width = 11, height = 3.5) # saving the figure as png to the working directory

# plot for nee only (daily)

EddyData_initial_explo_nee_daily <- EddyData %>%
  group_by(DoY, Year) %>% 
  summarise(
    NEE = mean(NEE, na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = c(NEE), #NEE_raw could be added
    names_to = "variables",
    values_to = "values"
  )

EddyData_initial_explo_nee_daily %>% 
  ggplot(aes(x = DoY, y = values))+
  geom_point(alpha = 0.5,color = "#0072B2",  size = 1.5)+
  facet_grid(vars(variables), vars(Year), scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )+
  geom_vline(data = mgmt_dates_p1, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  labs(title = "REddyProc input data E26 - Plot1 -  NEE (daily)",
       subtitle = "Red lines represent beginning and end of year, plus planting and harvest")+
  ylab("NEE (umolm-2s-1)")

ggsave("basic_nee_daily_p1.png", plot = last_plot(), width = 11, height = 3.5) # saving the figure as png to the working directory


# fingerprint plot: a fingerprint-plot is a color-coded image of the half-hourly fluxes by daytime on the x and and day of the year on the y axis.This may show several gaps

#plots for specific years

EProc$sPlotFingerprintY('NEE', Year = 2019, valueLimits = c(-200,200))
EProc$sPlotFingerprintY('LE', Year = 2019, valueLimits = c(-100,900))
EProc$sPlotFingerprintY('H', Year = 2019, valueLimits = c(-200,200))
EProc$sPlotHHFluxesY('NEE', Year = 2019)

#### gap-filling the data ####

# seasons were defined for each plot in previous steps

seasonFactor <- usCreateSeasonFactorYdayYear(
  EddyDataWithPosix$DateTime - 15*60, starts = seasonStarts) # in Daphnee's code there is no "-15*60; not sure what this is exactly
seasonStartsDate <- fConvertTimeToPosix( data.frame(Year = seasonStarts[,2]
                                                    , DoY = seasonStarts[,1], Hour = 0.50), 'YDH'
                                         , Year = "Year", Day = "DoY", Hour = "Hour")
# plotting NEE values
plot(NEE ~ DateTime, EddyDataWithPosix)
# plotting the lines for each season (seasonStarts_p1)
abline(v = seasonStartsDate$DateTime) 

# data gap-filling 

# estimating the ustar_threshold distribution

# the estimation of the distribution of uStar thresholds follows, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.

EProc$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 100L, probs = c(0.05,0.50,0.95)) # i kept these scenarios only for 0.5 to make it run quicker, but i should use: c(0.05, 0.50, 0.95)
EProc$sGetEstimatedUstarThresholdDistribution()

#The output reports annually aggregated uStar estimates for the original data and lower, median, and upper quantile of the estimated distribution. The threshold can vary between periods of different surface roughness, or pre-defined seasons. Therefore, there are estimates for different time periods, called seasons (those were defined in previous step)

# The subsequent post processing steps will be repeated using the four $u_$ threshold scenarios (non-resampled and tree quantiles of the bootstrapped distribution). They require to specify a $u_$-threshold for each season and a suffix to distinguish the outputs related to different thresholds. By default the annually aggregated estimates are used for each season within the year.

EProc$useSeaonsalUStarThresholds()
EProc$sGetUstarScenarios()

# actual gap-filling

EProc$sMDSGapFillUStarScens('NEE', FillAll = TRUE)

# For each of the different u∗ threshold estimates, a separate set of output columns of filled NEE and its uncertainty is generated, distinguished by the suffixes given with uStarSuffixes. Suffix “_f” denotes the filled value and “_fsd” the estimated standard deviation of its uncertainty.

# With option FillAll = TRUE, an uncertainty, specifically the standard deviation, of the flux is estimated for each record during gapfilling and stored in variable NEE_uStar_fsd.

# other meteorological variables need to be gap-filled for partitioning of NEE between Re and GPP

EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sMDSGapFill('Rg', FillAll=FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

grep("^NEE.*_f$", colnames( EProc$sExportResults()), value = TRUE) # extract name of the variable that will be used
EProc$sPlotFingerprintY('NEE_uStar_f', Year = 2021) # plotting the gap-filled data for a specific year

# checking values that have Ustar lower than thresholds, but are not excluded for some reason; after looking at these graphs and discussing with Patrick, seems like most of the points that have low U* but are not excluded seem to be in line with the rest of the values. So the program just keeps them. There were just a few values that looked far off and might need to be manually deleted. 

#### partitioning the NEE data into gpp and respiration ####

# there are two methods for partitioning the data, day-time method and night-time method; per previous conversations with Patrick, I am applying the day-time method.

EProc$sSetLocationInfo(LatDeg = 43.64079, LongDeg = -80.41301, TimeZoneHour = -5)  # setting the location for the site (this is E26)

# night-time method

EProc$sMRFluxPartitionUStarScens()
EProc$sApplyUStarScen(EProc$sMRFluxPartition)
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE) # extract the name of the variables that will be used

# daytime method

EProc$sGLFluxPartitionUStarScens()
EProc$sApplyUStarScen(EProc$sGLFluxPartition)
grep("GPP_DT_.*_f$|Reco", names(EProc$sExportResults()), value = TRUE)

# exporting the data

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData) # combining this files helps bring back the doy 
CombinedData$doy <- CombinedData$DoY

CombinedData_p34
CombinedData_p12  

# important variables in the CombineData dataset

# NEE = original NEE value (raw halfhourly data)
# Ustar = original Ustar value
# Ustar_U50_Tresh = median for the distribution of Ustar tresholds used for each season
# NEE_U50_orig = NEE value after ustar filtering considering the median for the distribution of Ustar tresholds used for each season
# NEE_U50_f = filled NEE value after ustar filtering considering the median for the distribution of Ustar tresholds used for each season

# after partitioning

# GPP_U50_f = gpp value obtained with night-time partitioning, considering the median of the distribution of u* threshold
# Reco_U50 = reco value obtained with night-time partitioning, considering the median of the distribution of u* threshold

# final values to use

# NEE_U50_f = NEE value after filtering and gap-filling, considering the median of the distribution of u* threshold for different seasons
# GPP_DT_U50 = GPP value obtained with day-time partitioning, considering the median of the distribution of u* threshold
# Reco_DT_U50 = Reco value obtained with day-time partitioning, considering the median of the distribution of u* threshold

# other information/variable names

# VAR_orig - Original values used for gap filling 
# VAR_f - Original values and gaps filled with mean of selected datapoints (condition depending on gap filling method) 
# VAR_fqc - Quality flag assigned depending on gap filling method and window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) 
# VAR_fall - All values considered as gaps (for uncertainty estimates) 
# VAR_fall_qc - Quality flag assigned depending on gap filling method and window length (1 = most reliable, 2 = medium, 3 = least reliable)
# VAR_fnum - Number of datapoints used for gap-filling 
# VAR_fsd - Standard deviation of datapoints used for gap filling (uncertainty) 
# VAR_fmeth - Method used for gap filling (1 = similar meteo condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) 
# VAR_fwin - Full window length used for gap filling


#######################################################################

#######################################################################
######################## estimating uncertainty #######################

# general info

# see question "How can I compute uncertainty of daily or annual aggregates? (#uncertaintyAggregation)" in https://www.bgc-jena.mpg.de/5629512/FAQ

####### source 1 ########

# The results of the different u∗ threshold scenarios can be used for estimating the uncertainty due to not knowing the threshold.

FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25
print(GPPAgg)

# The difference between those aggregated values is a first estimate of uncertainty range in GPP due to uncertainty of the u∗ threshold.

(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg) 


####### source 2 ############

# this will probably be the main strategy used for uncertainty estimation

# everything below is mostly built based on this source: https://cran.r-project.org/web/packages/REddyProc/vignettes/aggUncertainty.html

# Sara Knox also put together an improved and more complete version of the code above, here: https://github.com/CANFLUX/Biomet.net/blob/main/R/uncertainty/flux_uncertainty_function.R (I also copied the function below)

# what you will find below is a combination of those two sources; I think it would be ideal to make Sara Knox's code work, but I could not understand very well which the inputs of the function are, so it was easier to take some stuff from her code, and complement what I had from the other source; apart from that, Sara Knox's code did not include the code for the daily estimation; 

# the code below includes estimations for three types of uncertainty: 1) random error (uncertainty associated with the measurements), 2) systematic error (uncertainty associated with estimations of u* tresholds), and 3) the aggregated uncertainty (combinations of the previous two)

# for annual aggregated NEE, the three uncertainties can be estimated. For annual aggregated GPP and Reco, only option 2 can be estimated by the code below (Sara Knox's code did not include how to estimate options 1 and 3 for annual aggregated GPP and Reco; I actually do not know if it is not possible to do it, or if the code they have up in GitHub just does not have it). For daily NEE, the code below estimates option 1 if I understood things correctly. 

year_of_interest <- 2020 # added by agustin

# REddyProc flags filled data with poor gap-filling by a quality flag in NEE_<uStar>_fqc > 0 but still reports the fluxes. For aggregation we recommend computing the mean including those gap-filled records, i.e. using NEE_<uStar>_f instead of NEE_orig. However, for estimating the uncertainty of the aggregated value, the the gap-filled records should not contribute to the reduction of uncertainty due to more replicates.

# Hence, first we create a column similar NEE_orig_sd to NEE_<uStar>_fsd but where the estimated uncertainty is set to missing for the gap-filled records.

results <- EProc$sExportResults() %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    , DoY = as.POSIXlt(DateTime - 15*60)$yday # midnight belongs to the previous
  ) %>% 
  filter(year(DateTime) == year_of_interest) %>%  
  mutate(
    NEE_orig_sd = ifelse(
      is.finite(NEE_uStar_orig), NEE_uStar_fsd, NA), # NEE_orig_sd includes NEE_uStar_fsd only for measured values
    NEE_uStar_fgood = ifelse(
      NEE_uStar_fqc <= 1, is.finite(NEE_uStar_f), NA), # Only include filled values for the most reliable gap-filled observations. Note that is.finite() shouldn't be used here.
    resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA) # quantify the error terms, i.e. data-model residuals (only using observations (i.e., NEE_uStar_fqc == 0 is original data) and exclude also
    
    
    # NEE_orig_sd = ifelse(
    #   is.finite(.data$NEE_uStar_orig), .data$NEE_uStar_fsd, NA),
    # NEE_uStar_fgood = ifelse(
    #   is.finite(.data$NEE_uStar_fqc) & (.data$NEE_uStar_fqc <= 1), .data$NEE_uStar_f, NA),
    # resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA)
  )

# If the aggregated mean should be computed excluding poor quality-gap-filled data, then its best to use a column with values set to missing for poor quality, e.g. using NEE_<uStar>_fgood instead of NEE_<uStar>_f. However, the bias in aggregated results can be larger when omitting records, e.g. consistently omitting more low night-time fluxes, than with using poor estimates of those fluxes.

# random error

# For a given u* threshold, the aggregation across time uses many records. The random error in each record, i.e. NEE_fsd, is only partially correlated to the random error to records close by. Hence, the relative uncertainty of the aggregated value decreases compared to the average relative uncertainty of the individual observations.

# Considering correlations

# When observations are not independent of each other, the formulas are now different

autoCorr <- lognorm::computeEffectiveAutoCorr(results$resid)
nEff <- lognorm::computeEffectiveNumObs(results$resid, na.rm = TRUE)
c(nEff = nEff, nObs = sum(is.finite(results$resid)))

resRand <- results %>% summarise(
  nRec = sum(is.finite(NEE_orig_sd))
  , NEEagg = mean(NEE_uStar_f, na.rm = TRUE)
  , varMean = sum(NEE_orig_sd^2, na.rm = TRUE) / nRec / (!!nEff - 1)
  , seMean = sqrt(varMean) 
  #, seMean2 = sqrt(mean(NEE_orig_sd^2, na.rm = TRUE)) / sqrt(!!nEff - 1)
  , seMeanApprox = mean(NEE_orig_sd, na.rm = TRUE) / sqrt(!!nEff - 1)
) %>% select(NEEagg, seMean, seMeanApprox)
resRand

# The aggregated value is the same as when not considering the correlations, but its uncertainty increased compared to the computation neglecting correlations.
# Note, how we used NEE_uStar_f for computing the mean, but NEE_orig_sd instead of NEE_uStar_fsd for computing the uncertainty.

# daily aggregation (this is not presented in Sara Knox's code)

# When aggregating daily respiration, the same principles hold.
# However, when computing the number of effective observations, we recommend using the empirical autocorrelation function estimated on longer time series of residuals (autoCorr computed above) in computeEffectiveNumObs instead of estimating them from the residuals of each day.

# First, create a column DoY to subset records of each day.

# already done

# Now the aggregation can be done on data grouped by DoY. The notation !! tells summarise to use the variable autoCorr instead of a column with that name.

aggDay <- results %>% 
  group_by(DoY) %>% 
  summarise(
    DateTime = first(DateTime),
    nEff = lognorm::computeEffectiveNumObs(
      resid, effAcf = !!autoCorr, na.rm = TRUE),
    nRec = sum(is.finite(NEE_orig_sd)),
    NEE = mean(NEE_uStar_f, na.rm = TRUE),
    sdNEE = if (nEff <= 1) NA_real_ else sqrt(
      mean(NEE_orig_sd^2, na.rm = TRUE) / (nEff - 1)), 
    sdNEEuncorr = if (nRec <= 1) NA_real_ else sqrt(
      mean(NEE_orig_sd^2, na.rm = TRUE) / (nRec - 1)),
    .groups = "drop_last"
  )
aggDay

aggDay %>% 
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = NEE - sdNEE, ymax = NEE + sdNEE), fill = "skyblue", alpha = 0.75) +
  geom_line(aes(y = NEE), color = "blue", size = 1) +
  labs(
    title = "Daily NEE with Uncertainty",
    x = "Date",
    y = expression(NEE~(µmol~CO[2]~m^{-2}~s^{-1}))
  ) +
  theme_bw()

# u* threshold uncertainty

# There is also flux uncertainty due to uncertainty in u* threshold estimation. Since the same threshold is used for all times in a given uStar scenario, the relative uncertainty of this component does not decrease when aggregating across time.
# 
# The strategy is to
# 
# 1. estimate distribution of u* threshold
# 
# 2. compute time series of NEE (or other values of interest) for draws from this distribution, i.e. for many uStar-scenarios
# 
# 3. compute each associated aggregated value
# 
# 4. and then look at the distribution of the aggregated values.
# 
# Note that the entire processing down to the aggregated value has to be repeated for each uStar scenario. Hence, obtaining a good estimate of this uncertainty is computationally expensive.

# 1. First, we estimate many samples of the probability density of the unknown uStar threshold.

# done in previous steps of the code

# 2. Produce time series of gapfilled NEE for each scenario. They are stored in columns distinguished by a suffix with the quantile.

# done in previous steps of the code

# 3. Compute the annual mean for each scenario. Method sEddyProc_sApplyUStarScen calls a user-provided function that takes an argument suffix for each u*-threshold scenario. Here, we use it to create the corresponding NEE column name and compute mean across this column in the data exported from REddyProc.

computeMeanNEE <- function(ds, suffix){
  column_name <- paste0("NEE_",suffix,"_f")
  mean(ds[[column_name]], na.rm = TRUE) # agustin added "na.rm = TRUE" here; it was not working otherwise
}

FilledEddyData <- EProc$sExportResults() %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    , DoY = as.POSIXlt(DateTime - 15*60)$yday # midnight belongs to the previous
  ) %>% 
  filter(year(DateTime) == year_of_interest) 
  
NEEagg <- unlist(EProc$sApplyUStarScen(computeMeanNEE, FilledEddyData))
NEEagg

# 4. compute uncertainty across aggregated values

sdNEEagg_ustar <- sd(NEEagg)
sdNEEagg_ustar

# Combined aggregated uncertainty

# Assuming that the uncertainty due to unknown u*threshold is independent from the random uncertainty, the variances add.

sdAnnual <- data.frame(
  sdRand = resRand$seMean,
  sdUstar = sdNEEagg_ustar,
  sdComb = sqrt(resRand$seMean^2 + sdNEEagg_ustar^2) 
)
sdAnnual

data.mean_NEE_uStar_f <- data.frame(mean(results$NEE_uStar_f, na.rm = TRUE))
colnames(data.mean_NEE_uStar_f) <- 'mean_NEE_uStar_f'
NEE_sdAnnual <- cbind(data.mean_NEE_uStar_f,sdAnnual)

# from sara knox's code

# GPP uncertainty (only u* for now) 

# Nighttime
ind <- which(grepl("GPP_U*", names(results)) & grepl("_f$", names(results)))
column_name <- names(results)[ind] 

#calculate column means of specific columns
GPPagg_NT <- colMeans(results[ ,column_name], na.rm=T)

#compute uncertainty across aggregated values
sd_GPP_Ustar_NT<- sd(GPPagg_NT)
sd_GPP_Ustar_NT <- data.frame(sd_GPP_Ustar_NT)

# Daytime
ind <- which(grepl("GPP_DT*", names(results)) & !grepl("_SD$", names(results)))
column_name <- names(results)[ind] 

#calculate column means of specific columns
GPPagg_DT <- colMeans(results[ ,column_name], na.rm=T)

#compute uncertainty across aggregated values
sd_GPP_Ustar_DT<- sd(GPPagg_DT)
sd_GPP_Ustar_DT <- data.frame(sd_GPP_Ustar_DT)

# Reco uncertainty (only u* for now)

# Nighttime

# Rename column names to compute uncertainty
col_indx <- grep(pattern = '^Reco_U.*', names(results))
for (i in 1:length(col_indx)) {
  colnames(results)[col_indx[i]] <-
    paste(colnames(results)[col_indx[i]], "_f", sep = "")
}

ind <- which(grepl("Reco_U*", names(results)) & grepl("_f$", names(results)))
column_name <- names(results)[ind] 

#calculate column means of specific columns
Recoagg_NT <- colMeans(results[ ,column_name], na.rm=T)

#compute uncertainty across aggregated values
sd_Reco_Ustar_NT <- sd(Recoagg_NT)
sd_Reco_Ustar_NT <- data.frame(sd_Reco_Ustar_NT)

# Daytime
ind <- which(grepl("Reco_DT*", names(results)) & !grepl("_SD$", names(results)))
column_name <- names(results)[ind] 

#calculate column means of specific columns
Recoagg_DT <- colMeans(results[ ,column_name], na.rm=T)

#compute uncertainty across aggregated values
sd_Reco_Ustar_DT<- sd(Recoagg_DT)
sd_Reco_Ustar_DT <- data.frame(sd_Reco_Ustar_DT)

# Create output data frame
mean_sdAnnual <- NEE_sdAnnual %>%
  mutate(mean_GPP_uStar_f = mean(results$GPP_uStar_f, na.rm = TRUE),
         sd_GPP_Ustar_NT = sd_GPP_Ustar_NT,
         mean_Reco_uStar_f = mean(results$Reco_uStar, na.rm = TRUE),
         sd_Reco_Ustar_NT = sd_Reco_Ustar_NT,
         mean_GPP_DT_uStar = mean(results$GPP_DT_uStar, na.rm = TRUE),
         sd_GPP_Ustar_DT = sd_GPP_Ustar_DT,
         mean_Reco_DT_uStar = mean(results$Reco_DT_uStar, na.rm = TRUE),
         sd_Reco_Ustar_DT = sd_Reco_Ustar_DT)
mean_sdAnnual

# other part

data.mean_NEE_uStar_f <- data.frame(mean(results$NEE_uStar_f, na.rm = TRUE))
colnames(data.mean_NEE_uStar_f) <- 'mean_NEE_uStar_f'
NEE_sdAnnual <- cbind(data.mean_NEE_uStar_f,NEE_sdAnnual)

# Convert to annual sums
conv_gCO2 <- 1/(10^6)*44.01*60*60*24*length(results$NEE_uStar_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year # note from Agustin, I think this is because the original unit is in umolm-2s-1
conv_gC <- 1/(10^6)*12.011*60*60*24*length(results$NEE_uStar_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year

# g CO2
mean_sdAnnual_gCO2_all <- data.frame()
mean_sdAnnual_gC_all <- data.frame()

mean_sdAnnual_gCO2 <- mean_sdAnnual*conv_gCO2
mean_sdAnnual_gCO2

mean_sdAnnual_gCO2_all <- rbind(mean_sdAnnual_gCO2_all,mean_sdAnnual_gCO2)

# g C
mean_sdAnnual_gC <- mean_sdAnnual*conv_gC
mean_sdAnnual_gC # final units in g C/m2/year
mean_sdAnnual_gC_all <- rbind(mean_sdAnnual_gC_all,mean_sdAnnual_gC)


#######################################################################

#######################################################################
################### Sara Knox's uncertainty estimation function #######

# copied from github: https://github.com/CANFLUX/Biomet.net/blob/main/R/uncertainty/flux_uncertainty_function.R

# Written to calculate uncertainty for fluxes for annual sums
# Based on 'Aggregating uncertainty to daily and annual values' (see: https://github.com/bgctw/REddyProc/blob/master/vignettes/aggUncertainty.md)
# By Sara Knox
# Aug 26, 2022
# Modified July 11, 2023 to create function and loop over years

# Inputs
# site (all caps). e.g., "HOGG"
# ini_path = path to ini file. e.g., "/Users/sara/Code/Biomet.net/R/uncertainty/ini_files/"


# NOTES:
# 1) Could create as a function
# 2) Generalize to loop over years
# 3) Add daytime partitioning
# 4) Modify for CH4 and check variable names for random error for NEE

# Make sure to create ini file first

output <- function(site,ini_path) {
  
  # Load required libraries
  library("dplyr")
  library("lubridate")
  library("plotly")
  
  # Run ini file first 
  source(paste0(ini_path,site,"_annual_uncertainty_ini.R",sep = ""))
  
  # Read function for loading data
  p <- sapply(list.files(pattern="read_database_generalized.R", path=fx_path, full.names=TRUE), source)
  p <- sapply(list.files(pattern="RF_gf", path=fx_path, full.names=TRUE), source)
  
  # Create data frame for years & variables of interest to import into REddyProc
  df <- read_data_generalized(basepath,yrs,site,level_in,vars,tv_input,export)
  
  # Loop over years
  mean_sdAnnual_gCO2_all <- data.frame()
  mean_sdAnnual_gC_all <- data.frame()
  
  for (i in length(start_dates)) {
    start_ind <- which(df$datetime==start_dates[i])+1 #+1 added to start at 30 min 
    end_ind <- which(df$datetime==end_dates[i])
    data <- df[c(start_ind:end_ind), ]
    
    # NEE uncertainty
    
    # Random error
    # Considering correlations
    
    # REddyProc flags filled data with poor gap-filling by a quality flag in NEE_<uStar>_fqc > 0 but still reports the fluxes. 
    # For aggregation we recommend computing the mean including those gap-filled records, i.e. using NEE_<uStar>_f instead of NEE_orig. 
    # However, for estimating the uncertainty of the aggregated value, the the gap-filled records should not contribute to the reduction of uncertainty due to more replicates.
    # Hence, first we create a column 'NEE_orig_sd' similar to 'NEE_uStar_fsd' but where the estimated uncertainty is set to missing for the gap-filled records.
    data <- data %>% 
      mutate(
        NEE_orig_sd = ifelse(
          is.finite(NEE_uStar_orig), NEE_uStar_fsd, NA), # NEE_orig_sd includes NEE_uStar_fsd only for measured values
        NEE_uStar_fgood = ifelse(
          NEE_uStar_fqc <= 1, is.finite(NEE_uStar_f), NA), # Only include filled values for the most reliable gap-filled observations. Note that is.finite() shouldn't be used here.
        resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA)) # quantify the error terms, i.e. data-model residuals (only using observations (i.e., NEE_uStar_fqc == 0 is original data) and exclude also
    # "good" gap-filled data)
    # plot_ly(data = data, x = ~datetime, y = ~NEE_uStar_f, name = 'filled', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
    #   add_trace(data = data, x = ~datetime, y = ~NEE_uStar_orig, name = 'orig', mode = 'markers') %>% 
    #   toWebGL()
    
    # visualizing data
    plot_ly(data = data, x = ~datetime, y = ~NEE_U2.5_orig, name = 'U2.5', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_orig, name = 'uStar', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_U97.5_orig, name = 'U97.5', mode = 'markers') %>% 
      toWebGL()
    
    plot_ly(data = data, x = ~datetime, y = ~NEE_U2.5_fall, name = 'U2.5 fall', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
      #add_trace(data = data, x = ~datetime, y =~NEE_U2.5_fall, name = 'U2.5 fall', mode = 'markers') %>% 
      #add_trace(data = data, x = ~datetime, y =~NEE_uStar_f, name = 'uStar fill', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_fall, name = 'uStar fall', mode = 'markers') %>% 
      #add_trace(data = data, x = ~datetime, y =~NEE_U97.5_f, name = 'U97.5 fill', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_U97.5_fall, name = 'U97.5 fall', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_orig, name = 'uStar orig', mode = 'markers',marker = list(size = 5)) %>% 
      toWebGL()
    
    autoCorr <- lognorm::computeEffectiveAutoCorr(data$resid)
    nEff <- lognorm::computeEffectiveNumObs(data$resid, na.rm = TRUE)
    c(nEff = nEff, nObs = sum(is.finite(data$resid))) 
    
    # Note, how we used NEE_uStar_f for computing the mean, but NEE_orig_sd instead of NEE_uStar_fsd for computing the uncertainty.
    resRand <- data %>% summarise(
      nRec = sum(is.finite(NEE_orig_sd))
      , NEEagg = mean(NEE_uStar_f, na.rm = TRUE)
      , varMean = sum(NEE_orig_sd^2, na.rm = TRUE) / nRec / (!!nEff - 1)
      , sdMean = sqrt(varMean) 
      , sdMeanApprox = mean(NEE_orig_sd, na.rm = TRUE) / sqrt(!!nEff - 1)
    ) %>% dplyr::select(NEEagg, sdMean, sdMeanApprox)
    
    # can also compute Daily aggregation -> but not done here.
    
    # u* threshold uncertainty
    ind <- which(grepl("NEE_U*", names(data)) & grepl("_f$", names(data)))
    column_name <- names(data)[ind] 
    
    #calculate column means of specific columns
    NEEagg <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sdNEEagg_ustar <- sd(NEEagg)
    
    # Combined aggregated uncertainty
    
    #Assuming that the uncertainty due to unknown u*threshold is independent from the random uncertainty, the variances add.
    NEE_sdAnnual <- data.frame(
      sd_NEE_Rand = resRand$sdMean,
      sd_NEE_Ustar = sdNEEagg_ustar,
      sd_NEE_Comb = sqrt(resRand$sdMean^2 + sdNEEagg_ustar^2) 
    )
    
    data.mean_NEE_uStar_f <- data.frame(mean(data$NEE_uStar_f, na.rm = TRUE))
    colnames(data.mean_NEE_uStar_f) <- 'mean_NEE_uStar_f'
    NEE_sdAnnual <- cbind(data.mean_NEE_uStar_f,NEE_sdAnnual)
    
    # GPP uncertainty (only u* for now) 
    
    # Nighttime
    ind <- which(grepl("GPP_U*", names(data)) & grepl("_f$", names(data)))
    column_name <- names(data)[ind] 
    
    #calculate column means of specific columns
    GPPagg_NT <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sd_GPP_Ustar_NT<- sd(GPPagg_NT)
    sd_GPP_Ustar_NT <- data.frame(sd_GPP_Ustar_NT)
    
    # Daytime
    ind <- which(grepl("GPP_DT*", names(data)) & !grepl("_SD$", names(data)))
    column_name <- names(data)[ind] 
    
    #calculate column means of specific columns
    GPPagg_DT <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sd_GPP_Ustar_DT<- sd(GPPagg_DT)
    sd_GPP_Ustar_DT <- data.frame(sd_GPP_Ustar_DT)
    
    # Reco uncertainty (only u* for now)
    
    # Nighttime
    
    # Rename column names to compute uncertainty
    col_indx <- grep(pattern = '^Reco_U.*', names(data))
    for (i in 1:length(col_indx)) {
      colnames(data)[col_indx[i]] <-
        paste(colnames(data)[col_indx[i]], "_f", sep = "")
    }
    
    ind <- which(grepl("Reco_U*", names(data)) & grepl("_f$", names(data)))
    column_name <- names(data)[ind] 
    
    #calculate column means of specific columns
    Recoagg_NT <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sd_Reco_Ustar_NT <- sd(Recoagg_NT)
    sd_Reco_Ustar_NT <- data.frame(sd_Reco_Ustar_NT)
    
    # Daytime
    ind <- which(grepl("Reco_DT*", names(data)) & !grepl("_SD$", names(data)))
    column_name <- names(data)[ind] 
    
    #calculate column means of specific columns
    Recoagg_DT <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sd_Reco_Ustar_DT<- sd(Recoagg_DT)
    sd_Reco_Ustar_DT <- data.frame(sd_Reco_Ustar_DT)
    
    # Create output data frame
    mean_sdAnnual <- NEE_sdAnnual %>%
      mutate(mean_GPP_uStar_f = mean(data$GPP_uStar_f, na.rm = TRUE),
             sd_GPP_Ustar_NT = sd_GPP_Ustar_NT,
             mean_Reco_uStar_f = mean(data$Reco_uStar, na.rm = TRUE),
             sd_Reco_Ustar_NT = sd_Reco_Ustar_NT,
             mean_GPP_DT_uStar = mean(data$GPP_DT_uStar, na.rm = TRUE),
             sd_GPP_Ustar_DT = sd_GPP_Ustar_DT,
             mean_Reco_DT_uStar = mean(data$Reco_DT_uStar, na.rm = TRUE),
             sd_Reco_Ustar_DT = sd_Reco_Ustar_DT)
    mean_sdAnnual
    
    # Run USTAR & gap-filling uncertainty for FCH4
    
    # FCH4 uncertainty
    
    # Random error
    # Considering correlations
    
    # REddyProc flags filled data with poor gap-filling by a quality flag in NEE_<uStar>_fqc > 0 but still reports the fluxes. 
    # For aggregation we recommend computing the mean including those gap-filled records, i.e. using NEE_<uStar>_f instead of NEE_orig. 
    # However, for estimating the uncertainty of the aggregated value, the the gap-filled records should not contribute to the reduction of uncertainty due to more replicates.
    # Hence, first we create a column 'NEE_orig_sd' similar to 'NEE_uStar_fsd' but where the estimated uncertainty is set to missing for the gap-filled records.
    data <- data %>% 
      mutate(
        NEE_orig_sd = ifelse(
          is.finite(NEE_uStar_orig), NEE_uStar_fsd, NA), # NEE_orig_sd includes NEE_uStar_fsd only for measured values
        NEE_uStar_fgood = ifelse(
          NEE_uStar_fqc <= 1, is.finite(NEE_uStar_f), NA), # Only include filled values for the most reliable gap-filled observations. Note that is.finite() shouldn't be used here.
        resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA)) # quantify the error terms, i.e. data-model residuals (only using observations (i.e., NEE_uStar_fqc == 0 is original data) and exclude also
    # "good" gap-filled data)
    # plot_ly(data = data, x = ~datetime, y = ~NEE_uStar_f, name = 'filled', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
    #   add_trace(data = data, x = ~datetime, y = ~NEE_uStar_orig, name = 'orig', mode = 'markers') %>% 
    #   toWebGL()
    
    # visualizing data
    plot_ly(data = data, x = ~datetime, y = ~NEE_U2.5_orig, name = 'U2.5', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_orig, name = 'uStar', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_U97.5_orig, name = 'U97.5', mode = 'markers') %>% 
      toWebGL()
    
    plot_ly(data = data, x = ~datetime, y = ~NEE_U2.5_fall, name = 'U2.5 fall', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
      #add_trace(data = data, x = ~datetime, y =~NEE_U2.5_fall, name = 'U2.5 fall', mode = 'markers') %>% 
      #add_trace(data = data, x = ~datetime, y =~NEE_uStar_f, name = 'uStar fill', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_fall, name = 'uStar fall', mode = 'markers') %>% 
      #add_trace(data = data, x = ~datetime, y =~NEE_U97.5_f, name = 'U97.5 fill', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_U97.5_fall, name = 'U97.5 fall', mode = 'markers') %>% 
      add_trace(data = data, x = ~datetime, y =~NEE_uStar_orig, name = 'uStar orig', mode = 'markers',marker = list(size = 5)) %>% 
      toWebGL()
    
    autoCorr <- lognorm::computeEffectiveAutoCorr(data$resid)
    nEff <- lognorm::computeEffectiveNumObs(data$resid, na.rm = TRUE)
    c(nEff = nEff, nObs = sum(is.finite(data$resid))) 
    
    # Note, how we used NEE_uStar_f for computing the mean, but NEE_orig_sd instead of NEE_uStar_fsd for computing the uncertainty.
    resRand <- data %>% summarise(
      nRec = sum(is.finite(NEE_orig_sd))
      , NEEagg = mean(NEE_uStar_f, na.rm = TRUE)
      , varMean = sum(NEE_orig_sd^2, na.rm = TRUE) / nRec / (!!nEff - 1)
      , sdMean = sqrt(varMean) 
      , sdMeanApprox = mean(NEE_orig_sd, na.rm = TRUE) / sqrt(!!nEff - 1)
    ) %>% dplyr::select(NEEagg, sdMean, sdMeanApprox)
    
    # can also compute Daily aggregation -> but not done here.
    
    if (FCH4_uncertainty == 1) {
      
      # Load variables for gap-filling
      # Create data frame for years & variables of interest to import into REddyProc
      df_FCH4 <- read_data_generalized(basepath,yrs,site,level_RF_FCH4,predictors_FCH4,tv_input,export)
      
      df_FCH4 <- df_FCH4[c(start_ind:end_ind), ]
      
      ind <- which(grepl("FCH4_U*", names(data)) & grepl("_orig", names(data)))
      column_name <- names(data)[ind] 
      
      # Create empty dataframe 
      df_gap_filled_FCH4 <- data.frame(matrix(nrow = nrow(df_FCH4), ncol = length(column_name)))
      colnames(df_gap_filled_FCH4) <- column_name
      for (j in 1:length(ind)){ 
        
        # Create new data frame for each USTAR threshold + variables used for gap-filling
        data_RF <- cbind(df_FCH4[,1],data[,ind[j]],df_FCH4[,c(3:length(df_FCH4))])
        colnames(data_RF)[c(1,2)] <- c('datetime','FCH4')
        datetime <- df_FCH4$datetime
        
        gap_filled_FCH4 <- RF_gf(data_RF,predictors_FCH4[1],predictors_FCH4,plot_RF_results,datetime)
        
        df_gap_filled_FCH4[,j] <- gap_filled_FCH4[,1]
      }
    }
    
    #PLOT ALL ROWS! then calc uncertainty
    #calculate column means of specific columns
    FCH4agg <- colMeans(data[ ,column_name], na.rm=T)
    
    #compute uncertainty across aggregated values
    sdFCH4agg_ustar <- sd(FCH4agg)
    
    # Combined aggregated uncertainty
    
    #Assuming that the uncertainty due to unknown u*threshold is independent from the random uncertainty, the variances add.
    FCH4_sdAnnual <- data.frame(
      sd_NEE_Rand = resRand$sdMean,
      sd_NEE_Ustar = sdFCH4agg_ustar,
      sd_NEE_Comb = sqrt(resRand$sdMean^2 + sdNEEagg_ustar^2) 
    )
    
    data.mean_NEE_uStar_f <- data.frame(mean(data$NEE_uStar_f, na.rm = TRUE))
    colnames(data.mean_NEE_uStar_f) <- 'mean_NEE_uStar_f'
    NEE_sdAnnual <- cbind(data.mean_NEE_uStar_f,NEE_sdAnnual)
    
    # Convert to annual sums
    conv_gCO2 <- 1/(10^6)*44.01*60*60*24*length(data$NEE_uStar_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year
    conv_gC <- 1/(10^6)*12.011*60*60*24*length(data$NEE_uStar_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year
    
    # g CO2
    mean_sdAnnual_gCO2 <- mean_sdAnnual*conv_gCO2
    mean_sdAnnual_gCO2
    
    mean_sdAnnual_gCO2_all <- rbind(mean_sdAnnual_gCO2_all,mean_sdAnnual_gCO2)
    
    # g C
    mean_sdAnnual_gC <- mean_sdAnnual*conv_gC
    mean_sdAnnual_gC
    mean_sdAnnual_gC_all <- rbind(mean_sdAnnual_gC_all,mean_sdAnnual_gC)
  }
  
  # List output!
}



#######################################################################

#######################################################################
########### other items in the code not strictly needed #######

# halfhourly observations

CombinedData %>%
  #filter(!c(Year == 2018 & doy < 121)) %>% 
  #filter(!c(Year == 2024 & doy > 121)) %>% 
  #group_by(Year, doy) %>%
  # summarise(
  # NEE_raw = mean(NEE_raw, na.rm = TRUE),
  # NEE = mean(NEE, na.rm = TRUE),
  # NEE_U50_orig = mean(NEE_U50_orig, na.rm = TRUE),
  # NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE)) %>%
  pivot_longer(
    cols = c(NEE, NEE_U50_f, NEE_U50_orig),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(variables = factor(variables, levels = c("NEE", "NEE_U50_orig", "NEE_U50_f"))) %>%
  mutate(
    variables = recode(variables, 
                       "NEE_U50_f" = "u*-filtered & gap-filled NEE",
                       "NEE_U50_orig" = "u*-filtered NEE",
                       "NEE" = "NEE after initial filtering",
    )) %>% 
  ggplot(aes(x = doy, y = value, colour = variables))+
  geom_point(alpha = 0.4, size = 0.75)+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  facet_grid(vars(variables), vars(Year))+
  #facet_wrap(Year~., nrow = 4)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "none"
  )

ggsave(filename = "rproc_p3_nee.png", width = 10, height = 6, dpi = 300)


# estimating the n 

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 121)) %>%
  summarise(
    n_NEE_raw = sum(!is.na(NEE_raw)),
    n_NEE = sum(!is.na(NEE)),
    n_NEE_U50_orig = sum(!is.na(NEE_U50_orig)),
    n_NEE_U50_f = sum(!is.na(NEE_U50_f)),
    n_NEE_U50_f_na = sum(is.na(NEE_U50_f))
  ) %>%
  summarise(
    total_n_NEE_raw  = sum(n_NEE_raw),
    total_n_NEE = sum(n_NEE),
    total_n_NEE_U50_orig = sum(n_NEE_U50_orig),
    total_n_NEE_U50_f = sum(n_NEE_U50_f),
    total_n_NEE_U50_f_na  = sum(n_NEE_U50_f_na )
  )    

n_NEE_U50_f_check <- CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 121)) %>%
  select(NEE, NEE_U50_f,NEE_U50_orig, Year, DoY, Hour, Ustar, Ustar_U50_Thres) %>% 
  filter(is.na(NEE_U50_f))

# annual values

CombinedData %>%
  group_by(doy, Year) %>% 
  summarise(
    NEE_U50_f = mean(NEE_U50_f),
    GPP_DT_U50 = mean(GPP_DT_U50),
    Reco_DT_U50 = mean(Reco_DT_U50) 
  ) %>% 
  group_by(Year) %>%
  summarise(
  NEE_U50_f = sum(NEE_U50_f),
  GPP_DT_U50 = -sum(GPP_DT_U50),
  Reco_DT_U50 = sum(Reco_DT_U50)
  ) %>%
  pivot_longer(
    cols = c(NEE_U50_f, GPP_DT_U50, Reco_DT_U50),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(variables = factor(variables, levels = c("GPP_DT_U50", "Reco_DT_U50", "NEE_U50_f"))) %>%
  ggplot(aes(x = variables, y = value, fill = variables))+
  geom_col(colour = "black")+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  geom_text(aes(label = round(value, 0)), vjust = -0.5)+
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle=90, vjust=0.5)
  )

# looking at daily averages

# graph

CombinedData %>%
    filter(!c(Year == 2018 & doy < 121)) %>% 
    filter(!c(Year == 2024 & doy > 121)) %>% 
    group_by(Year, doy) %>%
    summarise(
      NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE)) %>% 
  pivot_longer(
          cols = c(NEE_U50_f),
          names_to = "variables",
          values_to = "value"
        ) %>%
        mutate(variables = factor(variables, levels = c("NEE_U50_f"))) %>%
        mutate(
          variables = recode(variables, 
                             "NEE_U50_f" = "u*-filtered & gap-filled NEE",

          )) %>% 
        ggplot(aes(x = doy, y = value, colour = variables))+
        geom_point(alpha = 0.5)+
        geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
        facet_grid(vars(variables), vars(Year))+
        #facet_wrap(Year~., nrow = 4)+
        theme_bw()+
        ylab("NEE (umolm-2s-1)")+
        theme(
          legend.position = "none"
        )

# number of observations

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 121)) %>% 
  group_by(Year, doy) %>%
  summarise(
    n_obs_nee = sum(!is.na(NEE)),  # Count non-NA half-hourly values
    NEE = ifelse(n_obs_nee >= 24, mean(NEE, na.rm = TRUE), NA_real_),  
    n_obs_NEE_U50_orig = sum(!is.na(NEE_U50_orig)),
    NEE_U50_orig = ifelse(n_obs_NEE_U50_orig >= 24, mean(NEE_U50_orig, na.rm = TRUE), NA_real_),
    n_obs_NEE_U50_f = sum(!is.na(NEE_U50_f)),
    NEE_U50_f = ifelse(n_obs_NEE_U50_f >= 24, mean(NEE_U50_f, na.rm = TRUE), NA_real_)) %>%
  select(-c(n_obs_nee,n_obs_NEE_U50_orig,n_obs_NEE_U50_f )) %>% 
  summarise(
    n_NEE = sum(!is.na(NEE)),
    n_NEE_U50_orig = sum(!is.na(NEE_U50_orig)),
    n_NEE_U50_f = sum(!is.na(NEE_U50_f)),
    n_NEE_U50_f_na = sum(is.na(NEE_U50_f))
  ) %>%
  summarise(
    total_n_NEE = sum(n_NEE),
    total_n_NEE_U50_orig = sum(n_NEE_U50_orig),
    total_n_NEE_U50_f = sum(n_NEE_U50_f),
    total_n_NEE_U50_f_na = sum(n_NEE_U50_f_na)
  )    
  
daily_averages_nee <- CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 121)) %>% 
  group_by(Year, doy) %>%
  summarise(
    n_obs_nee = sum(!is.na(NEE)),  # Count non-NA half-hourly values
    NEE = ifelse(n_obs_nee >= 24, mean(NEE, na.rm = TRUE), NA_real_),  
    n_obs_NEE_U50_orig = sum(!is.na(NEE_U50_orig)),
    NEE_U50_orig = ifelse(n_obs_NEE_U50_orig >= 24, mean(NEE_U50_orig, na.rm = TRUE), NA_real_),
    n_obs_NEE_U50_f = sum(!is.na(NEE_U50_f)),
    NEE_U50_f = ifelse(n_obs_NEE_U50_f >= 24, mean(NEE_U50_f, na.rm = TRUE), NA_real_)) %>%
  select(-c(n_obs_nee,n_obs_NEE_U50_orig,n_obs_NEE_U50_f ))

# night-time partitioning vs day-time partitioning

CombinedData %>%
  filter(Year %in% c(2020, 2021, 2022)) %>% 
  group_by(Year, doy) %>%
  summarise(
    NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE),
    GPP_U50_f = -mean(GPP_U50_f, na.rm = TRUE),
    GPP_DT_U50 = -mean(GPP_DT_U50, na.rm = TRUE),
    Reco_DT_U50 = mean(Reco_DT_U50, na.rm = TRUE),
    Reco_U50 = mean(Reco_U50, na.rm = TRUE)
  ) %>%
  group_by(Year) %>%
  # mutate(
  #   cum_NEE = cumsum(NEE_U50_f, na.rm = TRUE),
  #   cum_GPP_NT = cumsum(GPP_U50_f, na.rm = TRUE),  # Cumulative GPP (nighttime partitioning)
  #   cum_GPP_DT = cumsum(GPP_DT_U50, na.rm = TRUE),  # Cumulative GPP (daytime partitioning)
  #   cum_Reco_DT = cumsum(Reco_DT_U50, na.rm = TRUE),
  #   cum_Reco_NT = cumsum(Reco_U50, na.rm = TRUE)
  # ) %>%
  mutate(
    cum_NEE = cumsum(replace_na(NEE_U50_f, 0)),
    cum_GPP_NT = cumsum(replace_na(GPP_U50_f, 0)),  
    cum_GPP_DT = cumsum(replace_na(GPP_DT_U50, 0)),  
    cum_Reco_DT = cumsum(replace_na(Reco_DT_U50, 0)),
    cum_Reco_NT = cumsum(replace_na(Reco_U50, 0))
  ) %>% 
  pivot_longer(cols = starts_with("cum_"), names_to = "Variable", values_to = "Cumulative_Sum") %>%
  ggplot(aes(x = doy, y = Cumulative_Sum, color = Variable, linetype = Variable)) +
  
  geom_line(linewidth = 1, alpha = 0.5) +  # Match line thickness & transparency
  
  scale_color_manual(values = c("cum_GPP_NT" = "blue", "cum_GPP_DT" = "darkorange", "cum_Reco_NT" = "blue", "cum_Reco_DT" = "darkorange", cum_NEE = "darkgray")) +  # Match colors
  scale_linetype_manual(values = c("cum_GPP_NT" = "solid", "cum_GPP_DT" = "solid", "cum_Reco_NT" = "dashed", "cum_Reco_DT" = "dashed", cum_NEE = "solid")) + 
  labs(
    x = "Day of Year (DOY)",
    y = "Cumulative Sum",
    title = "Cumulative CO2 Fluxes Over the Year",
    color = "Variable",
    linetype = "Year"
  ) +
  geom_hline(yintercept = 0)+
  facet_wrap(Year ~ ., nrow = 1) +  # Match facet style
  theme_bw()

ggsave("ntvsdt.png", plot = last_plot(), width = 10, height = 5)
?ggsave()


## plotting the results and comparing other partitioning and gap-filling methods

# bring in the data Shannon separated for CO2 for 2023-2021 using Barr et al. (2004)

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
  geom_vline(data = filter(seasonStarts2, year %in% c(2023:2023)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# comparison graphs for respiration

combined%>% 
  ggplot(aes(x = doy, y = resp))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = resp_obs), color = "#D55E00", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2023:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# comparison graphs for gpp

combined%>% 
  ggplot(aes(x = doy, y = gpp*(-1)))+
  geom_line(size = 1, color = "black", alpha = 0.75)+
  geom_line(aes(x = doy, y = gpp_obs), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  geom_vline(data = filter(seasonStarts2, year %in% c(2023:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# graphing the sum of the fluxes across the entire year

# new dataset

combined_inner <- new_co2 %>% 
  filter(plot == 1) %>% 
  inner_join(filled, by = c("year", "doy")) # full join keeps only those points that are present in both datasets.

# merged graph

combined_inner %>% 
  filter(year %in% c(2023:2021)) %>%
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


#######################################################################

#######################################################################
###### importing flux gradient data co2 ###############################

# p1/2

# import flux gradient file 

column_names <- c("Year", "Date", "Plot", "CO2_flux", "flux_l", "grad", "K", "u_star", "H", 
                  "Ts", "int_z_l", "int_z_u", "d", "z0", "fetch_intake_big", "fetch_intake_small", 
                  "BL", "F1", "F2") # I think CO2_flux is in g co2/m2s
#2018

flux_grad_p1_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_P1",
                              header = FALSE)
colnames(flux_grad_p1_2018) <- flux_grad_p1_2018[2, ]
flux_grad_p1_2018 <- flux_grad_p1_2018[-c(1:3), ]

flux_grad_p2_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_P2", 
                              header = FALSE)
colnames(flux_grad_p2_2018) <- flux_grad_p2_2018[2, ]
flux_grad_p2_2018 <- flux_grad_p2_2018[-c(1:3), ]

#2019

flux_grad_p1_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_P1",
                              header = FALSE)
colnames(flux_grad_p1_2019) <- flux_grad_p1_2019[2, ]
flux_grad_p1_2019 <- flux_grad_p1_2019[-c(1:3), ]

flux_grad_p2_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_P2", 
                              header = FALSE)
colnames(flux_grad_p2_2019) <- flux_grad_p2_2019[2, ]
flux_grad_p2_2019 <- flux_grad_p2_2019[-c(1:3), ]

#2020

flux_grad_p1_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_P1",
                              header = FALSE)
colnames(flux_grad_p1_2020) <- flux_grad_p1_2020[2, ]
flux_grad_p1_2020 <- flux_grad_p1_2020[-c(1:3), ]

flux_grad_p2_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_P2", 
                              header = FALSE)
colnames(flux_grad_p2_2020) <- flux_grad_p2_2020[2, ]
flux_grad_p2_2020 <- flux_grad_p2_2020[-c(1:3), ]

#2021

flux_grad_p1_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_P1",
                              header = FALSE)
colnames(flux_grad_p1_2021) <- flux_grad_p1_2021[2, ]
flux_grad_p1_2021 <- flux_grad_p1_2021[-c(1:3), ]

flux_grad_p2_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_P2", 
                              header = FALSE)
colnames(flux_grad_p2_2021) <- flux_grad_p2_2021[2, ]
flux_grad_p2_2021 <- flux_grad_p2_2021[-c(1:3), ]

#2022

flux_grad_p1_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P1.csv",
                              header = FALSE)
colnames(flux_grad_p1_2022) <- flux_grad_p1_2022[2, ]
flux_grad_p1_2022 <- flux_grad_p1_2022[-c(1:3), ]

flux_grad_p2_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P2.csv", 
                              header = FALSE)
colnames(flux_grad_p2_2022) <- flux_grad_p2_2022[2, ]
flux_grad_p2_2022 <- flux_grad_p2_2022[-c(1:3), ]


#2023

flux_grad_p1_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_P1",
                              header = FALSE)
colnames(flux_grad_p1_2023) <- flux_grad_p1_2023[2, ]
flux_grad_p1_2023 <- flux_grad_p1_2023[-c(1:3), ]

flux_grad_p2_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_P2", 
                              header = FALSE)
colnames(flux_grad_p2_2023) <- flux_grad_p2_2023[2, ]
flux_grad_p2_2023 <- flux_grad_p2_2023[-c(1:3), ]

#2024

flux_grad_p1_2024 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2024/CO2flux_P1.csv",
                              header = FALSE)
colnames(flux_grad_p1_2024) <- flux_grad_p1_2024[2, ]
flux_grad_p1_2024 <- flux_grad_p1_2024[-c(1:3), ]

flux_grad_p2_2024 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2024/CO2flux_P2.csv", 
                              header = FALSE)
colnames(flux_grad_p2_2024) <- flux_grad_p2_2024[2, ]
flux_grad_p2_2024 <- flux_grad_p2_2024[-c(1:3), ]

# processing

flux_grad_p1p2_2018_2024 <- bind_rows(flux_grad_p1_2018, flux_grad_p2_2018, flux_grad_p1_2019, flux_grad_p2_2019, flux_grad_p1_2020, flux_grad_p2_2020, flux_grad_p1_2021, flux_grad_p2_2021, flux_grad_p1_2022, flux_grad_p2_2022, flux_grad_p1_2023, flux_grad_p2_2023, flux_grad_p1_2024, flux_grad_p2_2024)

flux_grad_p1p2_2018_2024$CO2_flux_umol <- (as.numeric(flux_grad_p1p2_2018_2024$`CO2 flux`) / 44.01) 

flux_grad_p1p2_2018_2024 <- mutate(flux_grad_p1p2_2018_2024, 
                                   CO2_flux_umol = ifelse(CO2_flux_umol < -100, NA, CO2_flux_umol), 
                                   CO2_flux_umol = ifelse(CO2_flux_umol > 60, NA, CO2_flux_umol)
)

flux_grad_p1p2_2018_2024$Date <- as.numeric(flux_grad_p1p2_2018_2024$` Date`)
flux_grad_p1p2_2018_2024$Year <- as.numeric(flux_grad_p1p2_2018_2024$`% Year`)

# loading eddy data to compare

CombinedData_p12$Date <- CombinedData_p12$DoY + CombinedData_p12$Hour / 24

# merging

flux_ec_2018_2024 <- CombinedData_p12 %>% 
  mutate(Date = round(Date, 3)) %>%
  left_join(
    flux_grad_p1p2_2018_2024 %>% mutate(Date = round(Date, 3)), 
    by = c("Date", "Year")
  )
flux_ec_2018_2024 %>%
  filter(Year == 2023) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()

flux_ec_2018_2024_model <- flux_ec_2018_2024 %>% 
  filter(!c(Year==2018 & doy<121)) %>% 
  filter(!c(Year==2024 & doy>121))         

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_2018_2024_model, Year==2023))
summary(model)

flux_grad_p1p2_2018_2024 %>%
  #filter(Year == 2024) %>% 
  ggplot(aes(x = Date, y = CO2_flux_umol, color =  as.factor(` Plot`)))+
  geom_point(alpha = 0.4, size = 0.8)+
  scale_color_manual(values = c("#0072B2", "#D55E00"))+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  xlab("DOY")
  theme(
    legend.position = "bottom"
  )

ggsave("fg_p12_2018_2024.png", plot = last_plot(), width = 11, height = 3.5)
  
  
# graph

flux_ec_2018_2024 %>%
  filter(!c(Year == 2018 & doy < 121)) %>% 
  
  # filter(!c(Year == 2018 & doy < 121)) %>% 
  # filter(!c(Year == 2024 & doy > 121)) %>% 
  # group_by(Year, doy) %>%
  # summarise(
  #   NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE),
  #   CO2_flux_umol = mean(CO2_flux_umol, na.rm = TRUE)
  #   ) %>% 
  pivot_longer(
    cols = c(NEE_U50_f, CO2_flux_umol),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(variables = factor(variables, levels = c("CO2_flux_umol", "NEE_U50_f"))) %>%
  # mutate(
  #   variables = recode(variables, 
  #                      "NEE_U50_f" = "u*-filtered & gap-filled NEE",
  #                      
  #   )) %>% 
  ggplot(aes(x = doy, y = value, colour = variables))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "bottom"
  )
flux_ec_2018_2024 %>%
  filter(Year == 2024 & abs(NEE_U50_f) > 25) %>% 
  select(NEE, NEE_U50_f, NEE_U50_orig, Ustar, Ustar_U50_Thres, DoY, Hour)
  

check <- flux_ec_2018_2024 %>%
  filter(!c(Year == 2018 & doy < 121)) %>% 
  filter(!c(Year == 2024 & doy > 121)) %>% 
  filter(Year == 2023) %>% 
  group_by(Year, doy) %>%
  summarise(
    NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE),
    CO2_flux_umol = mean(CO2_flux_umol, na.rm = TRUE)
  )


# checking specfiic data to fill gaps

#Current gaps in the data are for DOY 268-340 in 2019, and DOY 225 to 305 in 2023; this is the relationship between EC and flux gradient points in 2020 and 2022 (two other soybean years), for the period doy 268-305. Based on what was discussed with Claudia, this could be used as a justification to gap-fill with flux gradient data.

flux_ec_2018_2024 %>% 
  filter(Year %in% c(2020, 2022)) %>% 
  filter(DoY %in% c(225:340)) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_2018_2024, Year %in% c(2020, 2022) & DoY %in% c(225:340)))
summary(model)

# converting from umol/m2/s to g/m2/day

nee_daily_c <- flux_ec_2018_2024 %>%
  mutate(
    daily_C = NEE_U50_f * 1800 * (12 / 10^6)  # Convert umol CO2/m²/s to g C/m² per half-hour
  ) %>%
  group_by(Year, DoY) %>%  # Assuming you have Year and DOY columns
  summarise(
    total_C = sum(daily_C, na.rm = TRUE)  # Sum over all half-hourly values in a day
  )

nee_daily_c %>% 
  ggplot()+
  geom_line(aes(x = DoY, y = total_C ))+
  theme_bw()+
  facet_wrap(Year~.)+
  ylab("NEE (g C m-2 d-1)")

# p3/4

# import flux gradient file 

column_names <- c("Year", "Date", "Plot", "CO2_flux", "flux_l", "grad", "K", "u_star", "H", 
                  "Ts", "int_z_l", "int_z_u", "d", "z0", "fetch_intake_big", "fetch_intake_small", 
                  "BL", "F1", "F2") # I think CO2_flux is in g co2/m2s

#2018

flux_grad_p3_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2018) <- flux_grad_p3_2018[2, ]
flux_grad_p3_2018 <- flux_grad_p3_2018[-c(1:3), ]

flux_grad_p4_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2018) <- flux_grad_p4_2018[2, ]
flux_grad_p4_2018 <- flux_grad_p4_2018[-c(1:3), ]

#2019

flux_grad_p3_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2019) <- flux_grad_p3_2019[2, ]
flux_grad_p3_2019 <- flux_grad_p3_2019[-c(1:3), ]

flux_grad_p4_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2019) <- flux_grad_p4_2019[2, ]
flux_grad_p4_2019 <- flux_grad_p4_2019[-c(1:3), ]

#2020

flux_grad_p3_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2020) <- flux_grad_p3_2020[2, ]
flux_grad_p3_2020 <- flux_grad_p3_2020[-c(1:3), ]

flux_grad_p4_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2020) <- flux_grad_p4_2020[2, ]
flux_grad_p4_2020 <- flux_grad_p4_2020[-c(1:3), ]


#2021

flux_grad_p3_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2021) <- flux_grad_p3_2021[2, ]
flux_grad_p3_2021 <- flux_grad_p3_2021[-c(1:3), ]

flux_grad_p4_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2021) <- flux_grad_p4_2021[2, ]
flux_grad_p4_2021 <- flux_grad_p4_2021[-c(1:3), ]

#2022

flux_grad_p3_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P3.csv",
                              header = FALSE)
colnames(flux_grad_p3_2022) <- flux_grad_p3_2022[2, ]
flux_grad_p3_2022 <- flux_grad_p3_2022[-c(1:3), ]

flux_grad_p4_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P4.csv", 
                              header = FALSE)
colnames(flux_grad_p4_2022) <- flux_grad_p4_2022[2, ]
flux_grad_p4_2022 <- flux_grad_p4_2022[-c(1:3), ]


#2023

flux_grad_p3_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2023) <- flux_grad_p3_2023[2, ]
flux_grad_p3_2023 <- flux_grad_p3_2023[-c(1:3), ]

flux_grad_p4_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2023) <- flux_grad_p4_2023[2, ]
flux_grad_p4_2023 <- flux_grad_p4_2023[-c(1:3), ]

#2024

flux_grad_p3_2024 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2024/CO2flux_p3.csv",
                              header = FALSE)
colnames(flux_grad_p3_2024) <- flux_grad_p3_2024[2, ]
flux_grad_p3_2024 <- flux_grad_p3_2024[-c(1:3), ]

flux_grad_p4_2024 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2024/CO2flux_P4.csv", 
                              header = FALSE)
colnames(flux_grad_p4_2024) <- flux_grad_p4_2024[2, ]
flux_grad_p4_2024 <- flux_grad_p4_2024[-c(1:3), ]


# processing

flux_grad_p3p4_2018_2024 <- bind_rows(flux_grad_p3_2018, flux_grad_p4_2018,flux_grad_p3_2019, flux_grad_p4_2019,flux_grad_p3_2020, flux_grad_p4_2020, flux_grad_p3_2021, flux_grad_p4_2021, flux_grad_p3_2022, flux_grad_p4_2022, flux_grad_p3_2023, flux_grad_p4_2023, flux_grad_p3_2024, flux_grad_p4_2024)

flux_grad_p3p4_2018_2024$CO2_flux_umol <- (as.numeric(flux_grad_p3p4_2018_2024$`CO2 flux`) / 44.01) 

flux_grad_p3p4_2018_2024 <- mutate(flux_grad_p3p4_2018_2024, 
                                   CO2_flux_umol = ifelse(CO2_flux_umol < -100, NA, CO2_flux_umol), 
                                   CO2_flux_umol = ifelse(CO2_flux_umol > 60, NA, CO2_flux_umol)
)

flux_grad_p3p4_2018_2024$Date <- as.numeric(flux_grad_p3p4_2018_2024$` Date`)
flux_grad_p3p4_2018_2024$Year <- as.numeric(flux_grad_p3p4_2018_2024$`% Year`)

flux_grad_p3p4_2018_2024 %>%
  #filter(Year == 2024) %>% 
  ggplot(aes(x = Date, y = CO2_flux_umol, color =  as.factor(` Plot`)))+
  geom_point(alpha = 0.4, size = 0.8)+
  scale_color_manual(values = c("#0072B2", "#D55E00"))+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  xlab("DOY")+
  theme(
    legend.position = "right"
  )

ggsave("fg_p34_2018_2024.png", plot = last_plot(), width = 11, height = 3.5)

# loading eddy data to compare

CombinedData$Date <- CombinedData$DoY + CombinedData$Hour / 24

# merging

flux_ec_p34_2018_2024 <- CombinedData %>% 
  mutate(Date = round(Date, 3)) %>%
  left_join(
    flux_grad_p3p4_2018_2024 %>% mutate(Date = round(Date, 3)), 
    by = c("Date", "Year")
  )
flux_ec_p34_2018_2024 %>%
  #filter(Year == 2023) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_p34_2018_2024, Year == 2023))
summary(model)
model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = flux_ec_p34_2018_2024)
summary(model)


# graph

flux_ec_2018_2023 %>%
  filter(!c(Year == 2018 & doy < 121)) %>% 
  filter(!c(Year == 2024 & doy > 121)) %>% 
  # group_by(Year, doy) %>%
  # summarise(
  #   NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE),
  #   CO2_flux_umol = mean(CO2_flux_umol, na.rm = TRUE)
  #   ) %>% 
  pivot_longer(
    cols = c(NEE_U50_f, CO2_flux_umol),
    names_to = "variables",
    values_to = "value"
  ) %>%
  # mutate(variables = factor(variables, levels = c("NEE_U50_f"))) %>%
  # mutate(
  #   variables = recode(variables, 
  #                      "NEE_U50_f" = "u*-filtered & gap-filled NEE",
  #                      
  #   )) %>% 
  ggplot(aes(x = doy, y = value, colour = variables))+
  geom_point(alpha = 0.5)+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "bottom"
  )


check <- flux_ec_2018_2023 %>%
  filter(!c(Year == 2018 & doy < 121)) %>% 
  filter(!c(Year == 2024 & doy > 121)) %>% 
  filter(Year == 2023) %>% 
  group_by(Year, doy) %>%
  summarise(
    NEE_U50_f = mean(NEE_U50_f, na.rm = TRUE),
    CO2_flux_umol = mean(CO2_flux_umol, na.rm = TRUE)
  )





#######################################################################
