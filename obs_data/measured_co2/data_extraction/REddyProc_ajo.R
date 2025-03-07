# readyproc data processing
# 06/12/2024
# agustin olivo
# aolivo@uoguelph.ca

########################################################################
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
# source 3 (initial REddyProc paper): https://bg.copernicus.org/articles/15/5015/2023/bg-15-5015-2023.html
# source 4 (definition of different friction velocities across the year - u*): https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html
# source 5: https://rstudio-pubs-static.s3.amazonaws.com/84133_4c4347b1ba5e4067980787a85e27d68f.html
# source 6 (video): https://www.youtube.com/watch?v=-b0vc4u8kls

# key papers/documents:

# source 7: Wutzler, T., Lucas-Moffat, A., Migliavacca, M., Knauer, J., Sickel, K., Šigut, L., Menzer, O., and Reichstein, M.: Basic and extensible post-processing of eddy covariance flux data with REddyProc, Biogeosciences, 15, 5015–5030, https://doi.org/10.5194/bg-15-5015-2018, 2018.
# source 8: reference manual in https://cran.r-project.org/web/packages/REddyProc/index.html

# general info

# before starting, please clean everything from your global environment.Proceed with step 1, one plot at a time. Then with step 2. Once you finish estimating everything for one plot, clean everything in your environment again, and go back to Step 1 for the new plot.


########################################################################

###############################################################################
####################### STEP 1: load data for each plot #######################

####################### plot 1-2 ######################################

# The workflow starts with importing the half-hourly data. 

EddyData_2018_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2018_R.txt")
EddyData_2018_p3 <- EddyData_2018_p3 %>% 
  filter(DoY < 195) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1
EddyData_2018_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2018_R.txt")
EddyData_2018_p1 <- EddyData_2018_p1 %>% 
  filter(DoY > 194) 
EddyData_2019_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2019_R.txt")
EddyData_2020_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2020_R.txt")
EddyData_2021_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2021_R.txt")
EddyData_2022_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2022_R.txt")
EddyData_2023_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2023_R.txt")
EddyData_2024_p1 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P12_tower_2024_R.txt")

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
  c(121,2024)
)))


#######################################################################

###################### plot 3-4 #######################################

EddyData_2018_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2018_R.txt")
EddyData_2019_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2019_R.txt")
EddyData_2020_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2020_R.txt")
EddyData_2021_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2021_R.txt")
EddyData_2022_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2022_R.txt")
EddyData_2023_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2023_R.txt")
EddyData_2024_p3 <- fLoadTXTIntoDataframe("obs_data\\measured_co2\\data_extraction\\shared_by_pat_ec\\P3_tower_2024_R.txt")

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
  c(121,2024)
)))

#######################################################################

#######################################################################

########################################################################
########################### MAIN CODE ###################################

# there are a few different options on how to conduct this analysis; the one below considers generating different ustar seasons, different ustar thresholds for them, and also estimating the probability associated with those; in another files that Shannon had compiled, a slightly different approach is used where the seasons are generated, but the probability associated wit the ustar tresholds are not estimated.

# key source of information: https://cran.r-project.org/web/packages/REddyProc/vignettes/DEGebExample.html

# filtering out values outside of the range (mostly based on source 5 and conversations with Patrick)

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
EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg')) # this command creates an object of class "sEddyProc" with the data from EddyDataWithPosix, and verifies if there are any values outside of pre-defined ranges (via warning message; for NEE it uses values that are less than -50, and for VPD negative values)

#### additional data quality check steps ####

# check structure of the data
str(EProc$sDATA)

# statistical summary of the data
round(describe(EProc$sDATA[,-1]), 2)

# plots across multiple years and variables to check the units are consistent

EddyData_initial_explo <- EddyData %>% 
  pivot_longer(
    cols = c(LE, H, Ustar,Tair, RH, VPD, Rg),
    names_to = "variables",
    values_to = "values"
  )

EddyData_initial_explo %>% 
  ggplot(aes(x = DoY, y = values, colour = variables))+
  geom_point(alpha = 0.5)+
  facet_grid(vars(variables), vars(Year), scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )+
  geom_vline(xintercept = 121, color="red", linetype = "dashed")

ggsave("basic.png", plot = last_plot(), width = 12, height = 7)

# plor for nee only

EddyData_initial_explo_nee <-EddyData %>% 
  pivot_longer(
    cols = c(NEE, NEE_raw),
    names_to = "variables",
    values_to = "values"
  )

EddyData_initial_explo_nee %>% 
  ggplot(aes(x = DoY, y = values, fill = variables))+
  geom_point(alpha = 0.5, aes(color = variables))+
  facet_grid(vars(variables), vars(Year), scales = "free_y")+
  theme_bw()+
  theme(
    legend.position = "none"
  )+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")

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

EProc$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 100L, probs = c(0.50)) # i kept these scenarios only for 0.5 to make it run quicker, but i should use: c(0.05, 0.50, 0.95)
EProc$sGetEstimatedUstarThresholdDistribution()

#The output reports annually aggregated uStar estimates for the original data and lower, median, and upper quantile of the estimated distribution. The threshold can vary between periods of different surface roughness, or pre-defined seasons. Therefore, there are estimates for different time periods, called seasons (those were defined in previous step)

# The subsequent post processing steps will be repeated using the four $u_$ threshold scenarios (non-resampled and tree quantiles of the bootstrapped distribution). They require to specify a $u_$-threshold for each season and a suffix to distinguish the outputs related to different thresholds. By default the annually aggregated estimates are used for each season within the year.

EProc$useSeaonsalUStarThresholds()
EProc$sGetUstarScenarios()

# actual gap-filling

EProc$sMDSGapFillUStarScens('NEE', FillAll = TRUE)
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

EProc$sMRFluxPartitionUStarScens() # night time partitioning -> Reco, GPP
EProc$sApplyUStarScen(EProc$sMRFluxPartition)
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE) # extract the name of the variables that will be used

# daytime method

EProc$sGLFluxPartitionUStarScens()	# day time partitioning -> Reco_DT, GPP_DT
EProc$sApplyUStarScen(EProc$sGLFluxPartition)
grep("GPP_DT_.*_f$|Reco", names(EProc$sExportResults()), value = TRUE)

# exporting the data

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData) # combining this files helps bring back the doy 
CombinedData$doy <- CombinedData$DoY

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

# plot to look at the raw NEE data + gap-filled NEE data (for half-hourly observations and for daily means)

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
  geom_point(alpha = 0.5)+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  facet_grid(vars(variables), vars(Year))+
  #facet_wrap(Year~., nrow = 4)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "none"
  )

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


########################################################################

########################################################################
###### importing flux gradient data co2 ####################################

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
                  CO2_flux_umol = ifelse(abs(CO2_flux_umol) > 60, NA, CO2_flux_umol)
)

flux_grad_p1p2_2018_2024$Date <- as.numeric(flux_grad_p1p2_2018_2024$` Date`)
flux_grad_p1p2_2018_2024$Year <- as.numeric(flux_grad_p1p2_2018_2024$`% Year`)

# loading eddy data to compare

CombinedData$Date <- CombinedData$DoY + CombinedData$Hour / 24

# merging

flux_ec_2018_2024 <- CombinedData %>% 
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

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_2018_2024, Year == 2023))
summary(model)


# graph

flux_ec_2018_2024 %>%
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

flux_grad_p3p4_2018_2023 <- bind_rows(flux_grad_p3_2023, flux_grad_p4_2023, flux_grad_p3_2024, flux_grad_p4_2024)

flux_grad_p3p4_2018_2023$CO2_flux_umol <- (as.numeric(flux_grad_p3p4_2018_2023$`CO2 flux`) / 44.01) 

flux_grad_p3p4_2018_2023 <- mutate(flux_grad_p3p4_2018_2023, 
                                   CO2_flux_umol = ifelse(abs(CO2_flux_umol) > 60, NA, CO2_flux_umol)
)

flux_grad_p3p4_2018_2023$Date <- as.numeric(flux_grad_p3p4_2018_2023$` Date`)
flux_grad_p3p4_2018_2023$Year <- as.numeric(flux_grad_p3p4_2018_2023$`% Year`)

# loading eddy data to compare

CombinedData$Date <- CombinedData$DoY + CombinedData$Hour / 24

# merging

flux_ec_p34_2023_2024 <- CombinedData %>% 
  mutate(Date = round(Date, 3)) %>%
  left_join(
    flux_grad_p3p4_2018_2023 %>% mutate(Date = round(Date, 3)), 
    by = c("Date", "Year")
  )
flux_ec_p34_2023_2024 %>%
  #filter(Year == 2023) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_p34_2023_2024, Year == 2023))
model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = flux_ec_p34_2023_2024)

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





















########################################################################

########################################################################
###### importing flux gradient data n2o ####################################

# p1/2

# import flux gradient file 

column_names <- c("Year", "DOY", "Plot", "N2O_flux", "flux_l", "grad", "K", "u_star", "H", "Ts",
                  "int_z_l", "int_z_u", "d", "z0", "fetch_intake_big", "fetch_intake_small",
                  "BL", "F1", "F2")
#2018
flux_grad_p1_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2018_n2o) <- column_names
flux_grad_p2_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2018_n2o) <- column_names
flux_grad_p3_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2018_n2o) <- column_names
flux_grad_p4_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2018_n2o) <- column_names

#2019
flux_grad_p1_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2019_n2o) <- column_names
flux_grad_p2_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2019_n2o) <- column_names
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names

#2020
flux_grad_p1_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2020_n2o) <- column_names
flux_grad_p2_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2020_n2o) <- column_names
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names

#2021
flux_grad_p1_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2021_n2o) <- column_names
flux_grad_p2_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2021_n2o) <- column_names
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names

#2022
flux_grad_p1_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P1.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p1_2022_n2o) <- column_names
flux_grad_p2_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P2.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p2_2022_n2o) <- column_names
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names

#2023
flux_grad_p1_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2023_n2o) <- column_names
flux_grad_p2_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2023_n2o) <- column_names
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names

#2024
flux_grad_p1_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P1.csv", skip = 3, header = FALSE)
colnames(flux_grad_p1_2024_n2o) <- column_names
flux_grad_p2_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P2.csv", skip = 3, header = FALSE)
colnames(flux_grad_p2_2024_n2o) <- column_names
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p4_2024_n2o) <- column_names

# processing

flux_grad_p1p2_2018_2024_n2o <- bind_rows(
  select(flux_grad_p1_2018_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2018_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2019_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2019_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2020_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2020_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2021_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2021_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2022_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2022_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2023_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2023_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p1_2024_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p2_2024_n2o, Plot, Year, DOY, N2O_flux, u_star, H)
) %>% 
  arrange(Year, DOY)

flux_grad_p1p2_2018_2024_n2o$N2O_flux <- (as.numeric(flux_grad_p1p2_2018_2024_n2o$N2O_flux )*0.864) # converting from nanograms of N₂O-N per square meter per second (ng N₂O-N/m²/s) to grams of N₂O-N per hectare per day (g N₂O-N/ha/day)
flux_grad_p1p2_2018_2024_n2o <- flux_grad_p1p2_2018_2024_n2o %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
flux_grad_p1p2_2018_2024_n2o <- flux_grad_p1p2_2018_2024_n2o %>%
  mutate(DoY = as.integer(DOY))

# merging
str(azeem)
flux_grad_p1p2_2018_2024_n2o %>%
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, scales = "free_y")+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 


str(flux_grad_p1p2_2018_2024_n2o)

flux_grad_p1p2_2018_2024_n2o %>%
  group_by(Year, DoY) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  ) %>% 
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, )+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 

azeem$DoY = azeem$DOY
azeem$original_data = as.numeric(azeem$original_data )

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(131,2012), # plating
  c(276,2012), # harvest
  c(129,2013), # plating
  c(295,2013), # harvest
  c(146,2014), # plating
  c(308,2014), # harvest
  c(133,2015), # plating
  c(300,2015), # harvest
  c(133,2016), # plating
  c(280,2016), # harvest
  c(138,2017), # plating
  c(313,2017), # harvest
  c(140,2018), # plating
  c(303,2018), # harvest
  c(150,2019), # plating
  c(273,2019), # harvest
  c(213,2020), # harvest
  c(134,2021), # plating
  c(307,2021), # harvest
  c(138,2022), # plating
  c(276,2022), # harvest
  c(221,2023) # harvest
)))
colnames(mgmt_dates_p3) <- c("DoY", "Year")
  
mgmt_dates_p1 <- as.data.frame( do.call( rbind, list(
  c(157,2000),
  c(299,2000),
  c(125,2001),
  c(274,2001),
  c(275,2001),
  c(213,2002),
  c(135,2003),
  c(301,2003),
  c(139,2004),
  c(275,2004),
  c(131,2005),
  c(167,2005),
  c(130,2006),
  c(283,2006),
  c(127,2007),
  c(290,2007),
  c(127,2008),
  c(290,2008),
  c(132,2009),
  c(314,2009),
  c(138,2010),
  c(270,2010),
  c(274,2010),
  c(195,2011),
  c(131,2012), # plating
  c(276,2012), # harvest
  c(129,2013), # plating
  c(295,2013), # harvest
  c(146,2014), # plating
  c(308,2014), # harvest
  c(133,2015), # plating
  c(300,2015), # harvest
  c(133,2016), # planting
  c(280,2016), # harvest
  c(138,2017), # planting
  c(313,2017), # harvest
  c(140,2018), # plating
  c(303,2018), # harvest
  c(150,2019), # plating
  c(273,2019), # harvest
  c(143,2020), # plating
  c(267,2020), # harvest
  c(134,2021), # plating
  c(307,2021), # harvest
  c(138,2022), # plating
  c(276,2022), # harvest
  c(136,2023), # plating
  c(277,2023) # harvest
)))
colnames(mgmt_dates_p1) <- c("DoY", "Year")

# read azeem's data

azeem <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/x_other/simulation_azeem/E26_Gap filling_Azeem_2024paper.xlsx", sheet = "DATA")

azeem12_n2o <- azeem %>% 
  filter(Plot %in% c("P1", "P2")) %>%
  group_by(DoY, Year) %>% 
  summarise(
    avg12_n2o = mean(original_data, na.rm = TRUE)
  )
  

# p3/p4

# import flux gradient file 

column_names <- c("Year", "DOY", "Plot", "N2O_flux", "flux_l", "grad", "K", "u_star", "H", "Ts",
                  "int_z_l", "int_z_u", "d", "z0", "fetch_intake_big", "fetch_intake_small",
                  "BL", "F1", "F2")
#2018
flux_grad_p3_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2018_n2o) <- column_names
flux_grad_p4_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2018_n2o) <- column_names
flux_grad_p3_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2018_n2o) <- column_names
flux_grad_p4_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2018_n2o) <- column_names

#2019
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names

#2020
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names

#2021
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names

#2022
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p4.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names

#2023
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names

#2024
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p4.csv", skip = 3, header = FALSE)
colnames(flux_grad_p4_2024_n2o) <- column_names
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p4_2024_n2o) <- column_names

# processing

flux_grad_p3p4_2018_2024_n2o <- bind_rows(
  select(flux_grad_p3_2018_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2018_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2019_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2019_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2020_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2020_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2021_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2021_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2022_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2022_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2023_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2023_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p3_2024_n2o, Plot, Year, DOY, N2O_flux, u_star, H),
  select(flux_grad_p4_2024_n2o, Plot, Year, DOY, N2O_flux, u_star, H)
) %>% 
  arrange(Year, DOY)

flux_grad_p3p4_2018_2024_n2o$N2O_flux <- (as.numeric(flux_grad_p3p4_2018_2024_n2o$N2O_flux )*0.864) # converting from nanograms of N₂O-N per square meter per second (ng N₂O-N/m²/s) to grams of N₂O-N per hectare per day (g N₂O-N/ha/day)
flux_grad_p3p4_2018_2024_n2o <- flux_grad_p3p4_2018_2024_n2o %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))
flux_grad_p3p4_2018_2024_n2o <- flux_grad_p3p4_2018_2024_n2o %>%
  mutate(DoY = as.integer(DOY))

# merging
flux_grad_p3p4_2018_2024_n2o %>%
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, scales = "free_y")+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 


flux_grad_p3p4_2018_2024_n2o %>%
  group_by(Year, DoY) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  ) %>% 
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, )+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 

azeem$DoY = azeem$DOY
azeem$original_data = as.numeric(azeem$original_data )

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(131,2012), # plating
  c(276,2012), # harvest
  c(129,2013), # plating
  c(295,2013), # harvest
  c(146,2014), # plating
  c(308,2014), # harvest
  c(133,2015), # plating
  c(300,2015), # harvest
  c(133,2016), # plating
  c(280,2016), # harvest
  c(138,2017), # plating
  c(313,2017), # harvest
  c(140,2018), # plating
  c(303,2018), # harvest
  c(150,2019), # plating
  c(273,2019), # harvest
  c(213,2020), # harvest
  c(134,2021), # plating
  c(307,2021), # harvest
  c(138,2022), # plating
  c(276,2022), # harvest
  c(221,2023) # harvest
)))
colnames(mgmt_dates_p3) <- c("DoY", "Year")

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(157,2000),
  c(299,2000),
  c(125,2001),
  c(274,2001),
  c(275,2001),
  c(213,2002),
  c(135,2003),
  c(301,2003),
  c(139,2004),
  c(275,2004),
  c(131,2005),
  c(167,2005),
  c(130,2006),
  c(283,2006),
  c(127,2007),
  c(290,2007),
  c(127,2008),
  c(290,2008),
  c(132,2009),
  c(314,2009),
  c(138,2010),
  c(270,2010),
  c(274,2010),
  c(195,2011),
  c(131,2012), # plating
  c(276,2012), # harvest
  c(129,2013), # plating
  c(295,2013), # harvest
  c(146,2014), # plating
  c(308,2014), # harvest
  c(133,2015), # plating
  c(300,2015), # harvest
  c(133,2016), # planting
  c(280,2016), # harvest
  c(138,2017), # planting
  c(313,2017), # harvest
  c(140,2018), # plating
  c(303,2018), # harvest
  c(150,2019), # plating
  c(273,2019), # harvest
  c(143,2020), # plating
  c(267,2020), # harvest
  c(134,2021), # plating
  c(307,2021), # harvest
  c(138,2022), # plating
  c(276,2022), # harvest
  c(136,2023), # plating
  c(277,2023) # harvest
)))
colnames(mgmt_dates_p3) <- c("DoY", "Year")

# read azeem's data

azeem <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/x_other/simulation_azeem/E26_Gap filling_Azeem_2024paper.xlsx", sheet = "DATA")

azeem12_n2o <- azeem %>% 
  filter(Plot %in% c("P3", "P4")) %>%
  group_by(DoY, Year) %>% 
  summarise(
    avg12_n2o = mean(original_data, na.rm = TRUE)
  )
################################### NOT USED FROM HERE ONWARDS ####################################
########################################################################

########################################################################
###### option 2: data gapfilling  using ustar_threshold - NOT USED ANYMORE #####

# importing the data

# The workflow starts with importing the half-hourly data. 

EddyData <- fLoadTXTIntoDataframe("data\\measured_co2\\data_extraction\\2023\\REddyProc\\P12_tower\\Input\\P12_tower_2023_R.txt")

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

EProc$sPlotFingerprintY('NEE', Year = 2023, valueLimits = c(-200,200))
EProc$sPlotFingerprintY('LE', Year = 2023, valueLimits = c(-100,900))
EProc$sPlotFingerprintY('H', Year = 2023, valueLimits = c(-200,200))
EProc$sPlotHHFluxesY('NEE', Year = 2023)

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

# bring in the data Shannon separated for CO2 for 2023-2021

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
  geom_vline(data = filter(seasonStarts2, year %in% c(2023:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 



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
###### option 3: data gapfilling not using ustar_threshold - NOT USED ANYMORE #####

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
