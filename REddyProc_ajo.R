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
install.packages("rgl")  
install.packages("lubridate")
install.packages("plotly")
install.packages("ggpattern")

# load libraries
library(REddyProc)
library(dplyr)
library(writexl)
library(ggplot2)
library(psych)
library(openair)
library(tidyverse)
library(lognorm) # for uncertainty analysis
library(rgl)
library("dplyr")
library("lubridate")
library("plotly")
library(ggpattern)

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
# source 8: Sara Knox's reddyproc code: https://github.com/CANFLUX/Biomet.net/blob/main/R/database_functions/ThirdStage_REddyProc.R

# key papers/documents:

# source 9: Wutzler, T., Lucas-Moffat, A., Migliavacca, M., Knauer, J., Sickel, K., Šigut, L., Menzer, O., and Reichstein, M.: Basic and extensible post-processing of eddy covariance flux data with REddyProc, Biogeosciences, 15, 5015–5030, https://doi.org/10.5194/bg-15-5015-2018, 2018.
# source 10: reference manual in https://cran.r-project.org/web/packages/REddyProc/index.html

# general info

# before starting, please clean everything from your global environment.Proceed with step 1, one plot at a time. Then with step 2. Once you finish estimating everything for one plot, clean everything in your environment again, and go back to Step 1 for the new plot.


#######################################################################

#######################################################################
####################### STEP 1: load data for each plot ###############

########### loading the actual files

##### plot 1-2 #####

# The workflow starts with importing the half-hourly data. 

# loading data that contains only EC measurements 

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("ec_18_p3")
EddyData_2018_p3 <- EddyData_2018_p3 %>%
   filter(DoY < 194) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1
EddyData_2018_p1 <- fLoadTXTIntoDataframe("ec_18_p12") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2018_p1 <- EddyData_2018_p1 %>%
     filter(DoY > 193)
#2019
EddyData_2019_p1 <- fLoadTXTIntoDataframe("ec_19_p12")
#2020
EddyData_2020_p1 <- fLoadTXTIntoDataframe("ec_20_p12")
#2021
EddyData_2021_p1 <- fLoadTXTIntoDataframe("ec_21_p12")
#2022
EddyData_2022_p1 <- fLoadTXTIntoDataframe("ec_22_p12")
#2023
EddyData_2023_p1 <- fLoadTXTIntoDataframe("ec_23_p12")
#2024
EddyData_2024_p1 <- fLoadTXTIntoDataframe("ec_24_p12")

# loading data that contains EC and FG measurements

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2018")
EddyData_2018_p3 <- EddyData_2018_p3 %>%
  filter(DoY < 194) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1
EddyData_2018_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2018") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2018_p1 <- EddyData_2018_p1 %>%
  filter(DoY > 193)
#2019
EddyData_2019_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2019")
#2020
#EddyData_2020_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_p1_2020")
EddyData_2020_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2020")
#2021
EddyData_2021_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2021")
#2022
EddyData_2022_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2022")
#2023
EddyData_2023_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2023")
#2024
EddyData_2024_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2024")

# merging all years in a single dataset

EddyData_p1 <- rbind(EddyData_2018_p3, EddyData_2018_p1, EddyData_2019_p1, EddyData_2020_p1, EddyData_2021_p1, EddyData_2022_p1, EddyData_2023_p1,EddyData_2024_p1) 

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

##### plot 3-4 ####

# The workflow starts with importing the half-hourly data. 

# loading data that contains only EC measurements 

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("ec_18_p3") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2018_p3 <- EddyData_2018_p3 %>%
  filter(DoY > 120)
#2019
EddyData_2019_p3 <- fLoadTXTIntoDataframe("ec_19_p3")
#2020
EddyData_2020_p3 <- fLoadTXTIntoDataframe("ec_20_p3")
#2021
EddyData_2021_p3 <- fLoadTXTIntoDataframe("ec_21_p3")
#2022
EddyData_2022_p3 <- fLoadTXTIntoDataframe("ec_22_p3")
#2023
EddyData_2023_p3 <- fLoadTXTIntoDataframe("ec_23_p3")
#2024
EddyData_2024_p3 <- fLoadTXTIntoDataframe("ec_24_p3")

# loading data that contains EC and FG measurements

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2018")
#2019
EddyData_2019_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2019")
#2020
EddyData_2020_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2020")
#2021
EddyData_2021_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2021")
#2022
EddyData_2022_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2022")
#2023
EddyData_2023_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2023")
#2024
EddyData_2024_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2024")

# merging all years in a single dataset

EddyData_p3 <- rbind(EddyData_2018_p3, EddyData_2019_p3, EddyData_2020_p3, EddyData_2021_p3, EddyData_2022_p3, EddyData_2023_p3,EddyData_2024_p3) 

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
          NEE_raw = NEE, # I added this to count how many initial NEE values we had in the dataset.
          NEE = ifelse(NEE < -100, NA, NEE), # added based on discussion with Patrick and Claudia.
          NEE = ifelse(NEE > 60, NA, NEE), # added based on discussion with Patrick and Claudia.
          NEE_raw2 = NEE, # I added this to count how many initial NEE values we had in the dataset after filtering for the two brackets above.
          VPD = ifelse(VPD > 50, NA, VPD), # this was in the original code
          VPD = ifelse(VPD < 0, NA, VPD), # this was not in the original code from source 5, but REddyProc gave me a warning so I added it.
          Rg = pmin(Rg, 1200),
          Rg = pmax(Rg, 0),
          RH = ifelse(abs(RH) > 100, NA, RH), # I added this after realizing there were a few observations with values among 3000s and 6000s
          H = ifelse(H > 500 | H < -100, NA, H), # I noticed in 2024 there were a few H and LE values, so looking at the rest of the distribution I set these thresholds
          LE = ifelse(LE < -500, NA, LE), # I noticed in 2024 there were a few H and LE values, so looking at the rest of the distribution I set these thresholds
          #source = source        
          )

# Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")

# Add time stamp in POSIX time format
EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")

#EddyDataWithPosix$VPD <- fCalcVPDfromRHandTair(EddyDataWithPosix$RH, EddyDataWithPosix$Tair)

# initialize R5 reference class sEddyProc for post-processing of eddy data - change the name of the plot after "("
EProc <- sEddyProc$new('P1', EddyDataWithPosix, c('NEE','LE','H','Ustar','Tair','RH','VPD', 'Rg')) # this command creates an object of class "sEddyProc" with the data from EddyDataWithPosix, and verifies if there are any values outside of pre-defined ranges (via warning message; for NEE it uses values that are less than -50, and for VPD negative values); it is important to pay attention to these warnings, as they may signal issues with units, or the order of the observations in the dataset. 
# for nee it may show warnings for values with absolute number larger than 50, but we set it at 100 so we do not need to worry about that warning specifically.

#### additional data quality check steps ####

# check structure of the data
str(EProc$sDATA)

# statistical summary of the data
round(describe(EProc$sDATA[,-1]), 2)

# plots across multiple years and variables to check the units are consistent
EddyData_initial_explo <- EddyDataWithPosix %>% # converting data to long format (needed for plotting)
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

mgmt_dates  <- seasonStarts # bringing in the management dates to plot them as vertical lines in the graph
colnames(mgmt_dates) <- c("doy", "Year")

EddyData_initial_explo %>% # actual plot using ggplot
  #filter(Year %in% 2023:2024) %>% 
  #filter(variables == "NEE") %>% 
  ggplot(aes(x = DoY, y = values, colour = variables)) +
  geom_point(alpha = 0.5, size = 1) +
  facet_grid(vars(variables), vars(Year), scales = "free_y") +
  #facet_wrap(Year~., ncol = 2)+
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(data = mgmt_dates, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  geom_text(data = summary_stats,
            aes(x = Inf, y = Inf,
                label = paste0("Mean: ", round(mean_val, 1), "\nSD: ", round(sd_val, 1))),
            hjust = 1.1, vjust = 1.0,
            inherit.aes = FALSE,
            size = 3)+
  labs(
    title = "REddyProc input data E26 - Plot 3 (Half-hourly data)",
    subtitle = "Units: NEE (umolm-2s-1), LE (Wm-2), H (Wm-2), Ustar (ms-1), Tair (degC), RH (%), VPD (hPa), Rg (Wm-2)"
)

ggsave("basic_p1_filled.png",  width = 11, height = 7.5) # saving the figure as png to the working directory

EddyData %>% 
  group_by(Year) %>% 
  summarise(
    rg = mean(Rg, na.rm = TRUE),
    rh = mean(RH, na.rm = TRUE),
    tair = mean(Tair, na.rm = TRUE),
    vpd = mean(VPD, na.rm = TRUE)
  )

# plot for nee only (halfhourly), color-coded by source (EC or FG)

EddyData %>% 
  ggplot(aes(x = DoY, y = NEE, color = source))+
  geom_point(alpha = 0.3, size = 1)+
  scale_color_manual(values = c("#0072B2", "#009E73"))+
  facet_wrap(Year~., ncol=2)+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )+
  #geom_vline(data = mgmt_dates_p3, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  labs(title = "REddyProc input data E26 - Plot 3 -  NEE (Half-hourly)",
       subtitle = "Red lines represent beginning and end of year, plus planting and harvest")+
  ylab("NEE (umolm-2s-1)")

ggsave("basic_nee_halfhourly_p3_filled.png", width = 9, height = 8) # saving the figure as png to the working directory

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
  EddyDataWithPosix$DateTime-15*60, starts = seasonStarts) # in Daphnee's code there is no "-15*60; technically this is a strategy to avoid confusion with dates. Since data for each date is recorded from hour 0.5 to hour 24, hour 24 is technically the hour 00:00 of the next day. To avoid this confusio, authors in youtube video listed in references suggest shiftting the time stamp for 15 minutes.
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

# setting the location for the site (this is E26); I learned that the time zone varies depending if we are or not in daylight savings. In summer months it will be -4 and summer months -5

EProc$sSetLocationInfo(LatDeg = 43.64079, LongDeg = -80.41301, TimeZoneHour = -4)  

# other meteorological variables need to be gap-filled for partitioning of NEE between Re and GPP

EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sMDSGapFill('Rg', FillAll=FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning
EProc$sMDSGapFill('LE', FillAll=FALSE,  minNWarnRunLength = NA) # gap-fill now for future partitioning

# EProc$sFillVPDFromDew() # fill longer gaps still present in VPD_f

grep("^NEE.*_f$", colnames( EProc$sExportResults()), value = TRUE) # extract name of the variable that will be used
EProc$sPlotFingerprintY('NEE_uStar_f', Year = 2020) # plotting the gap-filled data for a specific year

# checking values that have Ustar lower than thresholds, but are not excluded for some reason; after looking at these graphs and discussing with Patrick, seems like most of the points that have low U* but are not excluded seem to be in line with the rest of the values. So the program just keeps them. There were just a few values that looked far off and might need to be manually deleted. 

#### partitioning the NEE data into gpp and respiration ####

# there are two methods for partitioning the data, day-time method and night-time method; per previous conversations with Patrick, I am applying the day-time method.

# night-time method (Reichstein 2005)

EProc$sMRFluxPartitionUStarScens()
#EProc$sApplyUStarScen(EProc$sMRFluxPartition)
grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE) # extract the name of the variables that will be used

# daytime method (Lasslop, 2010)

EProc$sGLFluxPartitionUStarScens()
#EProc$sApplyUStarScen(EProc$sGLFluxPartition)
grep("GPP_DT_.*_f$|Reco", names(EProc$sExportResults()), value = TRUE)

# exporting the data

FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData) # combining this files helps bring back the doy 
CombinedData$doy <- CombinedData$DoY
CombinedData <- CombinedData %>%
  # filter(!c(Year == 2018 & doy < 121)) %>%
  # filter(!c(Year == 2024 & doy > 120)) %>%
  mutate(
    crop_year = ifelse(Year == 2018 & doy > 120 | Year == 2019 & doy < 121, "18/19", 
                       ifelse(Year == 2019 & doy > 120 | Year == 2020 & doy < 121, "19/20",
                              ifelse(Year == 2020 & doy > 120 | Year == 2021 & doy < 121, "20/21",
                                     ifelse(Year == 2021 & doy > 120 | Year == 2022 & doy < 121, "21/22",
                                            ifelse(Year == 2022 & doy > 120 | Year == 2023 & doy < 121, "22/23",
                                                   ifelse(Year == 2023 & doy > 120 | Year == 2024 & doy < 121, "23/24", NA))))))

  )

CombinedData %>% 
  write_xlsx(path = "conv_output_reddyproc_ec&fg.xlsx")


# CombinedData_p12_ec = CombinedData
# CombinedData_p34_ec = CombinedData
CombinedData_p12_ec_fg 
CombinedData_p34_ec_fg 

CombinedData_p12_ec_fg$

# exporting datasets

conv_factor <- 12.011 / 1e6 * 1800

# comparing partitioning

# gpp

CombinedData_p12_ec_fg %>%
  filter(!is.na(crop_year)) %>% 
  ggplot(aes(x = GPP_U50_f, y = GPP_DT_U50)) +
  geom_point(aes(fill = crop_year), shape = 21, alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +  
  xlab("DT-GPP (Umol m-2 s-1)")+
  ylab("NT-GPP (Umol m-2 s-1)")+
  theme_bw()

ggsave("nt_dt_p12.png", plot = last_plot(), width = 5, height = 3.5)

  
summary(lm(data=CombinedData_p12_ec_fg, GPP_DT_U50~GPP_U50_f))


CombinedData_p34_ec_fg %>%
  filter(!is.na(crop_year)) %>% 
  ggplot(aes(x = GPP_U50_f, y = GPP_DT_U50)) +
  geom_point(aes(fill = crop_year), shape = 21, alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +  
  xlab("DT-GPP (Umol m-2 s-1")+
  ylab("NT-GPP (Umol m-2 s-1")+
  theme_bw()

ggsave("nt_dt_p34.png", plot = last_plot(), width = 5, height = 3.5)
summary(lm(data=CombinedData_p34_ec_fg, GPP_DT_U50~GPP_U50_f))


# reco

CombinedData_p12_ec_fg %>%
  filter(!is.na(crop_year)) %>% 
  ggplot(aes(x = Reco_U50, y = Reco_DT_U50)) +
  geom_point(aes(fill = crop_year), shape = 21, alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +  
  xlab("DT-Reco (Umol m-2 s-1)")+
  ylab("NT-Reco (Umol m-2 s-1)")+
  theme_bw()

ggsave("nt_dt_p12_reco.png", plot = last_plot(), width = 5, height = 3.5)

summary(lm(data=CombinedData_p12_ec_fg, Reco_DT_U50~Reco_U50))

CombinedData_p34_ec_fg %>%
  filter(!is.na(crop_year)) %>% 
  ggplot(aes(x = Reco_U50, y = Reco_DT_U50)) +
  geom_point(aes(fill = crop_year), shape = 21, alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +  
  xlab("DT-Reco (Umol m-2 s-1")+
  ylab("NT-Reco (Umol m-2 s-1")+
  theme_bw()

ggsave("nt_dt_p34_reco.png", plot = last_plot(), width = 5, height = 3.5)
summary(lm(data=CombinedData_p34_ec_fg, Reco_DT_U50~Reco_U50))

#  fluxes EC

?geom_smooth


#p12 

CombinedData_p12_ec_fg %>%
  mutate(
    NEE_gC = NEE_U50_f * conv_factor,
    
    GPP_U50_f_gC = GPP_U50_f * conv_factor,    # Nighttime method
    Reco_U50_gC = Reco_U50 * conv_factor,      # Nighttime method
    
    GPP_DT_U50_gC = GPP_DT_U50 * conv_factor,  # Daytime method
    Reco_DT_U50_gC = Reco_DT_U50 * conv_factor, # Daytime method
    year = Year
    ) %>%
  group_by(crop_year, doy, Year) %>%
  summarise(
    nee_g_c_m2_day = sum(NEE_gC, na.rm = TRUE),
    gpp_nt_g_c_m2_day = sum(GPP_U50_f_gC, na.rm = TRUE),
    reco_nt_g_c_m2_day = sum(Reco_U50_gC, na.rm = TRUE),
    gpp_dt_g_c_m2_day = sum(GPP_DT_U50_gC, na.rm = TRUE),
    reco_dt_g_c_m2_day = sum(Reco_DT_U50_gC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  write_xlsx(path = "obs_co2_p12.xlsx")

#p34

CombinedData_p34_ec_fg %>%
  mutate(
    NEE_gC = NEE_U50_f * conv_factor,
    
    GPP_U50_f_gC = GPP_U50_f * conv_factor,    # Nighttime method
    Reco_U50_gC = Reco_U50 * conv_factor,      # Nighttime method
    
    GPP_DT_U50_gC = GPP_DT_U50 * conv_factor,  # Daytime method
    Reco_DT_U50_gC = Reco_DT_U50 * conv_factor, # Daytime method
    
    year = Year
  ) %>%
  group_by(crop_year, doy, year) %>%
  summarise(
    nee_g_c_m2_day = sum(NEE_gC, na.rm = TRUE),
    gpp_nt_g_c_m2_day = sum(GPP_U50_f_gC, na.rm = TRUE),
    reco_nt_g_c_m2_day = sum(Reco_U50_gC, na.rm = TRUE),
    gpp_dt_g_c_m2_day = sum(GPP_DT_U50_gC, na.rm = TRUE),
    reco_dt_g_c_m2_day = sum(Reco_DT_U50_gC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  write_xlsx(path = "obs_co2_p34.xlsx")

# important variables in the CombineData dataset

# a good description of the variables can be found here: https://www.bgc-jena.mpg.de/5624929/Output-Format

# NEE = original NEE value (raw halfhourly data)
# Ustar = original Ustar value
# Ustar_U50_Tresh = median for the distribution of Ustar tresholds used for each season
# NEE_U50_orig = NEE value after ustar filtering considering the median for the distribution of Ustar tresholds used for each season
# NEE_U50_f = filled NEE value after ustar filtering considering the median for the distribution of Ustar tresholds used for each season

# after partitioning

# GPP_U50_f = gpp value obtained with night-time partitioning, considering the median of the distribution of u* threshold ; nightime method
# Reco_U50 = reco value obtained with night-time partitioning, considering the median of the distribution of u* threshold ; nightime method
# GPP_DT_U50 = GPP value obtained with day-time partitioning, considering the median of the distribution of u* threshold : daytime method
# Reco_DT_U50 = Reco value obtained with day-time partitioning, considering the median of the distribution of u* threshold : daytime method
# NEE_U50_f = NEE value after filtering and gap-filling, considering the median of the distribution of u* threshold for different seasons

# GPP_uStar_f : nightime method.
# Reco_uStar : nightime method.

# GPP_DT_uStar : daytime method.
# Reco_DT_uStar : daytime method.

# final values to use:
  
    # these are the variables that Sara Knox's code uses for final output reporting. Checking the difference among multiple output variables from REddyProc for NEE, it looks like in general there is not much difference between NEE_U50_f and NEE_uStart_f; I was initially thinking of using NEE_U50_f , but then I saw in Sara Knox’s code that she uses NEE_U50_f for some of her outputs. Annual aggregated values do not seem to show large differences between those two variables.Annual aggregated values do not seem to show large differences between those two variables.
    
    # NEE_uStar_f
    
    # GPP_uStar_f : nightime partitioning method.
    # Reco_uStar : nightime partitioning method.
    
    # GPP_DT_uStar : daytime partitioning method.
    # Reco_DT_uStar : daytime partitioning method.
    
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

# after speaking with Sarah Knox confirmed that random error associated with measurement is mostly related with NEE and in general not associated with GPP and Reco. Since GPP and Reco are not measured, there is not much point in estimating random error for them.

# also after the meeting with Sarah Knox, I added to this code the lines to estimate also random error associated with daily values, following a similar approach used for annual ones.

year_of_interest <- "23/24" # added by agustin, as I am running one year at a time for now.

# REddyProc flags filled data with poor gap-filling by a quality flag in NEE_<uStar>_fqc > 0 but still reports the fluxes. For aggregation we recommend computing the mean including those gap-filled records, i.e. using NEE_<uStar>_f instead of NEE_orig. However, for estimating the uncertainty of the aggregated value, the the gap-filled records should not contribute to the reduction of uncertainty due to more replicates.

# Hence, first we create a column similar NEE_orig_sd to NEE_<uStar>_fsd but where the estimated uncertainty is set to missing for the gap-filled records.

results <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     
    #, DoY = as.POSIXlt(DateTime - 15*60)$yday # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
  ) %>% 
  filter(crop_year == year_of_interest) %>%  
  mutate(
    NEE_orig_sd = ifelse(
      is.finite(NEE_U50_orig), NEE_U50_fsd, NA), # NEE_orig_sd includes NEE_uStar_fsd only for measured values
    NEE_uStar_fgood = ifelse(
      NEE_uStar_fqc <= 1, NEE_U50_f, NA), # Only include filled values for the most reliable gap-filled observations. Note that is.finite() shouldn't be used here.
    resid = ifelse(NEE_U50_fqc == 0, NEE_U50_orig - NEE_U50_fall, NA) # quantify the error terms, i.e. data-model residuals (only using observations (i.e., NEE_uStar_fqc == 0 is original data) and exclude also
    
    
    # NEE_orig_sd = ifelse(
    #   is.finite(.data$NEE_uStar_orig), .data$NEE_uStar_fsd, NA),
    # NEE_uStar_fgood = ifelse(
    #   is.finite(.data$NEE_uStar_fqc) & (.data$NEE_uStar_fqc <= 1), .data$NEE_uStar_f, NA),
    # resid = ifelse(NEE_uStar_fqc == 0, NEE_uStar_orig - NEE_uStar_fall, NA)
  )

# checking the data

# plot_ly(data = results, x = ~DateTime, y = ~NEE_U05_orig, name = 'U05', type = 'scatter', mode = 'markers',marker = list(size = 3)) %>%
#   add_trace(data = results, x = ~DateTime, y =~NEE_uStar_orig, name = 'uStar', mode = 'markers') %>% 
#   add_trace(data = results, x = ~DateTime, y =~NEE_U95_orig, name = 'U95', mode = 'markers') %>% 
#   toWebGL()

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
  , NEEagg = mean(NEE_U50_f, na.rm = TRUE)
  , varMean = sum(NEE_orig_sd^2, na.rm = TRUE) / nRec / (!!nEff - 1)
  , seMean = sqrt(varMean) 
  #, seMean2 = sqrt(mean(NEE_orig_sd^2, na.rm = TRUE)) / sqrt(!!nEff - 1)
  , seMeanApprox = mean(NEE_orig_sd, na.rm = TRUE) / sqrt(!!nEff - 1)
) %>% select(NEEagg, seMean, seMeanApprox)
resRand

# The aggregated value is the same as when not considering the correlations, but its uncertainty increased compared to the computation neglecting correlations.
# Note, how we used NEE_uStar_f for computing the mean, but NEE_orig_sd instead of NEE_uStar_fsd for computing the uncertainty.


##### daily aggregation (this is not presented in Sara Knox's code, I brought it from the other source I considered;) ###
# also in this section there are multiple pieces of code associated with estimation of daily GPP and Reco U* uncertainty. The code original described by Sarah Knox on how to do that for annual values, was adapted to daily estimations.

# When aggregating daily respiration, the same principles hold.
# However, when computing the number of effective observations, we recommend using the empirical autocorrelation function estimated on longer time series of residuals (autoCorr computed above) in computeEffectiveNumObs instead of estimating them from the residuals of each day.

# First, create a column DoY to subset records of each day.

# already done

# Now the aggregation can be done on data grouped by DoY. The notation !! tells summarise to use the variable autoCorr instead of a column with that name.

aggDay <- results %>%
  group_by(Year, DoY) %>%
  summarise(
    DateTime = first(DateTime),
    nEff = lognorm::computeEffectiveNumObs(
      resid, effAcf = !!autoCorr, na.rm = TRUE),
    nRec = sum(is.finite(NEE_orig_sd)),
    NEE = mean(NEE_U50_f, na.rm = TRUE),
    sdNEE = if (nEff <= 1) NA_real_ else sqrt(
      mean(NEE_orig_sd^2, na.rm = TRUE) / (nEff - 1)),
    sdNEEuncorr = if (nRec <= 1) NA_real_ else sqrt(
      mean(NEE_orig_sd^2, na.rm = TRUE) / (nRec - 1)),
    .groups = "drop_last"
  )

# aggDay %>%
#   ggplot(aes(x = DoY)) +
#   geom_ribbon(aes(ymin = NEE - sdNEE, ymax = NEE + sdNEE), fill = "skyblue", alpha = 0.75) +
#   geom_line(aes(y = NEE), color = "blue", size = 1) +
#   labs(
#     title = "Daily NEE with Uncertainty - 21/22",
#     x = "Date",
#     y = expression(NEE~(µmol~CO[2]~m^{-2}~s^{-1}))
#   ) +
#   facet_wrap(Year~., nrow=2)+
#   theme_bw()
# 
# # g C
# aggDay$nee_g_c_m2_day <- aggDay$NEE*1.0368
# aggDay$sd_nee_g_c_m2_day <- aggDay$sdNEE*1.0368
# 
# #mean_sd_daily_gC_all <- data.frame()
# 
# # summary all years
# mean_sd_daily_gC_all <- rbind(mean_sd_daily_gC_all, aggDay)
# mean_sd_daily_gC_all

# estimate u* treshold uncertainty for daily NEE means (this code was adapted from the code that Sarah Knox put together for annual estimations after I met with her and she suggested this idea)

computeDailyMeanNEE <- function(ds, suffix){
  column_name <- paste0("NEE_", suffix, "_f")
  ds %>%
    group_by(DoY) %>%
    summarise(daily_mean = mean(.data[[column_name]], na.rm = TRUE)) %>%
    ungroup()
}

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     
    #, DoY = as.POSIXlt(DateTime - 15*60)$yday # this was causing an issue with doy's so I decided to comment it out.
  ) %>% 
  filter(crop_year == year_of_interest) 

daily_means_list <- EProc$sApplyUStarScen(computeDailyMeanNEE, FilledEddyData)

# Add a name for each scenario (from names of the list)
names(daily_means_list) <- names(daily_means_list)
# Convert to wide format with DoY as common column
daily_wide <- reduce(daily_means_list, full_join, by = "DoY")
colnames(daily_wide) <- c("DoY", paste0("NEE_", names(daily_means_list)))

daily_summary <- daily_wide %>%
  rowwise() %>%
  mutate(
    NEE_mean = mean(c_across(starts_with("NEE_")), na.rm = TRUE),
    NEE_sd = sd(c_across(starts_with("NEE_")), na.rm = TRUE)
  ) %>%
  ungroup()

# combine u* treshold uncertainty and random uncertainty

# Join the two tables by DoY
daily_uncertainty <- aggDay %>%
  select(Year, DoY, DateTime, NEE, sdNEE) %>%
  left_join(daily_summary %>% select(DoY, sdUstar = NEE_sd), by = "DoY")

# Combine uncertainties
daily_uncertainty <- daily_uncertainty %>%
  mutate(
    sdComb = sqrt(sdNEE^2 + sdUstar^2)
  )

# Convert to g C m-2 day-1
daily_uncertainty <- daily_uncertainty %>%
  mutate(
    nee_g_c_m2_day = NEE * 1.0368,
    sd_rand_g_c = sdNEE * 1.0368,
    sd_ustar_g_c = sdUstar * 1.0368,
    sd_comb_g_c = sdComb * 1.0368
  )

# store data

#mean_sd_daily_gC_all_nee <- data.frame()

# summary all years
mean_sd_daily_gC_all_nee <- rbind(mean_sd_daily_gC_all_nee, daily_uncertainty)
mean_sd_daily_gC_all_nee

# export
# mean_sd_daily_gC_all_nee %>%
#    write_xlsx(path = "mean_sd_daily_gC_all_nee_conv.xlsx")

# export
# mean_sd_daily_gC_all_nee %>%
#    write_xlsx(path = "mean_sd_daily_gC_all_nee_div.xlsx")

# plot daily means with uncertainties

# uncertainty combined

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = nee_g_c_m2_day  - sd_comb_g_c, ymax = nee_g_c_m2_day  + sd_comb_g_c), fill = "skyblue", alpha = 0.75) +
  geom_line(aes(y = nee_g_c_m2_day), color = "blue", size = 1) +
  labs(
    title = "Daily NEE with random and u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# random uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = nee_g_c_m2_day  - sd_rand_g_c , ymax = nee_g_c_m2_day  + sd_rand_g_c ), fill = "pink", alpha = 0.75) +
  geom_line(aes(y = nee_g_c_m2_day), color = "red", size = 1) +
  labs(
    title = "Daily NEE with random and u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# u* uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = nee_g_c_m2_day  - sd_ustar_g_c  , ymax = nee_g_c_m2_day  + sd_ustar_g_c  ), fill = "lightgreen", alpha = 0.75) +
  geom_line(aes(y = nee_g_c_m2_day), color = "darkgreen", size = 1) +
  labs(
    title = "Daily NEE with random and u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# estimate u* treshold uncertainty for daily GPP means with nighttime partitioning

computeDailyMeanGPP <- function(ds, suffix){
  column_name <- paste0("GPP_", suffix, "_f")
  ds %>%
    #mutate(DoY = as.POSIXlt(DateTime - 15*60)$yday) %>% # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
    group_by(DoY) %>%
    summarise(daily_mean = mean(.data[[column_name]], na.rm = TRUE)) %>%
    ungroup()
}

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
   # , DoY = as.POSIXlt(DateTime - 15*60)$yday # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
    
  ) %>% 
  filter(crop_year == year_of_interest) 

daily_means_list <- EProc$sApplyUStarScen(computeDailyMeanGPP, FilledEddyData)

# Add a name for each scenario (from names of the list)
names(daily_means_list) <- names(daily_means_list)
# Convert to wide format with DoY as common column
daily_wide <- reduce(daily_means_list, full_join, by = "DoY")
colnames(daily_wide) <- c("DoY", paste0("GPP_", names(daily_means_list)))

daily_summary <- daily_wide %>%
  rowwise() %>%
  mutate(
    GPP_mean = mean(c_across(starts_with("GPP_")), na.rm = TRUE),
    GPP_sd = sd(c_across(starts_with("GPP_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to g C m-2 day-1
daily_uncertainty <- daily_summary %>%
  mutate(
    gpp_mean_g_c_m2_day_nt = GPP_mean  * 1.0368,
    gpp_sd_ustar_g_c_nt = GPP_sd * 1.0368
  )

# Join the two tables by DoY
daily_uncertainty <- aggDay %>%
  select(Year, DoY, DateTime) %>%
  left_join(daily_uncertainty, by = "DoY")

# store data

#mean_sd_daily_gC_all_gpp_nt <- data.frame()

# summary all years
mean_sd_daily_gC_all_gpp_nt <- rbind(mean_sd_daily_gC_all_gpp_nt, daily_uncertainty)
mean_sd_daily_gC_all_gpp_nt

# export
# mean_sd_daily_gC_all_gpp_nt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_gpp_nt_conv.xlsx")

# export
# mean_sd_daily_gC_all_gpp_nt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_gpp_nt_div.xlsx")

# plot daily means with uncertainties

# u* uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = gpp_mean_g_c_m2_day_nt  - gpp_sd_ustar_g_c_nt  , ymax = gpp_mean_g_c_m2_day_nt  + gpp_sd_ustar_g_c_nt  ), fill = "lightgreen", alpha = 0.75) +
  geom_line(aes(y = gpp_mean_g_c_m2_day_nt), color = "darkgreen", size = 1) +
  labs(
    title = "Daily GPP (NT) with u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()


# estimate u* treshold uncertainty for daily RECO means with nighttime partitioning

computeDailyMeanRECO <- function(ds, suffix){
  column_name <- paste0("Reco_", suffix)
  ds %>%
   # mutate(DoY = as.POSIXlt(DateTime - 15*60)$yday) %>% # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
    group_by(DoY) %>%
    summarise(daily_mean = mean(.data[[column_name]], na.rm = TRUE)) %>%
    ungroup()
}

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    #, DoY = as.POSIXlt(DateTime - 15*60)$yday # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
  ) %>% 
  filter(crop_year == year_of_interest) 

daily_means_list <- EProc$sApplyUStarScen(computeDailyMeanRECO, FilledEddyData)

# Add a name for each scenario (from names of the list)
names(daily_means_list) <- names(daily_means_list)
# Convert to wide format with DoY as common column
daily_wide <- reduce(daily_means_list, full_join, by = "DoY")
colnames(daily_wide) <- c("DoY", paste0("Reco_", names(daily_means_list)))

daily_summary <- daily_wide %>%
  rowwise() %>%
  mutate(
    Reco_mean = mean(c_across(starts_with("Reco_")), na.rm = TRUE),
    Reco_sd = sd(c_across(starts_with("Reco_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to g C m-2 day-1
daily_uncertainty <- daily_summary %>%
  mutate(
    reco_mean_g_c_m2_day_nt = Reco_mean  * 1.0368,
    reco_sd_ustar_g_c_nt = Reco_sd * 1.0368
  )

# Join the two tables by DoY
daily_uncertainty <- aggDay %>%
  select(Year, DoY, DateTime) %>%
  left_join(daily_uncertainty, by = "DoY")

# store data

#mean_sd_daily_gC_all_reco_nt <- data.frame()

# summary all years
mean_sd_daily_gC_all_reco_nt <- rbind(mean_sd_daily_gC_all_reco_nt, daily_uncertainty)
mean_sd_daily_gC_all_reco_nt

# export
# mean_sd_daily_gC_all_reco_nt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_reco_nt_conv.xlsx")

# export
# mean_sd_daily_gC_all_reco_nt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_reco_nt_div.xlsx")

# plot daily means with uncertainties

# u* uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt  , ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt  ), fill = "lightgreen", alpha = 0.75) +
  geom_line(aes(y = reco_mean_g_c_m2_day_nt), color = "darkgreen", size = 1) +
  labs(
    title = "Daily RECO (NT) with u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# estimate u* treshold uncertainty for daily GPP means with DAYTIME partitioning

computeDailyMeanGPP <- function(ds, suffix){
  column_name <- paste0("GPP_DT_", suffix)
  ds %>%
    #mutate(DoY = as.POSIXlt(DateTime - 15*60)$yday) %>% # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
    group_by(DoY) %>%
    summarise(daily_mean = mean(.data[[column_name]], na.rm = TRUE)) %>%
    ungroup()
}

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    #, DoY = as.POSIXlt(DateTime - 15*60)$yday # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
  ) %>% 
  filter(crop_year == year_of_interest) 

daily_means_list <- EProc$sApplyUStarScen(computeDailyMeanGPP, FilledEddyData)

# Add a name for each scenario (from names of the list)
names(daily_means_list) <- names(daily_means_list)
# Convert to wide format with DoY as common column
daily_wide <- reduce(daily_means_list, full_join, by = "DoY")
colnames(daily_wide) <- c("DoY", paste0("GPP_DT_", names(daily_means_list)))

daily_summary <- daily_wide %>%
  rowwise() %>%
  mutate(
    GPP_DT_mean = mean(c_across(starts_with("GPP_DT_")), na.rm = TRUE),
    GPP_DT_sd = sd(c_across(starts_with("GPP_DT_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to g C m-2 day-1
daily_uncertainty <- daily_summary %>%
  mutate(
    gpp_mean_g_c_m2_day_dt = GPP_DT_mean  * 1.0368,
    gpp_sd_ustar_g_c_dt = GPP_DT_sd * 1.0368
  )

# Join the two tables by DoY
daily_uncertainty <- aggDay %>%
  select(Year, DoY, DateTime) %>%
  left_join(daily_uncertainty, by = "DoY")

# store data

#mean_sd_daily_gC_all_gpp_dt <- data.frame()

# summary all years
mean_sd_daily_gC_all_gpp_dt <- rbind(mean_sd_daily_gC_all_gpp_dt, daily_uncertainty)
mean_sd_daily_gC_all_gpp_dt

# export
# mean_sd_daily_gC_all_gpp_dt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_gpp_dt_conv.xlsx")

# export
# mean_sd_daily_gC_all_gpp_dt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_gpp_dt_div.xlsx")

# plot daily means with uncertainties

# u* uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = gpp_mean_g_c_m2_day_dt  - gpp_sd_ustar_g_c_dt  , ymax = gpp_mean_g_c_m2_day_dt  + gpp_sd_ustar_g_c_dt  ), fill = "lightgreen", alpha = 0.75) +
  geom_line(aes(y = gpp_mean_g_c_m2_day_dt), color = "darkgreen", size = 1) +
  labs(
    title = "Daily GPP (DT) with u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# estimate u* treshold uncertainty for daily RECO means with daytime partitioning

computeDailyMeanRECO <- function(ds, suffix){
  column_name <- paste0("Reco_DT_", suffix)
  ds %>%
    #mutate(DoY = as.POSIXlt(DateTime - 15*60)$yday) %>% # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
    group_by(DoY) %>%
    summarise(daily_mean = mean(.data[[column_name]], na.rm = TRUE)) %>%
    ungroup()
}

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    #, DoY = as.POSIXlt(DateTime - 15*60)$yday # I realized this was causing an issue (doy 365 was not created or was override), so I commented out and this seemed to fix the issue
  ) %>% 
  filter(crop_year == year_of_interest) 

daily_means_list <- EProc$sApplyUStarScen(computeDailyMeanRECO, FilledEddyData)

# Add a name for each scenario (from names of the list)
names(daily_means_list) <- names(daily_means_list)
# Convert to wide format with DoY as common column
daily_wide <- reduce(daily_means_list, full_join, by = "DoY")
colnames(daily_wide) <- c("DoY", paste0("Reco_DT_", names(daily_means_list)))

daily_summary <- daily_wide %>%
  rowwise() %>%
  mutate(
    Reco_DT_mean = mean(c_across(starts_with("Reco_DT_")), na.rm = TRUE),
    Reco_DT_sd = sd(c_across(starts_with("Reco_DT_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to g C m-2 day-1
daily_uncertainty <- daily_summary %>%
  mutate(
    reco_mean_g_c_m2_day_dt = Reco_DT_mean  * 1.0368,
    reco_sd_ustar_g_c_dt = Reco_DT_sd * 1.0368
  )

# Join the two tables by DoY
daily_uncertainty <- aggDay %>%
  select(Year, DoY, DateTime) %>%
  left_join(daily_uncertainty, by = "DoY")

# store data

#mean_sd_daily_gC_all_reco_dt <- data.frame()

# summary all years
mean_sd_daily_gC_all_reco_dt <- rbind(mean_sd_daily_gC_all_reco_dt, daily_uncertainty)
mean_sd_daily_gC_all_reco_dt

# export
# mean_sd_daily_gC_all_reco_dt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_reco_dt_conv.xlsx")

# export
# mean_sd_daily_gC_all_reco_dt %>%
#   write_xlsx(path = "mean_sd_daily_gC_all_reco_dt_div.xlsx")

# plot daily means with uncertainties

# u* uncertainty

daily_uncertainty %>%
  ggplot(aes(x = DoY)) +
  geom_ribbon(aes(ymin = reco_mean_g_c_m2_day_dt  - reco_sd_ustar_g_c_dt  , ymax = reco_mean_g_c_m2_day_dt  + reco_sd_ustar_g_c_dt  ), fill = "lightgreen", alpha = 0.75) +
  geom_line(aes(y = reco_mean_g_c_m2_day_dt), color = "darkgreen", size = 1) +
  labs(
    title = "Daily RECO (DT) with u* uncertainty",
    x = "Date",
    y = "CO2 (g C/m2/day)"
  ) +
  facet_wrap(Year~., nrow=2)+
  theme_bw()

# u* threshold uncertainty for annual estimations

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

FilledEddyData <- CombinedData %>%
  mutate(
    DateTime = EddyDataWithPosix$DateTime     # take time stamp form input data
    , DoY = as.POSIXlt(DateTime - 15*60)$yday # midnight belongs to the previous
  ) %>% 
  filter(crop_year == year_of_interest) 
  
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

data.mean_NEE_U50_f <- data.frame(mean(results$NEE_U50_f, na.rm = TRUE))
colnames(data.mean_NEE_U50_f) <- 'mean_NEE_U50_f'
NEE_sdAnnual <- cbind(data.mean_NEE_U50_f,sdAnnual)

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
sd_GPP_uStar_DT<- sd(GPPagg_DT)
sd_GPP_uStar_DT <- data.frame(sd_GPP_uStar_DT)

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
  mutate(mean_GPP_U50_f = mean(results$GPP_U50_f, na.rm = TRUE),
         sd_GPP_Ustar_NT = sd_GPP_Ustar_NT,
         mean_Reco_U50_f = mean(results$Reco_U50, na.rm = TRUE),
         sd_Reco_Ustar_NT = sd_Reco_Ustar_NT,
         mean_GPP_DT_U50 = mean(results$GPP_DT_U50, na.rm = TRUE),
         sd_GPP_uStar_DT = sd_GPP_uStar_DT,
         mean_Reco_DT_U50 = mean(results$Reco_DT_U50, na.rm = TRUE),
         sd_Reco_Ustar_DT = sd_Reco_Ustar_DT)
mean_sdAnnual

# other part

data.mean_NEE_U50_f <- data.frame(mean(results$NEE_U50_f, na.rm = TRUE))
colnames(data.mean_NEE_U50_f) <- 'mean_NEE_U50_f'
NEE_sdAnnual <- cbind(data.mean_NEE_U50_f,NEE_sdAnnual)

# Convert to annual sums
#conv_gCO2 <- 1/(10^6)*44.01*60*60*24*length(results$NEE_U50_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year # note from Agustin, I think this is because the original unit is in umolm-2s-1
conv_gC <- 1/(10^6)*12.011*60*60*24*length(results$NEE_U50_f)/48 # Converts umol to mol, mol to gCO2, x seconds in a year

# g CO2
# mean_sdAnnual_gCO2_all <- data.frame()

# mean_sdAnnual_gCO2 <- mean_sdAnnual*conv_gCO2
# mean_sdAnnual_gCO2
# 
# mean_sdAnnual_gCO2_all <- rbind(mean_sdAnnual_gCO2_all,mean_sdAnnual_gCO2)

# g C
#mean_sdAnnual_gC_all <- data.frame()

mean_sdAnnual_gC <- mean_sdAnnual*conv_gC
mean_sdAnnual_gC # final units in g C/m2/year


# ading year (change every time you run the code)

mean_sdAnnual_gC <- mean_sdAnnual_gC %>%
  mutate(crop_year = year_of_interest) %>%
  select(crop_year, everything())

# summary all years
mean_sdAnnual_gC_all <- rbind(mean_sdAnnual_gC_all,mean_sdAnnual_gC)
mean_sdAnnual_gC_all

# export
# mean_sdAnnual_gC_all %>%
#   write_xlsx(path = "mean_sdAnnual_gC_all_conv.xlsx")

# export
# mean_sdAnnual_gC_all %>%
#   write_xlsx(path = "mean_sdAnnual_gC_all_div.xlsx")


# plotting annual values with uncertainty

# nightime method

# Prepare the data for plotting

# Step 1: pivot mean values
mean_df <- mean_sdAnnual_gC_all %>%
  select(crop_year,
         mean_NEE_U50_f,
         mean_GPP_U50_f,
         mean_Reco_U50_f) %>%
  summarise(
    crop_year = crop_year,
    mean_NEE_U50_f = mean_NEE_U50_f,
    mean_GPP_U50_f = mean_GPP_U50_f*-1,
    mean_Reco_U50_f = mean_Reco_U50_f
  ) %>% 
  pivot_longer(
    cols = -crop_year,
    names_to = "metric",
    values_to = "mean"
  ) %>%
  mutate(metric = recode(metric,
                         "mean_NEE_U50_f" = "NEE",
                         "mean_GPP_U50_f" = "GPP",
                         "mean_Reco_U50_f" = "Reco"))

# Step 2: pivot sd values
sd_df <- mean_sdAnnual_gC_all %>%
  select(crop_year,
         sdComb,
         sd_GPP_Ustar_NT,
         sd_Reco_Ustar_NT) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "metric",
    values_to = "sd"
  ) %>%
  mutate(metric = recode(metric,
                         "sdComb" = "NEE",
                         "sd_GPP_Ustar_NT" = "GPP",
                         "sd_Reco_Ustar_NT" = "Reco"))

# Step 3: join mean and sd data
plot_data <- left_join(mean_df, sd_df, by = c("crop_year", "metric")) %>%
  mutate(metric = factor(metric, levels = c("GPP", "Reco", "NEE")))


ggplot(plot_data, aes(x = metric, y = mean, fill = metric)) +
  geom_col(colour = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge()) +
  geom_text(aes(label = round(mean, 0)), vjust = -1.1) +
  scale_fill_manual(values = c("#009E73","#D55E00", "#0072B2")) +
  facet_wrap(~ crop_year, nrow = 1) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )+
  labs(title = "Annual fluxes, NIGHT-TIME partitioning method - P12",
       subtitle = "Error bars = SD (NEE = random + u*, GPP = u*, Reco = u*)",
       y="Flux (g C m-2 y-1)")

       

# daytime method

# Prepare the data for plotting

# Step 1: pivot mean values
mean_df <- mean_sdAnnual_gC_all %>%
  select(crop_year,
         mean_NEE_U50_f,
         mean_GPP_DT_U50,
         mean_Reco_DT_U50 ) %>%
  summarise(
    crop_year = crop_year,
    mean_NEE_U50_f = mean_NEE_U50_f,
    mean_GPP_DT_U50 = mean_GPP_DT_U50*-1,
    mean_Reco_DT_U50  = mean_Reco_DT_U50 
  ) %>% 
  pivot_longer(
    cols = -crop_year,
    names_to = "metric",
    values_to = "mean"
  ) %>%
  mutate(metric = recode(metric,
                         "mean_NEE_U50_f" = "NEE",
                         "mean_GPP_DT_U50" = "GPP",
                         "mean_Reco_DT_U50" = "Reco"))

# Step 2: pivot sd values
sd_df <- mean_sdAnnual_gC_all %>%
  select(crop_year,
         sdComb,
         sd_GPP_Ustar_DT,
         sd_Reco_Ustar_DT ) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "metric",
    values_to = "sd"
  ) %>%
  mutate(metric = recode(metric,
                         "sdComb" = "NEE",
                         "sd_GPP_Ustar_DT" = "GPP",
                         "sd_Reco_Ustar_DT" = "Reco"))

# Step 3: join mean and sd data
plot_data <- left_join(mean_df, sd_df, by = c("crop_year", "metric")) %>%
  mutate(metric = factor(metric, levels = c("GPP", "Reco", "NEE")))


ggplot(plot_data, aes(x = metric, y = mean, fill = metric)) +
  geom_col(colour = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, position = position_dodge()) +
  geom_text(aes(label = round(mean, 0)), vjust = -0.8) +
  scale_fill_manual(values = c("#009E73","#D55E00", "#0072B2")) +
  facet_wrap(~ crop_year, nrow = 1) +
  theme_bw() +
  #ylab("Flux (g C m⁻² yr⁻¹)") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )+
  labs(title = "Annual fluxes, DAY-TIME partitioning method - P12",
       subtitle = "Error bars = SD (NEE = random + u*, GPP = u*, Reco = u*)",
       y="Flux (g C m-2 y-1)")


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
#################### graphs ###########################################

#

# plot for nee only (halfhourly) - EC + FG data color-coded

CombinedData %>% 
  ggplot(aes(x = DoY, y = NEE, color = source))+
  geom_point(alpha = 0.3, size = 1)+
  scale_color_manual(values = c("#0072B2", "#009E73"))+
  facet_wrap(Year~., ncol=2)+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )+
  geom_vline(data = mgmt_dates_p3, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  labs(title = "REddyProc input data E26 - Plot3 -  NEE (Half-hourly)",
       subtitle = "Red lines represent beginning and end of year, plus planting and harvest")+
  ylab("NEE (umolm-2s-1)")

ggsave("basic_nee_halfhourly_p3_filled.png", width = 9, height = 8) # saving the figure as png to the working directory

# some final raw variables

EddyData_basic_variables <- CombinedData %>% # converting data to long format (needed for plotting)
  pivot_longer(
    cols = c(LE, H, Ustar,Tair_f, RH, VPD_f, Rg_f, NEE_uStar_f),
    names_to = "variables",
    values_to = "values"
  )
summary_stats <- EddyData_basic_variables %>%
  group_by(variables, Year) %>%
  summarize(
    mean_val = mean(values, na.rm = TRUE),
    sd_val = sd(values, na.rm = TRUE),
    .groups = "drop"
  )

mgmt_dates_p1  <- seasonStarts # bringing in the management dates to plot them as vertical lines in the graph
colnames(mgmt_dates_p1) <- c("doy", "Year")

EddyData_basic_variables %>% # actual plot using ggplot
  #filter(variables == "Rg") %>% 
  filter(Year == 2023) %>%
  ggplot(aes(x = DoY, y = values, colour = variables)) +
  geom_point(alpha = 0.5, size = 1) +
  facet_grid(vars(variables), vars(Year), scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_vline(data = mgmt_dates_p1, aes(xintercept =doy) , color = "red", linetype = "dashed") +
  geom_text(data = filter(summary_stats, Year == 2023),
            aes(x = Inf, y = Inf,
                label = paste0("Mean: ", round(mean_val, 1), "\nSD: ", round(sd_val, 1))),
            hjust = 1.1, vjust = 1.0,
            inherit.aes = FALSE,
            size = 3)+
  labs(
    title = "REddyProc input data E26 - Plot1 (Half-hourly data)",
    subtitle = "Units: NEE (umolm-2s-1), LE (Wm-2), H (Wm-2), Ustar (ms-1), Tair (degC), RH (%), VPD (hPa), Rg (Wm-2)"
  )


# halfhourly observations

# crop year (doy 121 to doy 120)

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  mutate(
    doy_crop = case_when(
      doy >= 121 ~ doy - 120,         # e.g., doy 121 becomes 1, 122 becomes 2, ..., 365 becomes 245
      doy <= 120 ~ doy + (365 - 120)  # e.g., doy 1 becomes 246, 120 becomes 365
    )
  ) %>%
  pivot_longer(
    cols = c(NEE, NEE_uStar_f, NEE_uStar_orig),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(variables = factor(variables, levels = c("NEE", "NEE_uStar_orig", "NEE_uStar_f"))) %>%
  mutate(
    variables = recode(variables, 
                       "NEE_uStar_f" = "u*-filtered & gap-filled NEE",
                       "NEE_uStar_orig" = "u*-filtered NEE",
                       "NEE" = "NEE after initial filtering",
    )) %>% 
  ggplot(aes(x = doy_crop, y = value, colour = variables))+
  geom_point(alpha = 0.4, size = 0.9)+
  geom_vline(xintercept = 245, color = "red", linetype = "dashed")+ # DOY 365 in crop day terms
  facet_grid(vars(variables), vars(crop_year), )+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "none"
  )+
  labs(title = "NEE dataset at multiple stages of REddyProc processing - P34",
       #subtitle = "Red lines represent beginning and end of year, plus planting and harvest"
       )+ 
  scale_x_continuous(
    breaks = c(1, 80, 180, 245, 365),  # These are doy_crop values corresponding to DOY 121, 200, 300, 365
    labels = c("121", "200", "300", "365", "120")
       )

ggsave(filename = "rproc_p3_nee.png", width = 10, height = 6, dpi = 300)

# estimating the n 

# main one I used

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  #filter((doy > 130 & doy < 300)) %>%
  group_by(crop_year) %>% 
  summarise(
    NEE_n_raw = sum(!is.na(NEE_raw)),
    NEE_n_raw2 = sum(!is.na(NEE_raw2)),
    n_NEE_uStar_orig = sum(!is.na(NEE_U50_orig)),
    n_NEE_uStar_f = sum(!is.na(NEE_U50_f)),
    n_NEE_uStar_f_na = sum(is.na(NEE_U50_f))
  ) %>%
  group_by(crop_year) %>% 
  summarise(
    NEE_n_raw  = sum(NEE_n_raw),
    NEE_n_raw2  = sum(NEE_n_raw2),
    NEE_n_after_filtering = sum(n_NEE_uStar_orig),
    NEE_n_gap_filled = sum(n_NEE_uStar_f),
    NEE_n_remaining_na  = sum(n_NEE_uStar_f_na )
  )   

# other estimations of total number of observations

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  #filter((doy > 130 & doy < 300)) %>%
  group_by(crop_year) %>% 
  summarise(
    NEE_n_raw = sum(!is.na(NEE_raw)),
    NEE_n_in_filt = sum(!is.na(NEE)),
    NEE_n_U50_ustar_fil    = sum(!is.na(NEE_U50_orig)),
    n_NEE_U50_f = sum(!is.na(NEE_U50_f)),
    n_NEE_U50_f_na    = sum(is.na(NEE_U50_f)),
    .groups = "drop"
  ) %>%
  mutate(
    prop_raw_vs      = NEE_n_raw / n_NEE_U50_f*100,
    prop_nee_in_filt      = NEE_n_in_filt / n_NEE_U50_f*100,
    prop_U50_ustar_fil    = NEE_n_U50_ustar_fil / n_NEE_U50_f*100,
    prop_NEE_U50_f = n_NEE_U50_f / n_NEE_U50_f*100,
    prop_remaining_na_vs_final = n_NEE_U50_f_na / n_NEE_U50_f*100
  )


# crop year p12

mgmt_labels <- mgmt_dates_p1 %>%
  mutate(
    label = paste0("DOY = ", doy),
    y_pos = min(c(mod_co2_p1$mod_nee, obs_co2_p1$nee_g_c_m2_day), na.rm = TRUE) # slightly above max y
  )

custom_labels_facets <- c(
  "18/19" = "Year 1 (18-19 | Corn)",
  "19/20" = "Year 2 (19-20 | Soybean)",
  "20/21" = "Year 3 (20-21 | Soybean)",
  "21/22" = "Year 4 (21-22 | Corn)",
  "22/23" = "Year 5 (22-23 | Soybean)",
  "23/24" = "Year 6 (23-24 | Soybean)"
)

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>%
  filter(!(Year == 2024 & doy > 120)) %>%
  # create crop-day
  mutate(doy_crop = case_when(
    doy >= 121 ~ doy - 120,
    doy <= 120 ~ doy + (365 - 120)
  )) %>%
  # keep only the additional (gap-filled) points in NEE_U50_f
  mutate(
    NEE_U50_f_plot = ifelse(is.na(NEE_U50_orig), NEE_U50_f, NA)
  ) %>%
  pivot_longer(
    cols = c(NEE_U50_orig, NEE_U50_f_plot),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(
    variables = recode(variables,
                       "NEE_U50_orig"   = "Original NEE",
                       "NEE_U50_f_plot" = "Gap-filled NEE"   # <-- here!
    ),
    variables = factor(variables,
                       levels = c("Original NEE", "Gap-filled NEE")
    )
  ) %>%
  ggplot(aes(x = doy_crop, y = value, color = variables)) +
  geom_point( size = 0.9, shape = 21) +
  scale_color_manual(values = c("Gap-filled NEE" = "#F0E442", 
                                "Original NEE" = "#999999"))+
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  theme_bw()+
  ylab(expression(NEE~(mu*mol~m^-2~s^-1)))+
  xlab("Day of year")+
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  scale_y_continuous(limits = c(-100, 60), breaks = seq(-100, 60, by = 20))+
  scale_x_continuous(
    breaks = c(1, 80, 180, 245, 365),  # These are doy_crop values corresponding to DOY 121, 200, 300, 365
    labels = c("121", "200", "300", "365", "120")
  )+
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 1,     # Align text so it’s right over the line
    size = 3.0,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_vline(
    data = mgmt_dates_p1,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6,
    inherit.aes = FALSE
  ) +
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 1,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown",
    inherit.aes = FALSE
  )

ggsave(filename = "rproc_p12_nee.png", width = 8, height = 6, dpi = 300)


# crop year p3

mgmt_labels <- mgmt_dates_p3 %>%
  mutate(
    label = paste0("DOY = ", doy),
    y_pos = min(c(mod_co2_p3$mod_nee, obs_co2_p3$nee_g_c_m2_day), na.rm = TRUE) # slightly above max y
  )
custom_labels_facets <- c(
  "18/19" = "Year 1 (18-19 | Corn+2-CC)",
  "19/20" = "Year 2 (19-20 | Soybean+WW)",
  "20/21" = "Year 3 (20-21 | WW+4-CC)",
  "21/22" = "Year 4 (21-22 | Corn+2-CC)",
  "22/23" = "Year 5 (22-23 | Soybean+WW)",
  "23/24" = "Year 6 (23-24 | WW+4-CC)"
)

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>%
  filter(!(Year == 2024 & doy > 120)) %>%
  # create crop-day
  mutate(doy_crop = case_when(
    doy >= 121 ~ doy - 120,
    doy <= 120 ~ doy + (365 - 120)
  )) %>%
  # keep only the additional (gap-filled) points in NEE_U50_f
  mutate(
    NEE_U50_f_plot = ifelse(is.na(NEE_U50_orig), NEE_U50_f, NA)
  ) %>%
  pivot_longer(
    cols = c(NEE_U50_orig, NEE_U50_f_plot),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(
    variables = recode(variables,
                       "NEE_U50_orig"   = "Original NEE",
                       "NEE_U50_f_plot" = "Gap-filled NEE"   # <-- here!
    ),
    variables = factor(variables,
                       levels = c("Original NEE", "Gap-filled NEE")
    )
  ) %>%
  ggplot(aes(x = doy_crop, y = value, color = variables)) +
  geom_point( size = 0.9, shape = 21) +
  scale_color_manual(values = c("Gap-filled NEE" = "#F0E442", 
                                "Original NEE" = "#999999"))+
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  theme_bw()+
  ylab(expression(NEE~(mu*mol~m^-2~s^-1)))+
  xlab("Day of year")+
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  scale_y_continuous(limits = c(-100, 60), breaks = seq(-100, 60, by = 20))+
  scale_x_continuous(
    breaks = c(1, 80, 180, 245, 365),  # These are doy_crop values corresponding to DOY 121, 200, 300, 365
    labels = c("121", "200", "300", "365", "120")
  )+
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 1,     # Align text so it’s right over the line
    size = 3.0,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_vline(
    data = mgmt_dates_p3,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6,
    inherit.aes = FALSE
  ) +
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 1,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown",
    inherit.aes = FALSE
  )

ggsave(filename = "rproc_p3_nee.png", width = 8, height = 6, dpi = 300)

# calendar year

CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  # pivot_longer(
  #   cols = c(NEE, NEE_uStar_f, NEE_uStar_orig),
  #   names_to = "variables",
  #   values_to = "value"
  # ) %>%
  pivot_longer(
    cols = c(NEE_uStar_f, NEE_uStar_orig),
    names_to = "variables",
    values_to = "value"
  ) %>%
  mutate(variables = factor(variables, levels = c("NEE_uStar_orig", "NEE_uStar_f"))) %>%
  mutate(
    variables = recode(variables, 
                       "NEE_uStar_f" = "u*-filtered & gap-filled NEE",
                       "NEE_uStar_orig" = "u*-filtered NEE",
                       #"NEE" = "NEE after initial filtering",
    )) %>% 
  ggplot(aes(x = doy, y = value, colour = variables))+
  geom_line(alpha = 0.4, size = 0.9)+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  facet_wrap(crop_year~., ncol = 2)+
  scale_colour_manual(values = c("black", "yellow"))+
  #facet_grid(vars(variables), vars(Year))+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  theme(
    legend.position = "right"
  )+
  labs(title = "Halfhourly NEE dataset at multiple stages of REddyProc processing - P12",
       #subtitle = "Red lines represent beginning and end of year, plus planting and harvest"
  )

ggsave(filename = "rproc_p1_nee_filled_vs_nofilled_new.png", width = 10, height = 6, dpi = 300)

# annual values

# Conversion factor
conv_factor <- 12.011 / 1e6 * 1800

# --- Nighttime method ---
night_df <- CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  mutate(
    NEE_gC = NEE_U50_f * conv_factor,
    GPP_gC = GPP_U50_f * conv_factor,
    Reco_gC = Reco_U50 * conv_factor
  ) %>%
  # mutate(
  #   NEE_gC = NEE_uStar_f * conv_factor,
  #   GPP_gC = GPP_uStar_f * conv_factor,
  #   Reco_gC = Reco_uStar * conv_factor
  # ) %>%
  group_by(crop_year) %>%
  summarise(
    method = "Nighttime",
    NEE = sum(NEE_gC, na.rm = TRUE),
    GPP = -sum(GPP_gC, na.rm = TRUE),
    Reco = sum(Reco_gC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(NEE, GPP, Reco), names_to = "variable", values_to = "value")

# --- Daytime method ---
day_df <- CombinedData %>%
  filter(!(Year == 2018 & doy < 121)) %>% 
  filter(!(Year == 2024 & doy > 120)) %>%
  mutate(
    GPP_gC = GPP_DT_U50 * conv_factor,
    Reco_gC = Reco_DT_U50 * conv_factor,
    NEE_gC = -GPP_gC + Reco_gC
  ) %>%
  # mutate(
  #   GPP_gC = GPP_DT_uStar * conv_factor,
  #   Reco_gC = Reco_DT_uStar * conv_factor,
  #   NEE_gC = -GPP_gC + Reco_gC
  # ) %>%
  group_by(crop_year) %>%
  summarise(
    method = "Daytime",
    NEE = sum(NEE_gC, na.rm = TRUE),
    GPP = -sum(GPP_gC, na.rm = TRUE),
    Reco = sum(Reco_gC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(NEE, GPP, Reco), names_to = "variable", values_to = "value")

# --- Combine and format ---
combined_df <- bind_rows(night_df, day_df) %>%
  mutate(
    variable = factor(variable, levels = c("GPP", "Reco", "NEE")),
    method = factor(method, levels = c("Nighttime", "Daytime")),
    fill_color = case_when(
      variable %in% c("GPP", "Reco", "NEE") & method == "Nighttime" ~ "#0072B2",
      variable %in% c("GPP", "Reco", "NEE") & method == "Daytime" ~ "#E69F00"
    ),
    pattern = case_when(
      variable == "Reco" ~ "stripe",
      variable == "NEE"  ~ "circle",
      TRUE ~ "none"
    ),
    label_vjust = case_when(
      variable == "GPP" ~ 0.5,
      variable == "Reco" ~ 0.4,
      variable == "NEE" ~ 0.5,
      ),
    label_color = if_else(variable == "GPP", "white", "black")
  )

# --- Plot ---
ggplot(combined_df, aes(x = variable, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(preserve = "single"),
    colour = "black",               # Black border for the bars
    pattern_fill = "white",         # White-filled pattern shapes
    pattern_colour = NA,            # Remove black outline from patterns
    pattern_angle = 45,
    pattern_density = 0.4,         # Lower density = fewer, bigger patterns
    pattern_spacing = 0.05,          # Higher spacing = more room between patterns (bigger visible shapes)
    pattern_key_scale_factor = 0.1
  )+
  scale_fill_identity() +
  scale_pattern_identity() +
  geom_text(
    aes(label = round(value, 0), vjust = label_vjust, colour = label_color, hjust = -0.1),
    position = position_dodge2(width = 0.9),
    angle = 90,
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(-1800, 1800), breaks = seq(-1800, 1800, by = 200))+
  scale_color_identity() +
  facet_wrap(~ crop_year, nrow = 1) +
  ylab("C fluxes (g C m⁻² year⁻¹)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

ggsave("annual_sum_p3.png", width = 10, height = 5)


# incorporating error

sd_long <- mean_sdAnnual_gC_all %>%
  mutate(
    crop_year = crop_year
  ) %>%
  select(crop_year,
         sd_GPP_Ustar_NT, sd_Reco_Ustar_NT,
         sd_GPP_Ustar_DT, sd_Reco_Ustar_DT,
         sdComb) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "var_method",
    values_to = "sd"
  ) %>%
  mutate(
    variable = case_when(
      str_detect(var_method, "GPP") ~ "GPP",
      str_detect(var_method, "Reco") ~ "Reco",
      str_detect(var_method, "sdComb") ~ "NEE"
    ),
    method = case_when(
      str_detect(var_method, "_NT") ~ "Nighttime",
      str_detect(var_method, "_DT") ~ "Daytime",
      var_method == "sdComb" ~ "Nighttime"  # NEE sdComb applies to nighttime only
    )
  ) %>%
  select(crop_year, variable, method, sd)

plot_df <- combined_df %>%
  left_join(sd_long, by = c("crop_year", "variable", "method")) %>%
  mutate(
    ymin = value - sd,
    ymax = value + sd
  )
plot_df$variable <- factor(plot_df$variable, levels = c("GPP", "Reco", "NEE"))


# --- Plot ---
ggplot(plot_df, aes(x = variable, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.9),
    colour = "black",               # Black border for the bars
    pattern_fill = "white",         # White-filled pattern shapes
    pattern_colour = NA,            # Remove black outline from patterns
    pattern_angle = 45,
    pattern_density = 0.4,         # Lower density = fewer, bigger patterns
    pattern_spacing = 0.05,          # Higher spacing = more room between patterns (bigger visible shapes)
    pattern_key_scale_factor = 0.1
  )+
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge2(width = 0.9),
    width = 0.9,
    
    color = "black",
    na.rm = TRUE
  )+
  scale_fill_identity() +
  scale_pattern_identity() +
  geom_text(
    aes(label = round(value, 0), vjust = label_vjust, colour = label_color, hjust = -0.5),
    position = position_dodge2(width = 0.9),
    angle = 90,
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(-1800, 1850), breaks = seq(-1800, 1800, by = 200))+
  scale_color_identity() +
  facet_wrap(~ crop_year, nrow = 1) +
  ylab("NEE (g C m⁻² year⁻¹)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )+
  labs(title = "Annual fluxes P1/2",
       subtitle = "Error bars = SD (NEE = random + u*, GPP = u*, Reco = u*)")


ggsave("annual_sum_p1.png", width = 10, height = 6)


# night-time partitioning vs day-time partitioning - cumulative graph - crop year

CombinedData %>%
  filter(!(Year == 2018 & doy < 121), !(Year == 2024 & doy > 120)) %>%
  group_by(crop_year, DoY) %>%
  summarise(
    NEE_f = sum(NEE_U50_f * conv_factor),           # convert to g C m-2 d-1
    GPP_f = -sum(GPP_U50_f * conv_factor),          # nighttime method
    Reco = sum(Reco_U50 * conv_factor),             # nighttime method
    GPP_DT = -sum(GPP_DT_U50 * conv_factor),        # daytime method
    Reco_DT = sum(Reco_DT_U50 * conv_factor),       # daytime method
    NEE_DT = GPP_DT+Reco_DT,
    .groups = "drop"
  ) %>%
  group_by(crop_year) %>%
  mutate(
    cum_NEE = cumsum(NEE_f),
    cum_NEE_DT = cumsum(NEE_DT),
    cum_GPP_NT = cumsum(GPP_f),
    cum_GPP_DT = cumsum(GPP_DT),
    cum_Reco_DT = cumsum(Reco_DT),
    cum_Reco_NT = cumsum(Reco)
  ) %>%
  pivot_longer(cols = starts_with("cum_"), names_to = "Variable", values_to = "Cumulative_Sum") %>%
  ggplot(aes(x = DoY, y = Cumulative_Sum, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = c(
    "cum_GPP_NT" = "blue",
    "cum_GPP_DT" = "darkorange",
    "cum_Reco_NT" = "blue",
    "cum_Reco_DT" = "darkorange",
    "cum_NEE" = "blue",
    "cum_NEE_DT" = "darkorange"
  )) +
  scale_linetype_manual(values = c(
    "cum_GPP_NT" = "solid",
    "cum_GPP_DT" = "solid",
    "cum_Reco_NT" = "dashed",
    "cum_Reco_DT" = "dashed",
    "cum_NEE" = "dotted",
    "cum_NEE_DT" = "dotted"
  )) +
  labs(
    x = "Day of Year (DOY)",
    y = "Cumulative CO₂ Flux (g C m⁻² yr⁻¹)",
    title = "Cumulative CO₂ Fluxes Over the Crop Year",
    color = "Variable",
    linetype = "Variable"
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(~crop_year, nrow = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("ntvsdt_p12_new.png", width = 10, height = 5)

# night-time partitioning vs day-time partitioning - cumulative graph - calendar year

CombinedData %>%
  filter(!(Year == 2018 & doy < 121), !(Year == 2024 & doy > 120)) %>%
  group_by(Year, DoY) %>%
  summarise(
    NEE_f = sum(NEE_U50_f * conv_factor),           # convert to g C m-2 d-1
    GPP_f = -sum(GPP_U50_f * conv_factor),          # nighttime method
    Reco = sum(Reco_U50 * conv_factor),             # nighttime method
    GPP_DT = -sum(GPP_DT_U50 * conv_factor),        # daytime method
    Reco_DT = sum(Reco_DT_U50 * conv_factor),       # daytime method
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  mutate(
    cum_NEE = cumsum(NEE_f),
    cum_GPP_NT = cumsum(GPP_f),
    cum_GPP_DT = cumsum(GPP_DT),
    cum_Reco_DT = cumsum(Reco_DT),
    cum_Reco_NT = cumsum(Reco)
  ) %>%
  pivot_longer(cols = starts_with("cum_"), names_to = "Variable", values_to = "Cumulative_Sum") %>%
  ggplot(aes(x = DoY, y = Cumulative_Sum, color = Variable, linetype = Variable)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = c(
    "cum_GPP_NT" = "blue",
    "cum_GPP_DT" = "darkorange",
    "cum_Reco_NT" = "blue",
    "cum_Reco_DT" = "darkorange",
    "cum_NEE" = "darkgray"
  )) +
  scale_linetype_manual(values = c(
    "cum_GPP_NT" = "solid",
    "cum_GPP_DT" = "solid",
    "cum_Reco_NT" = "dashed",
    "cum_Reco_DT" = "dashed",
    "cum_NEE" = "solid"
  )) +
  labs(
    x = "Day of Year (DOY)",
    y = "Cumulative CO₂ Flux (g C m⁻² yr⁻¹)",
    title = "Cumulative CO₂ Fluxes Over the Crop Year",
    color = "Variable",
    linetype = "Variable"
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Year, nrow = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plotting the daily results and comparing other partitioning and gap-filling methods

# bring in the data Shannon separated for CO2 for 2023-2021 using Barr et al. (2004); this is what Nirmani used in her paper.

seasonStarts2 <- seasonStarts
colnames(seasonStarts2) <- c("doy", "year")

# bring in dataset from other partitioning and gap-filling method.
getwd()
new_co2 <- read.csv("obs_data/measured_co2/co2_jevans_folder.csv", header = FALSE)

colnames(new_co2) <- new_co2[2, ]
new_co2 <- new_co2[-c(1:3),]
new_co2$nee_obs <- as.numeric(new_co2$NEE)
new_co2$gpp_obs <- as.numeric(new_co2$GPP)
new_co2$resp_obs <- as.numeric(new_co2$Re)
new_co2$Year <- as.numeric(new_co2$year)
new_co2$source <- "co2_jevans_other"
new_co2 <- new_co2 %>%
  mutate(doy = as.numeric(doy)) %>% 
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )


check<-CombinedData %>% 
  filter(Year== 2021 & doy %in% 130:160) %>% 
  select(Year, doy, NEE, NEE_U50_f, NEE_U50_fqc, Reco_U50, Reco_DT_U50, Tair, Tair_f, VPD, VPD_f, Rg, Rg_f)

CombinedData %>% 
  filter(Year== 2021 & doy %in% 130:160) %>% 
  ggplot()+
  geom_line(aes(x = doy, y = Reco_U50))+
  theme_bw()

# CombinedData %>%
#   filter(Year==2021) %>%
#   filter(!GPP_U50_fqc >1) %>% 
#   ggplot()+
#   geom_point(aes(x = doy, y = NEE), color = "black", alpha=0.1)+
#   #geom_point(aes(x = doy, y = -GPP_U50_f), color = "blue", alpha=0.1)+
#   geom_point(aes(x = doy, y = Reco_U50), color = "red", alpha=0.1)
# 
# 
# check<-CombinedData %>%
#   filter(Year==2021 & doy %in% 120:150) %>% 
#   select(Year, doy, NEE, GPP_U50_f, GPP_U50_fqc, Reco_U50, Tair, Tair_f)

# creating a dataset with the reddyproc data 

filled <- CombinedData %>%
  #filter(NEE_U50_fqc<3) %>% 
  # mutate(
  # 
  #   NEE_gC = NEE_uStar_f * conv_factor,
  # 
  #   GPP_uStar_f_gC = GPP_uStar_f * conv_factor, # nighttime method
  #   Reco_uStar_gC = Reco_uStar * conv_factor, # nighttime method
  # 
  #   GPP_DT_uStar_gC = GPP_DT_uStar * conv_factor, # daytime method
  #   Reco_DT_uStar_gC = Reco_DT_uStar * conv_factor # daytime method
  # 
  # ) %>%
  mutate(
    
    NEE_gC = NEE_U50_f * conv_factor,

    GPP_U50_f_gC = GPP_U50_f * conv_factor, # nighttime method
    Reco_U50_gC = Reco_U50 * conv_factor, # nighttime method

    GPP_DT_U50_gC = GPP_DT_U50 * conv_factor, # daytime method
    Reco_DT_U50_gC = Reco_DT_U50 * conv_factor # daytime method

  ) %>%
  group_by(doy, Year) %>%

  # summarise(
  #   
  #   nee = NEE_gC,
  #   
  #   gpp_nt = GPP_uStar_f_gC, # nighttime method
  #   re_nt = Reco_uStar_gC, # nighttime method
  #   
  #   gpp_dt = GPP_DT_uStar_gC, # daytime method
  #   re_dt = Reco_DT_uStar_gC, # dayttime method
  #   
  # ) %>% 
  summarise(
    
    nee = sum(NEE_gC),
    
    gpp_nt = sum(GPP_U50_f_gC), # nighttime method
    re_nt = sum(Reco_U50_gC), # nighttime method
    
    gpp_dt = sum(GPP_DT_U50_gC), # daytime method
    re_dt = sum(Reco_DT_U50_gC), # dayttime method
    
  )

filled2 <- CombinedData %>%
   filter(NEE_U50_fqc<2) %>%
  filter(Tair_fqc<2) %>% 
  filter(Rg_fqc<2) %>% 
  
  # filter(GPP_U50_fqc<2) %>%
  # filter(Ustar_U50_fqc<2) %>% 
  #filter(Tair_fqc<2) %>% 
  #filter(Rg_fqc<2) %>% 
  
  group_by(doy, Year) %>%
  # mutate(
  #   
  #   nee = mean(NEE_uStar_f) * 1.0368,
  #   
  #   gpp_nt = mean(GPP_uStar_f) * 1.0368, # nighttime method
  #   re_nt = mean(Reco_uStar) * 1.0368, # nighttime method
  #   
  #   gpp_dt = mean(GPP_DT_uStar) * 1.0368, # daytime method
  #   re_dt = mean(Reco_DT_uStar) * 1.0368 # daytime method
  #   
  # )
  mutate(
    
    nee = mean(NEE_U50_f, na.rm = TRUE) * 1.0368,
    
    gpp_nt = mean(GPP_U50_f, na.rm = TRUE) * 1.0368, # nighttime method
    re_nt = mean(Reco_U50, na.rm = TRUE) * 1.0368, # nighttime method
    
    gpp_dt = mean(GPP_DT_U50, na.rm = TRUE) * 1.0368, # daytime method
    re_dt = mean(Reco_DT_U50, na.rm = TRUE) * 1.0368, # daytime method
    
    tair = mean(Tair, na.rm = TRUE),
    rg = mean(Rg_f, na.rm = TRUE)
  )



# combine datasets to be compared

# combined <- filled %>% 
#   left_join(filter(new_co2, plot == 1), by = c("Year", "doy")) # The key feature of left_join() is that it keeps all the rows from the left data frame and only the matching rows from the right data frame. If no match is found in the right data frame, NA values are inserted in the corresponding columns.

# comparison graphs for nee

filled2 %>%
  filter(Year == 2020) %>% 
  #filter(crop_year %in% c("18/19", "19/20", "20/21"))%>% 
  #filter(crop_year %in% c("18/19", "19/20", "20/21", "21/22", "22/23", "23/24"))%>% 
  
  ggplot()+
   # geom_line(aes(x = doy, y = nee_obs), color = "black", linetype = 1, size = 1, alpha = 0.5)+ # nirmani
   # geom_line(aes(x = doy, y = nee), size = 1, color = "#0072B2", alpha = 0.5)+ # new
   # 
  #geom_line(aes(x = doy, y = gpp_obs), size = 1, color = "black", alpha = 0.75)+ # nirmani
  # geom_line(aes(x = doy, y = -gpp_nt), color = "blue", linetype = 1, size = 1, alpha = 0.5)+ #new
  # geom_line(aes(x = doy, y = -gpp_dt), color = "darkorange", linetype = 1, size = 1, alpha = 0.5)+ # new

  #geom_line(aes(x = doy, y = resp_obs), size = 1, color = "black", alpha = 0.75)+ # nirmani
  geom_line(aes(x = doy, y = re_nt), color = "blue", linetype = 1, size = 1, alpha = 0.5)+ #new
  geom_line(aes(x = doy, y = re_dt), color = "darkorange", linetype = 1, size = 1, alpha = 0.5)+ # new

  facet_wrap(Year~., nrow = 4)+
  ylab("Reco (g C m-2 day-1)")+
  #ylab("NEE (g C m-2 day-1)")+
  #ylab("Reco (g C m-2 30-min-1)")+
  #ylab("GPP (g C m-2 30-min-1)")+
  
  #geom_line(aes(x = doy, y = tair), color = "red", linetype = 1, size = 1, alpha = 0.5)+ # new
  geom_line(aes(x = doy, y = nee), color = "darkgreen", linetype = 1, size = 1, alpha = 0.5)+ # new
 # geom_point(aes(x = doy, y = rg), color = "purple", linetype = 1, size = 1, alpha = 0.5)+ # new
  
  labs(
    title ="Reco",
    subtitle = "Comparison daytime (yellow) and nightime (blue) partitioning methods"
  )+
  
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

ggsave("reco_daily_p12_new.png", width = 10, height = 7)


CombinedData %>% 
  filter(Year == 2020 & doy %in% 100:300) %>% 
  group_by(doy) %>% 
  summarise(
    Rg = mean(Rg),
    Tair = mean(Tair),
    NEE_U50_f = mean(NEE_U50_f),
    GPP_U50_f = mean(GPP_U50_f),
    Reco_U50 = mean(Reco_U95)
  ) %>% 
  ggplot()+
  geom_point(aes(x=doy, y=Rg))+
  geom_point(aes(x=doy, y=Reco_U50*10), color = "red")+
  geom_point(aes(x=doy, y=Tair*10), color = "blue")+
  geom_point(aes(x=doy, y=NEE_U50_f*10), color = "darkgreen")



ggplot( )+
  #geom_point(data = filter(CombinedData, Year==2021 & doy %in% 130:140), aes(x = doy, y = NEE))+
  geom_point(data = filter(CombinedData, Year==2021 & doy %in% 130:140  ), aes(x = doy, y = NEE_uStar_f), color = "blue", alpha = 0.2)+
  geom_point(data = filter(CombinedData, Year==2021 & doy %in% 130:140 & NEE_uStar_fqc <2 ), aes(x = doy, y = NEE_uStar_f), color = "red", alpha = 0.2)+
  
  
  theme_bw()

NEE_U50_f_HQ<-CombinedData %>% 
  filter(Year==2021 & doy %in% 130:140 & NEE_U50_fqc <2 ) %>% 
  group_by(doy, Year) %>%
  summarise(
    NEE_hq = mean(NEE_U50_f, na.rm = TRUE)
  )

CombinedData %>% 
  group_by(Year) %>%
  summarise(
    total = n(),
    prop_1 = sum(NEE_U50_fqc == 1) / total*100,
    prop_2 = sum(NEE_U50_fqc == 2) / total*100,
    prop_3 = sum(NEE_U50_fqc == 3) / total*100,
    prop_4 = sum(NEE_U50_fqc == 4) / total*100
  )


CombinedData %>% 
  filter(Year==2020) %>%
  filter(doy %in% 1:365) %>%
  group_by(doy, Year) %>%
  summarise(
    NEE = mean(NEE, na.rm = TRUE),
    
    NEE_U50_f = mean(NEE_U50_f),
    
    GPP_U50 = mean(GPP_U50_f),
    Reco_U50 = mean(Reco_U50),

    GPP_DT_U50 = mean(GPP_DT_U50),
    Reco_DT_U50 = mean(Reco_DT_U50),

    Tair = mean(Tair),
    vpd = mean(VPD),
    rg = mean(Rg)/10
  ) %>%
  ggplot()+
  #geom_line(aes(x = doy, y = NEE_U50_f, alhpa = 0.5), color = "blue",linewidth = 1)+
  geom_line(aes(x = doy, y = NEE, alhpa = 0.5), color = "blue",linewidth = 1)+
  #geom_line(data = NEE_U50_f_HQ, aes(x = doy, y = NEE_hq, alhpa = 0.5), color = "green",linewidth = 1)+
  
  # geom_line(aes(x = doy, y = -GPP_DT_U50, alhpa = 0.5), color = "black", linewidth = 1)+
  # geom_line(aes(x = doy, y = Reco_DT_U50, alpha = 0.5), color = "purple", linewidth = 1)+
  
  geom_line(aes(x = doy, y = -GPP_U50, alhpa = 0.5), color = "black", linewidth = 1)+
  geom_line(aes(x = doy, y = Reco_U50, alpha = 0.5), color = "red", linewidth = 1)+
  
  geom_line(aes(x = doy, y = Tair, alpha = 0.5), color = "purple", linewidth = 1)+
  #geom_line(aes(x = doy, y = vpd, alpha = 0.5), color = "lightblue", linewidth = 1)+
  #geom_line(aes(x = doy, y = rg, alpha = 0.5), color = "darkgreen", linewidth = 1)+
  
  theme_bw()+
  facet_wrap(Year~.)


CombinedData %>% 
  filter(Year==2020) %>%
  #filter(doy %in% 200:250) %>%
  ggplot()+
  #geom_line(aes(x = doy, y = -GPP_DT_uStar, alhpa = 0.5), linewidth = 1)+
  geom_point(aes(x = doy, y = NEE_uStar_f, alhpa = 0.5), color = "blue",linewidth = 1)+
  geom_point(aes(x = doy, y = NEE_uStar_orig, alhpa = 0.5), linewidth = 1)+
  #geom_line(aes(x = doy, y = Tair, alpha = 0.5), color = "red", linewidth = 1)+
  #geom_line(aes(x = doy, y = vpd, alpha = 0.5), color = "lightblue", linewidth = 1)+
  #geom_point(aes(x = doy, y = Rg, alpha = 0.5), color = "darkgreen", linewidth = 1)+
  geom_line(aes(x = doy, y = Reco_DT_uStar, alpha = 0.5), color = "purple", linewidth = 1)+
  geom_line(aes(x = doy, y = Reco_uStar, alpha = 0.5), color = "red", linewidth = 1)+
  
  theme_bw()

sum(!is.na(CombinedData$NEE_uStar_orig))

ggplot()+
  # geom_point(data =  filter(CombinedData, is.na(NEE_uStar_orig)), aes(x = DoY, y = Reco_DT_uStar))+
  # geom_point(data =  filter(CombinedData, !is.na(NEE_uStar_orig)), aes(x = DoY, y = Reco_DT_uStar), color = "blue")+
  geom_point(data =  filter(CombinedData, is.na(NEE_uStar_orig)), aes(x = DoY, y = Reco_uStar))+
  geom_point(data =  filter(CombinedData, !is.na(NEE_uStar_orig)), aes(x = DoY, y = Reco_uStar), color = "blue")+
  # geom_point(data =  filter(CombinedData, is.na(NEE_uStar_orig)), aes(x = DoY, y = NEE_uStar_f))+
  # geom_point(data =  filter(CombinedData, !is.na(NEE_uStar_orig)), aes(x = DoY, y = NEE_uStar_f), color = "blue")+
  theme_bw()

CombinedData %>% 
  filter(Year == 2019 & doy %in% c(250:280)) %>%
  summarise(
    n = sum(is.na(NEE_uStar_orig))
  )
  
# checking data 

CombinedData_p12$Reco_DT_U50

CombinedData_p12 %>% 
  filter(Year == 2020 & doy %in% c(100:250)) %>%
  # mutate(
  # 
  #   NEE_gC = NEE_uStar_f * conv_factor,
  # 
  #   GPP_uStar_f_gC = GPP_uStar_f * conv_factor, # nighttime method
  #   Reco_uStar_gC = Reco_uStar * conv_factor, # nighttime method
  # 
  #   GPP_DT_uStar_gC = GPP_DT_uStar * conv_factor, # daytime method
  #   Reco_DT_uStar_gC = Reco_DT_uStar * conv_factor # daytime method
  # 
  # ) %>%
  # mutate(
  #   
  #   NEE_gC = NEE_uStar_f * conv_factor,
  #   
  #   GPP_uStar_f_gC = GPP_U50_f * conv_factor, # nighttime method
  #   Reco_uStar_gC = Reco_U50 * conv_factor, # nighttime method
  #   
  #   GPP_DT_uStar_gC = GPP_DT_U50 * conv_factor, # daytime method
  #   Reco_DT_uStar_gC = Reco_DT_U50 * conv_factor # daytime method
  #   
  # ) %>% 
  group_by(doy) %>% 
  # summarise(
  #   GPP_uStar_f_gC = sum(GPP_uStar_f_gC),
  #   GPP_DT_uStar_gC = sum(GPP_DT_uStar_gC)
  # ) %>% 
  summarise(
    GPP_uStar_f_gC = mean(GPP_uStar_f),
    GPP_DT_uStar_gC = mean(GPP_DT_uStar),
    NEE = mean(NEE, na.rm = T),
    LE = mean(LE, na.rm = T),
    H = mean(H, na.rm = T),
    Tair = mean(Tair, na.rm = T),
    RH = mean(RH, na.rm = T),
    VPD = mean(NEE, na.rm = T),
    Rg = mean(Rg, na.rm = T),
    Rg_f = mean(Rg_f, na.rm = T),
    Tair_f= mean(Tair_f)
  ) %>% 
  ggplot()+
  geom_line(aes(x = doy, y = -GPP_uStar_f_gC), color = "blue", linewidth=1)+
  geom_line(aes(x = doy, y = -GPP_DT_uStar_gC), color = "orange", linewidth=1)+
  #geom_point(aes(x = doy, y = NEE, alpha = 0.5), color = "red", linewidth=1)+
  #geom_point(aes(x = doy, y = -LE, alpha = 0.5), color = "red", linewidth=1)+
  #geom_line(aes(x = doy, y = H, alpha = 0.5), color = "red", linewidth=1)+
  # geom_line(aes(x = doy, y = -Tair, alpha = 0.5), color = "red", linewidth=1)+
  # geom_line(aes(x = doy, y = -Tair_f, alpha = 0.5), color = "blue", linewidth=1)+
  # geom_line(aes(x = doy, y = -Rg/10, alpha = 0.5), color = "red", linewidth=1)+
  # geom_line(aes(x = doy, y = -Rg_f/10, alpha = 0.5), color = "blue", linewidth=1)+
  geom_line(aes(x = doy, y = VPD, alpha = 0.5), color = "red", linewidth=1)+
  theme_bw()

CombinedData_p12$Rg_f
CombinedData_p12$LE
CombinedData_p12$H
CombinedData_p12$Tair
CombinedData_p12$RH
CombinedData_p12$VPD
CombinedData_p12$Rg

GPP_DT_uStar

CombinedData_p12 %>% 
  filter(Year == 2020 & doy %in% c(100:250)) %>%
  group_by(doy) %>%
  summarise(
    Rg = mean(Rg, na.rm = T)
  ) %>%
  ggplot()+
  geom_line(aes(x = doy, y = Rg/10))+
  geom_point(data = filter(CombinedData_p12, Year == 2020 & doy %in% c(100:250) ), aes(x = doy, y = Reco_uStar, alpha = 0.1), color = "blue", linewidth=1)+
  geom_point(data = filter(CombinedData_p12, Year == 2020 & doy %in% c(100:250) ), aes(x = doy, y = Reco_DT_uStar, alpha = 0.1), color = "orange", linewidth=1)+
  theme_bw()

CombinedData_p12 %>% 
  filter(Year == 2023 & doy %in% c(150:200)) %>%
  group_by(doy) %>%
  summarise(
    Rg = mean(Rg, na.rm = T),
    GPP_uStar_f = mean(GPP_uStar_f),
    GPP_DT_uStar = mean(GPP_DT_uStar)
  ) %>%
  ggplot()+
  geom_line(aes(x = doy, y = Rg/10))+
  # geom_point(data = filter(CombinedData_p12, Year == 2023 & doy %in% c(150:200) ), aes(x = doy, y = GPP_uStar_f, alpha = 0.1), color = "blue", linewidth=1)+
  # geom_point(data = filter(CombinedData_p12, Year == 2023 & doy %in% c(150:200) ), aes(x = doy, y =GPP_DT_uStar , alpha = 0.1), color = "orange", linewidth=1)+
  geom_line(aes(x = doy, y = GPP_uStar_f, alpha = 0.1), color = "blue", linewidth=1)+
  geom_line(aes(x = doy, y =GPP_DT_uStar , alpha = 0.1), color = "orange", linewidth=1)+
  theme_bw()


# # comparison graphs for respiration
# 
# combined%>% 
#   ggplot(aes(x = doy, y = resp))+
#   geom_line(size = 1, color = "black", alpha = 0.75)+
#   geom_line(aes(x = doy, y = resp_obs), color = "#D55E00", linetype = 1, size = 1, alpha = 0.5)+
#   facet_wrap(year~., nrow = 6)+
#   theme_bw()+
#   geom_vline(data = filter(seasonStarts2, year %in% c(2023:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 
# 
# # comparison graphs for gpp
# 
# combined%>% 
#   ggplot(aes(x = doy, y = gpp*(-1)))+
#   geom_line(size = 1, color = "black", alpha = 0.75)+
#   geom_line(aes(x = doy, y = gpp_obs), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
#   facet_wrap(year~., nrow = 6)+
#   theme_bw()+
#   geom_vline(data = filter(seasonStarts2, year %in% c(2023:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 
# 
# # graphing the sum of the fluxes across the entire year
# 
# # new dataset
# 
# combined_inner <- new_co2 %>% 
#   filter(plot == 1) %>% 
#   inner_join(filled, by = c("year", "doy")) # full join keeps only those points that are present in both datasets.
# 
# # merged graph
# 
# combined_inner %>% 
#   filter(year %in% c(2023:2021)) %>%
#   group_by(year) %>% 
#   summarise(
#     nee = sum(nee),
#     gpp = sum(gpp)*(-1),
#     resp = sum(resp),
#     nee_obs = sum(nee_obs),
#     gpp_obs = sum(gpp_obs),
#     resp_obs = sum(resp_obs)
#   ) %>%
#   pivot_longer(
#     cols = -year,  # All columns except 'year'
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     source = ifelse(grepl("_obs$", variable), "shannon", "reddyproc"),  # Add "old" or "new"
#     variable = str_remove(variable, "_obs$")
#   ) %>% 
#   mutate(variable = factor(variable, levels = c("gpp", "resp", "nee"))) %>%  # Set the desired order
#   ggplot(aes(x = source, y = value, fill = interaction(source, variable))) +  # Color based on interaction between source and variable
#   geom_col(colour = "black") +
#   scale_fill_manual(
#     values = c(
#       "reddyproc.gpp" = "gray", "shannon.gpp" = "#009E73", 
#       "reddyproc.resp" = "gray", "shannon.resp" = "#D55E00", 
#       "reddyproc.nee" = "gray", "shannon.nee" = "#0072B2", 
#       "reddyproc" = "gray", "shannon" = "white"
#     )
#   ) +  # Set custom colors for the interaction between source and variable
#   facet_grid(vars(variable), vars(year)) +
#   theme_bw() +
#   geom_text(aes(label = round(value, digits = 0)), vjust = 0.5) +  # Adjust text position if needed
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   ylab("Annual cumulative C flux (g/m2/year)")


#######################################################################

#######################################################################
################## understanding conversion factors ###################

# ─────────────────────────────────────────────────────────────
# 📌 CONVERSION FACTORS — µmol CO2 m⁻² s⁻¹ → g C m⁻² per unit time
# ─────────────────────────────────────────────────────────────

# General formula:
#   flux (µmol m⁻² s⁻¹) × time interval (in seconds) × (12.011 g/mol) / 1e6
#   → gives you g C m⁻² for that time interval

# ✅ HALF-HOURLY to g C m⁻²:
conv_factor_halfhour <- 12.011 / 1e6 * 1800  # 1800 = 30 min in seconds
# Use this to convert half-hourly fluxes (e.g., NEE) to g C:
# e.g., NEE_gC = NEE * conv_factor_halfhour

# ✅ HOURLY to g C m⁻²:
conv_factor_hour <- 12.011 / 1e6 * 3600  # 3600 = 1 hour in seconds

# ✅ DAILY MEAN FLUX (µmol m⁻² s⁻¹) to g C m⁻² d⁻¹:
conv_factor_day <- 12.011 / 1e6 * 86400  # 86400 = 24 hrs in seconds
# Multiply daily mean NEE by this to get g C m⁻² day⁻¹

# ✅ ANNUAL MEAN FLUX to g C m⁻² year⁻¹:
# This approach assumes a mean NEE value and scales it to the year
# Multiply by total seconds in a year (365 * 24 * 60 * 60 = 31,536,000)
conv_factor_annual <- 12.011 / 1e6 * 60 * 60 * 24 * 365  # = 378.0 approx.
# OR dynamically, if you want to account for the number of records in your dataset:
conv_factor_annual_dynamic <- 1 / 1e6 * 12.011 * 60 * 60 * 24 * length(results$NEE_U50_f) / 48
# (Assumes 48 half-hourly records per day)

# 🚨 Note: Pick the correct factor depending on whether you're converting instantaneous fluxes
# (half-hourly, hourly) or aggregated mean values (daily, annual)


#######################################################################

#######################################################################
########## building dataset EC + FG ###################################

# plot 12

# 2018

ec_18_p12_in <- read.table("ec_18_p12", header = TRUE) # this is the original file with EC data I had created
ec_18_p12_in <- ec_18_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2018_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2018)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2018 <- ec_18_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2018_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2018, file = "combined_ec_fg_full_data_allyear_p1_2018", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2019

ec_19_p12_in <- read.table("ec_19_p12", header = TRUE) # this is the original file with EC data I had created
ec_19_p12_in <- ec_19_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2019_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2019)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2019 <- ec_19_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2019_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2019, file = "combined_ec_fg_full_data_allyear_p1_2019", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2020

ec_20_p12_in <- read.table("ec_20_p12", header = TRUE) # this is the original file with EC data I had created
ec_20_p12_in <- ec_20_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2020_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2020)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2020 <- ec_20_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2020_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2020, file = "combined_ec_fg_full_data_allyear_p1_2020", sep = "\t", row.names = FALSE, quote = FALSE) 
# 2021

ec_21_p12_in <- read.table("ec_21_p12", header = TRUE) # this is the original file with EC data I had created
ec_21_p12_in <- ec_21_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2021_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2021)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2021 <- ec_21_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2021_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2021, file = "combined_ec_fg_full_data_allyear_p1_2021", sep = "\t", row.names = FALSE, quote = FALSE) 
# 2022

ec_22_p12_in <- read.table("ec_22_p12", header = TRUE) # this is the original file with EC data I had created
ec_22_p12_in <- ec_22_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2022_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2022)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2022 <- ec_22_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2022_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2022, file = "combined_ec_fg_full_data_allyear_p1_2022", sep = "\t", row.names = FALSE, quote = FALSE) 
# 2023

ec_23_p12_in <- read.table("ec_23_p12", header = TRUE) # this is the original file with EC data I had created
ec_23_p12_in <- ec_23_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2023_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2023)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2023 <- ec_23_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2023_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2023, file = "combined_ec_fg_full_data_allyear_p1_2023", sep = "\t", row.names = FALSE, quote = FALSE) 
# 2024

ec_24_p12_in <- read.table("ec_24_p12", header = TRUE) # this is the original file with EC data I had created
ec_24_p12_in <- ec_24_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2024_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2024)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2024 <- ec_24_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2024_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2024, file = "combined_ec_fg_full_data_allyear_p1_2024", sep = "\t", row.names = FALSE, quote = FALSE) 

# loading the data

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("ec_18_p3")
EddyData_2018_p3 <- EddyData_2018_p3 %>%
  filter(DoY < 194) # bringing in the dataset from p3 for the first few months of research year 1, as that is not available from tower in p1
EddyData_2018_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2018") # this function assumes the columns have the name of the variables, and the first row has the units
EddyData_2018_p1 <- EddyData_2018_p1 %>%
  filter(DoY > 193)
#2019
#EddyData_2019_p1 <- fLoadTXTIntoDataframe("ec_19_p12")
EddyData_2019_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2019")
#2020
#EddyData_2020_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_p1_2020")
EddyData_2020_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2020")
#2021
EddyData_2021_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2021")
#2022
EddyData_2022_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2022")
#2023
#EddyData_2023_p1 <- fLoadTXTIntoDataframe("ec_23_p12")
EddyData_2023_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2023")
#2024
EddyData_2024_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2024")










#######################################################################

########### test ####################

# comparing with filtered vector that agustin had

# filtered vector that agustin had

co2_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2022 <- co2_p3_2022 %>% rename(NEE = V1)
sum(is.na(co2_p3_2022))
sum(!is.na(co2_p3_2022))
n <- nrow(co2_p3_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)
time_df_2022 <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)
ec_22_p3 <- bind_cols(time_df_2022,co2_p3_2022)
ec_22_p3 <- ec_22_p3 %>%
  mutate(
    DOY= DoY + Hour / 24
  ) %>% 
  filter(DOY>121 & DOY<160)
ec_22_p3 <- ec_22_p3 %>%
  mutate(DOY = round(DOY, 2))

# file that jaison sent

flux_p3_05_2025_co2_jaison <- read.csv("C:/Users/aolivo/Downloads/EP__batch__2022-05-01-2022-05-31_eddypro_full_output__v20250902_00.csv", skip = 1, header = TRUE)
flux_p3_05_2025_co2_jaison = flux_grad_p1_05_2025_co2[-1,]
flux_p3_05_2025_co2_jaison$DOY = as.numeric(flux_p3_05_2025_co2_jaison$DOY)
flux_p3_05_2025_co2_jaison <- flux_p3_05_2025_co2_jaison %>%
  mutate(DOY = round(as.numeric(DOY), 2))

ec_22_p3 %>%
  left_join(flux_p3_05_2025_co2_jaison, by="DOY") %>% 
  filter(DOY>121 & DOY<160) %>%
  filter(NEE>-300) %>% 
  select(DOY, NEE, co2_flux ) %>% 
  ggplot(aes(x = NEE, y = as.numeric(co2_flux)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

data=ec_22_p3 %>%
  left_join(flux_p3_05_2025_co2_jaison, by="DOY") %>% 
  filter(DOY>121 & DOY<160) %>%
  filter(NEE>-300) %>% 
  select(DOY, NEE, co2_flux )

summary(lm(data=data,as.numeric(co2_flux)~NEE))

# comparing with filtered vector in the database

co2_p3_2022 <- read.table("Z:/E26/database/ers4/2022/EC/EddyPro/P3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2022 <- co2_p3_2022 %>% rename(NEE = V1)
sum(is.na(co2_p3_2022))
sum(!is.na(co2_p3_2022))
n <- nrow(co2_p3_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)
time_df_2022 <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)
ec_22_p3 <- bind_cols(time_df_2022,co2_p3_2022)
ec_22_p3 <- ec_22_p3 %>%
  mutate(
    DOY= DoY + Hour / 24
  ) %>% 
  filter(DOY>121 & DOY<160)
ec_22_p3 <- ec_22_p3 %>%
  mutate(DOY = round(DOY, 2))

# file that jaison sent

flux_p3_05_2025_co2_jaison <- read.csv("C:/Users/aolivo/Downloads/EP__batch__2022-05-01-2022-05-31_eddypro_full_output__v20250902_00.csv", skip = 1, header = TRUE)
flux_p3_05_2025_co2_jaison = flux_grad_p1_05_2025_co2[-1,]
flux_p3_05_2025_co2_jaison$DOY = as.numeric(flux_p3_05_2025_co2_jaison$DOY)
flux_p3_05_2025_co2_jaison <- flux_p3_05_2025_co2_jaison %>%
  mutate(DOY = round(as.numeric(DOY), 2))

ec_22_p3 %>%
  left_join(flux_p3_05_2025_co2_jaison, by="DOY") %>% 
  filter(DOY>121 & DOY<160) %>%
  filter(NEE>-300) %>% 
  select(DOY, NEE, co2_flux ) %>% 
  ggplot(aes(x = NEE, y = as.numeric(co2_flux)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

# comparing with raw EddyPro output from database

# file in database

flux_p3_05_2025_co2_eddypro <- read.csv("Z:/E26/database/ers4/2022/EC/EddyPro/P3_tower/EddyProOutputs/Full_output/eddypro_Plot3_May_June_2022_full_output_2024-04-24T165437_adv.csv", skip = 1, header = TRUE)
flux_p3_05_2025_co2_eddypro = flux_p3_05_2025_co2_eddypro[-1,]
flux_p3_05_2025_co2_eddypro$DOY = as.numeric(flux_p3_05_2025_co2_eddypro$DOY)
flux_p3_05_2025_co2_eddypro <- flux_p3_05_2025_co2_eddypro %>%
  mutate(DOY = round(as.numeric(DOY), 4))
flux_p3_05_2025_co2_eddypro = flux_p3_05_2025_co2_eddypro %>% 
  mutate(
    co2_flux2 = co2_flux
  ) %>%   
  select(DOY, co2_flux2)

# file that jaison sent

flux_p3_05_2025_co2_jaison <- read.csv("C:/Users/aolivo/Downloads/EP__batch__2022-05-01-2022-05-31_eddypro_full_output__v20250902_00.csv", skip = 1, header = TRUE)
flux_p3_05_2025_co2_jaison = flux_grad_p1_05_2025_co2[-1,]
flux_p3_05_2025_co2_jaison$DOY = as.numeric(flux_p3_05_2025_co2_jaison$DOY)
flux_p3_05_2025_co2_jaison <- flux_p3_05_2025_co2_jaison %>%
  mutate(DOY = round(as.numeric(DOY), 4))

flux_p3_05_2025_co2 %>%
  left_join(flux_p3_05_2025_co2_eddypro, by="DOY") %>% 
  filter(DOY>121 & DOY<160) %>%
  select(DOY, co2_flux, co2_flux2 ) %>%
  filter(!co2_flux<abs(1000)) %>% 
  ggplot(aes(x = as.numeric(co2_flux), y = as.numeric(co2_flux2)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()


# comparing with filtered vector

co2_p3_2022 <- read.table("Z:/E26/database/ers4/2022/EC/EddyPro/P3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2022 <- co2_p3_2022 %>% rename(NEE = V1)
sum(is.na(co2_p3_2022))
sum(!is.na(co2_p3_2022))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2022 <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

ec_22_p3 <- bind_cols(time_df_2022,co2_p3_2022)

ec_22_p3 <- ec_22_p3 %>%
  mutate(
    DOY= DoY + Hour / 24
  ) %>% 
  filter(DOY>121 & DOY<160)

ec_22_p3 <- ec_22_p3 %>%
  mutate(DOY = round(DOY, 2))

#######################################################################
####################### old ###########################################

# current EC data has some gaps for plots 1/2 and 3/4
# p12: current gaps in NEE data are for DOY 268-340 in 2019, and DOY 230 to 279 in 2023;
# p34: current gaps in NEE data are for DOY 332-365 in 2019, and DOY 88 to 121 in 2024;

# I will bring flux gradient data to fill these specifics gaps.

########### loading flux gradient data that is used to fill gaps + merging all files to create the final dataframe that will be used as reddyproc input

# the bulk of the flux gradient data is loaded in another major section below. See other headlines below. Those vectors have to be loaded before this code can be run.

# checking specific data to fill gaps

# plot 1/2

# Current gaps in the data are for DOY 268-340 in 2019, and DOY 230 to 279 in 2023;

# period 1 - P12 (DOY 268-340 in 2019)

ec_19_p12_in <- read.table("ec_19_p12", header = TRUE) # this is the original file with EC data I had created
ec_19_p12_in <- ec_19_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2019_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2019 & DOY %in% 268:340)) %>% 
  filter((Year == 2019)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2019 <- ec_19_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2019_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2019, file = "combined_ec_fg_full_data_p1_2019", sep = "\t", row.names = FALSE, quote = FALSE) # final merged file that I will use for 2019

sum(is.na(as.numeric(combined_ec_fg_full_data_p1_2019$NEE)))
sum(is.na(as.numeric(ec_19_p12_in$NEE)))

# TEST
ec_20_p12_in <- read.table("ec_20_p12", header = TRUE) # this is the original file with EC data I had created
ec_20_p12_in <- ec_20_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))
flux_grad_p1p2_2020_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2020)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2020 <- ec_20_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2020_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2020, file = "combined_ec_fg_full_data_p1_2020", sep = "\t", row.names = FALSE, quote = FALSE) # final merged file that I will use for 2020

sum(is.na(as.numeric(combined_ec_fg_full_data_p1_2020$NEE)))
sum(is.na(as.numeric(ec_20_p12_in$NEE)))

str(combined_ec_fg_full_data_p1_2020)

# period 2 - P12 (DOY 230 to 279 in 2023)

ec_23_p12_in <- read.table("ec_23_p12", header = TRUE)
ec_23_p12_in <- ec_23_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2))

flux_grad_p1p2_2023_to_merge <- flux_grad_p1p2_2018_2024 %>% 
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter(Year == 2023 & DOY %in% 230:279) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2023 <- ec_23_p12_in %>% 
  left_join(
    flux_grad_p1p2_2023_to_merge, 
    by = c("Date")
  ) %>%  
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`)
  ) %>% 
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p1_2023, file = "combined_ec_fg_full_data_p1_2023", sep = "\t", row.names = FALSE, quote = FALSE)

# p3

# Current gaps in the data are for DOY 332-365 in 2019, and DOY 88 to 121 in 2024;

# period 1 - P3 (DOY 332-365 in 2019)

ec_19_p3_in <- read.table("ec_19_p3", header = TRUE)
ec_19_p3_in <- ec_19_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits = 2))

flux_grad_p3_2019_to_merge <- flux_grad_p3p4_2018_2024 %>% 
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2019 & DOY %in% 332:365)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2019 <- ec_19_p3_in %>% 
  left_join(
    flux_grad_p3_2019_to_merge, 
    by = c("Date")
  ) %>%
  left_join(
    obs_temp_2019_p3, 
    by = c("Date", "Year")
  ) %>%
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`),
    Tair = ifelse(is.na(Air_temp_avg), Tair, Air_temp_avg),
  ) %>% 
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p3_2019, file = "combined_ec_fg_full_data_p3_2019", sep = "\t", row.names = FALSE, quote = FALSE)

# period 2 - P3 (DOY 88 to 121 in 2024)

ec_24_p3_in <- read.table("ec_24_p3", header = TRUE)
ec_24_p3_in <- ec_24_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24))%>% 
  mutate(Date = round(Date, digits = 2))

flux_grad_p3_2024_to_merge <- flux_grad_p3p4_2018_2024 %>% 
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2024 & DOY %in% 88:160)) %>% 
  mutate(Date = round(Date, 2)) %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2024 <- ec_24_p3_in %>% 
  left_join(
    flux_grad_p3_2024_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    NEE = ifelse(is.na(CO2_flux_umol), NEE, CO2_flux_umol),
    Ustar = ifelse(is.na(` u*`), Ustar, ` u*`),
    H = ifelse(is.na(` H`), H, ` H`),
  ) %>% 
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

write.table(combined_ec_fg_full_data_p3_2024, file = "combined_ec_fg_full_data_p3_2024", sep = "\t", row.names = FALSE, quote = FALSE)

#######################################################################

