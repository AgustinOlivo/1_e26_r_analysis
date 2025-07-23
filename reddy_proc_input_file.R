# creation of reddyproc input based on eddyprot full output
# agustin olivo
# aolivo@uoguelph.ca

######################################
############# general ################
# packages

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
library(lubridate)
library(tidyr)

###################################

########################################################
########## tower p12 - EC data only ##############

units_row <- tibble(
  Year = "-", 
  DoY = "-", 
  Hour = "-", 
  NEE = "umolm-2s-1", 
  LE = "Wm-2", 
  H = "Wm-2", 
  Ustar = "ms-1", 
  Tair = "degC", 
  RH = "%", 
  VPD = "hPa", 
  Rg = "Wm-2"
)

# read raw data

# 2018

# co2 

# ec data
co2_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2018 <- co2_p12_2018 %>% rename(NEE = V1)
sum(is.na(co2_p12_2018))
sum(!is.na(co2_p12_2018))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2018)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n) # the time-stamp has to start at 0.5 in each day and finish at 24 for that same day! This is super important for REddyProc inputs.

time_df_2018 <- data.frame(
  Year = rep(2018, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# le 
le_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2018 <- le_p12_2018 %>% rename(LE = V1)
sum(is.na(le_p12_2018))
sum(!is.na(le_p12_2018))

# h
h_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2018 <- h_p12_2018 %>% rename(H = V1)
sum(is.na(h_p12_2018))
sum(!is.na(h_p12_2018))

# ustar
ustar_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2018 <- ustar_p12_2018 %>% rename(Ustar = V1)
sum(is.na(ustar_p12_2018))
sum(!is.na(ustar_p12_2018))

# tair
tair_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2018 <- tair_p12_2018 %>% rename(Tair = V1)
tair_p12_2018$Tair <-  tair_p12_2018$Tair - 273.15
sum(!is.na(tair_p12_2018))
sum(is.na(tair_p12_2018)) # a lot of NAs remaining in this dataset

# other sources

# temperature from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_temp_2018 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
obs_temp_2018 <- obs_temp_2018 %>% 
  bind_cols(
    time_df_2018
  ) %>% rename(Tair = V1)
sum(is.na(obs_temp_2018$Tair))
sum(!is.na(obs_temp_2018$Tair))

# Replace NA or NaN in tair_p12_2018 with values from obs_temp_2018
tair_p12_2018$Tair[is.na(tair_p12_2018$Tair) | is.nan(tair_p12_2018$Tair)] <- 
  obs_temp_2018$Tair[is.na(tair_p12_2018$Tair) | is.nan(tair_p12_2018$Tair)]
sum(is.na(tair_p12_2018$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2018$V1))
sum(!is.na(rh_p12_2018$V1))
rh_p12_2018 <- rh_p12_2018 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2018$V1))
sum(!is.na(vpd_p12_2018$V1))
vpd_p12_2018 <- vpd_p12_2018 %>% rename(VPD = V1)

# Rg from other files that patrick had created, and that should be good in terms of time stamp (for other years, I had to puleed from a different source to fix the time stamp)

rg_p12_2018_raw <- fLoadTXTIntoDataframe("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2018_R.txt")
rg_p12_2018 <- rg_p12_2018_raw %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )
rg_p12_2018 <- rg_p12_2018 %>% rename(Rg = Rg)  # Already named
rg_p12_2018 <- bind_cols(time_df_2018, rg_p12_2018) %>% 
  select(-c(Year, DoY, Hour))

sum(is.na(rg_p12_2018))
sum(!is.na(rg_p12_2018))

# combine all into one data frame
ec_18_p12 <- bind_cols(time_df_2018,co2_p12_2018,le_p12_2018,h_p12_2018,ustar_p12_2018,tair_p12_2018,rh_p12_2018,vpd_p12_2018,rg_p12_2018)
ec_18_p12 <- rbind(units_row, ec_18_p12)

write.table(ec_18_p12, file = "ec_18_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2019

# filtered
co2_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2019 <- co2_p12_2019 %>% rename(NEE = V1)
sum(is.na(co2_p12_2019))
sum(!is.na(co2_p12_2019))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2019)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2019 <- data.frame(
  Year = rep(2019, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2019 <- le_p12_2019 %>% rename(LE = V1)
sum(is.na(le_p12_2019))
sum(!is.na(le_p12_2019))

h_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2019 <- h_p12_2019 %>% rename(H = V1)
sum(is.na(h_p12_2019))
sum(!is.na(h_p12_2019))

ustar_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2019 <- ustar_p12_2019 %>% rename(Ustar = V1)
sum(is.na(ustar_p12_2019))
sum(!is.na(ustar_p12_2019))

tair_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2019 <- tair_p12_2019 %>% rename(Tair = V1)
tair_p12_2019$Tair <-  tair_p12_2019$Tair - 273.15
sum(is.na(tair_p12_2019)) 
sum(!is.na(tair_p12_2019)) # after an initial run, it seems like the original fieltered vectore file had a lot of gaps, and that may be causing issues with the gap-filling. I will add some additional data to this dataset.

# other sources

# temperature from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_temp_2019 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2019_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2019 <- obs_temp_2019[-1,]
obs_temp_2019 <- obs_temp_2019 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2019
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2019$Tair))
sum(!is.na(obs_temp_2019$Tair))

# Replace NA or NaN in tair_p12_2019 with values from obs_temp_2019
tair_p12_2019$Tair[is.na(tair_p12_2019$Tair) | is.nan(tair_p12_2019$Tair)] <- 
  obs_temp_2019$Tair[is.na(tair_p12_2019$Tair) | is.nan(tair_p12_2019$Tair)]
sum(is.na(tair_p12_2019$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2019", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2019$V1))
rh_p12_2019 <- rh_p12_2019 %>% rename(RH = V1)

# rg from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_rh_2019 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2019_30min_avg.csv", skip = 3, header = TRUE)
obs_rh_2019 <- obs_rh_2019[-1,]
obs_rh_2019 <- obs_rh_2019 %>%
  select(RH_avg) %>%
  mutate(
    RH = as.numeric(RH_avg)
  ) %>% bind_cols(
    time_df_2019
  ) %>% 
  select(!RH_avg)
sum(is.na(obs_rh_2019$RH))
sum(!is.na(obs_rh_2019$RH))

# Replace NA or NaN in tair_p12_2019 with values from obs_temp_2019
rh_p12_2019$RH[is.na(rh_p12_2019$RH) | is.nan(rh_p12_2019$RH)] <- 
  obs_rh_2019$RH[is.na(rh_p12_2019$RH) | is.nan(rh_p12_2019$RH)]
sum(is.na(rh_p12_2019$RH))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2019", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2019$V1))
vpd_p12_2019 <- vpd_p12_2019 %>% rename(VPD = V1)
sum(is.na(vpd_p12_2019))
sum(!is.na(vpd_p12_2019))

# exctracting from a different source (old files)
rg_p12_2019_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2019_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2019_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2019_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2019 <- rg_p3_2019_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2019_raw[[cur_column()]])))
rg_2019 <- rg_2019 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2019 <- bind_cols(time_df_2019, rg_2019) %>%
  select(-Year)

rg_2019 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

sum(is.na(rg_2019$Rg))
sum(!is.na(rg_2019$Rg))

# combine all into one data frame
ec_19_p12 <- bind_cols(time_df_2019,co2_p12_2019,le_p12_2019,h_p12_2019,ustar_p12_2019,tair_p12_2019,rh_p12_2019,vpd_p12_2019) %>% 
  left_join(rg_2019, by = c("DoY", "Hour"))
ec_19_p12 <- rbind(units_row, ec_19_p12)

write.table(ec_19_p12, file = "ec_19_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2020

# filtered
co2_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2020 <- co2_p12_2020 %>% rename(NEE = V1)
sum(is.na(co2_p12_2020))
sum(!is.na(co2_p12_2020))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2020)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2020 <- data.frame(
  Year = rep(2020, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2020 <- le_p12_2020 %>% rename(LE = V1)
sum(is.na(le_p12_2020))
sum(!is.na(le_p12_2020))

h_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2020 <- h_p12_2020 %>% rename(H = V1)
sum(is.na(h_p12_2020))
sum(!is.na(h_p12_2020))

ustar_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2020 <- ustar_p12_2020 %>% rename(Ustar = V1)
sum(is.na(h_p12_2020))
sum(!is.na(h_p12_2020))

tair_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2020 <- tair_p12_2020 %>% rename(Tair = V1)
tair_p12_2020$Tair <-  tair_p12_2020$Tair - 273.15

sum(is.na(tair_p12_2020)) # after an initial run, it seems like the original fieltered vectore file had a lot of gaps, and that may be causing issues with the gap-filling. I will add some additional data to this dataset.
sum(!is.na(tair_p12_2020))

# other sources

# temperature

obs_temp_2020 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2020_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2020 <- obs_temp_2020[-1,]
obs_temp_2020 <- obs_temp_2020 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2020
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2020$Tair))
sum(!is.na(obs_temp_2020$Tair))

# Replace NA or NaN in tair_p12_2020 with values from obs_temp_2020
tair_p12_2020$Tair[is.na(tair_p12_2020$Tair) | is.nan(tair_p12_2020$Tair)] <- 
  obs_temp_2020$Tair[is.na(tair_p12_2020$Tair) | is.nan(tair_p12_2020$Tair)]
sum(is.na(tair_p12_2020$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2020", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2020$V1))
rh_p12_2020 <- rh_p12_2020 %>% rename(RH = V1)
sum(is.na(rh_p12_2020))
sum(!is.na(rh_p12_2020))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2020", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2020$V1))
vpd_p12_2020 <- vpd_p12_2020 %>% rename(VPD = V1)
sum(is.na(vpd_p12_2020))
sum(!is.na(vpd_p12_2020))

# rg

rg_p12_2020_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2020_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2020_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2020_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2020 <- rg_p3_2020_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2020_raw[[cur_column()]])))
sum(is.na(rg_p12_2020_raw))
sum(is.na(rg_p3_2020_raw))
sum(is.na(rg_2020))

rg_2020 <- rg_2020 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2020 <- bind_cols(time_df_2020, rg_2020) %>%
  select(-Year)

rg_2020 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# rg_p12_2020_raw <- fLoadTXTIntoDataframe("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2020_R.txt")
# rg_p12_2020[1, ] <- NA # apply NA the first row in the dataset that starts at 0 and I think it should start at 0.5; this may not apply to all years
# rg_p12_2020 <- rg_p12_2020_raw %>%
#   select(Rg) %>%
#   mutate(
#     Rg = as.numeric(Rg)
#   )
# rg_p12_2020 <- rg_p12_2020 %>% rename(Rg = Rg)  # Already named
# rg_p12_2020 <- bind_cols(time_df, rg_p12_2020) %>%
#   select(-Year)
# rg_p12_2020 %>%
#   ggplot()+
#   geom_point(aes(x = Hour, y =Rg))

# rg_p12_2020_raw <- fLoadTXTIntoDataframe("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2020_R.txt")
# rg_p12_2020 <- rg_p12_2020_raw %>%
#   slice(-c(1)) %>%
#   select(Rg, DoY, Hour) %>%
#   mutate(
#     Rg = as.numeric(Rg),
#     DoY = as.integer(DoY),
#     Hour = as.numeric(Hour)
#   )
# rg_p12_2020 <- rg_p12_2020 %>% rename(Rg = Rg)  # Already named
# nrow(rg_p12_2020)

# combine all into one data frame
ec_20_p12 <- bind_cols(time_df_2020,co2_p12_2020,le_p12_2020,h_p12_2020,ustar_p12_2020,tair_p12_2020,rh_p12_2020,vpd_p12_2020) %>% 
  left_join(rg_2020, by = c("DoY", "Hour"))
ec_20_p12 <- rbind(units_row, ec_20_p12)

write.table(ec_20_p12, file = "ec_20_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2021

# filtered
co2_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2021 <- co2_p12_2021 %>% rename(NEE = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2021)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2021 <- data.frame(
  Year = rep(2021, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2021 <- le_p12_2021 %>% rename(LE = V1)

h_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2021 <- h_p12_2021 %>% rename(H = V1)

ustar_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2021 <- ustar_p12_2021 %>% rename(Ustar = V1)

tair_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2021 <- tair_p12_2021 %>% rename(Tair = V1)
tair_p12_2021$Tair <- tair_p12_2021$Tair - 273.15

sum(is.na(tair_p12_2021))
sum(!is.na(tair_p12_2021))

# other sources

# temperature

obs_temp_2021 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2021_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2021 <- obs_temp_2021[-1,]
obs_temp_2021 <- obs_temp_2021 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2021
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2021$Tair))
sum(!is.na(obs_temp_2021$Tair))

# Replace NA or NaN in tair_p12_2021 with values from obs_temp_2021
tair_p12_2021$Tair[is.na(tair_p12_2021$Tair) | is.nan(tair_p12_2021$Tair)] <- 
  obs_temp_2021$Tair[is.na(tair_p12_2021$Tair) | is.nan(tair_p12_2021$Tair)]
sum(is.na(tair_p12_2021$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2021", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2021$V1))
rh_p12_2021 <- rh_p12_2021 %>% rename(RH = V1)
sum(is.na(rh_p12_2021))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2021", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2021$V1))
vpd_p12_2021 <- vpd_p12_2021 %>% rename(VPD = V1)
sum(is.na(vpd_p12_2021))

# rg

rg_p12_2021_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2021_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2021_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2021_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2021 <- rg_p3_2021_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2021_raw[[cur_column()]])))
sum(is.na(rg_p12_2021_raw))
sum(is.na(rg_p3_2021_raw))
sum(is.na(rg_2021))

rg_2021 <- rg_2021 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2021 <- bind_cols(time_df_2021, rg_2021) %>%
  select(-Year)

rg_2021 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_21_p12 <- bind_cols(time_df_2021,co2_p12_2021,le_p12_2021,h_p12_2021,ustar_p12_2021,tair_p12_2021,rh_p12_2021,vpd_p12_2021) %>% 
  left_join(rg_2021, by = c("DoY", "Hour"))
ec_21_p12 <- rbind(units_row, ec_21_p12)

write.table(ec_21_p12, file = "ec_21_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2022

# filtered
co2_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2022 <- co2_p12_2022 %>% rename(NEE = V1)
sum(is.na(co2_p12_2022))
sum(!is.na(co2_p12_2022))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2022 <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2022 <- le_p12_2022 %>% rename(LE = V1)

h_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2022 <- h_p12_2022 %>% rename(H = V1)

ustar_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2022 <- ustar_p12_2022 %>% rename(Ustar = V1)

tair_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2022 <- tair_p12_2022 %>% rename(Tair = V1)
tair_p12_2022$Tair <-  tair_p12_2022$Tair - 273.15
sum(is.na(tair_p12_2022))
sum(!is.na(tair_p12_2022))

# other sources

# temperature
obs_temp_2022 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE) # here I decided to use this file instead of the ECCC available in the database becuase the ECCC seemed to have a lot of NAs
obs_temp_2022 <- obs_temp_2022 %>% rename(Tair = V1)
sum(is.na(obs_temp_2022))
sum(!is.na(obs_temp_2022))

# Replace NA or NaN in tair_p12_2022 with values from obs_temp_2022
tair_p12_2022$Tair[is.na(tair_p12_2022$Tair) | is.nan(tair_p12_2022$Tair)] <- 
  obs_temp_2022$Tair[is.na(tair_p12_2022$Tair) | is.nan(tair_p12_2022$Tair)]
sum(is.na(tair_p12_2022$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2022$V1))
sum(!is.na(rh_p12_2022$V1))
rh_p12_2022 <- rh_p12_2022 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2022$V1))
vpd_p12_2022 <- vpd_p12_2022 %>% rename(VPD = V1)

# rg

rg_p12_2022_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2022_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# rg_p3_2022_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2022_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# the data from p3 looked weird, so I cotinued only with data from p12 for 2022; it did have sufficient datapoints I think

rg_2022 <- rg_p12_2022_raw 
sum(is.na(rg_p12_2022_raw))

rg_2022 <- rg_2022 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )
sum(is.na(rg_2022$Rg))

rg_2022 <- bind_cols(time_df_2022, rg_2022) %>%
  select(-Year)

rg_2022 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_22_p12 <- bind_cols(time_df_2022,co2_p12_2022,le_p12_2022,h_p12_2022,ustar_p12_2022,tair_p12_2022,rh_p12_2022,vpd_p12_2022) %>% 
  left_join(rg_2022, by = c("DoY", "Hour"))
ec_22_p12 <- rbind(units_row, ec_22_p12)

write.table(ec_22_p12, file = "ec_22_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2023

# filtered
co2_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2023 <- co2_p12_2023 %>% rename(NEE = V1)
sum(is.na(co2_p12_2023))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2023)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2023 <- data.frame(
  Year = rep(2023, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2023 <- le_p12_2023 %>% rename(LE = V1)

h_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2023 <- h_p12_2023 %>% rename(H = V1)

ustar_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2023 <- ustar_p12_2023 %>% rename(Ustar = V1)

tair_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2023 <- tair_p12_2023 %>% rename(Tair = V1)
tair_p12_2023$Tair <-  tair_p12_2023$Tair - 273.15

sum(is.na(tair_p12_2023))
sum(!is.na(tair_p12_2023))

# other sources

# temperature
obs_temp_2023 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE) 
obs_temp_2023 <- obs_temp_2023 %>% rename(Tair = V1)
sum(is.na(obs_temp_2023))
sum(!is.na(obs_temp_2023))

# Replace NA or NaN in tair_p12_2023 with values from obs_temp_2023
tair_p12_2023$Tair[is.na(tair_p12_2023$Tair) | is.nan(tair_p12_2023$Tair)] <- 
  obs_temp_2023$Tair[is.na(tair_p12_2023$Tair) | is.nan(tair_p12_2023$Tair)]
sum(is.na(tair_p12_2023$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2023$V1))
rh_p12_2023 <- rh_p12_2023 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2023$V1))
vpd_p12_2023 <- vpd_p12_2023 %>% rename(VPD = V1)

# rg

# radiation data from the previous source for 2023 looks higher than other years; discussing with Claudia we agreed to use weather station data.

obs_rg_2023 <- read.csv("obs_data/measured_co2/data_extraction/rg/ERS_WX2_weather_2023.csv",  header = TRUE)
colnames(obs_rg_2023) <- obs_rg_2023[1,]
obs_rg_2023 <- obs_rg_2023[-c(1:3),]
rg_2023 <- obs_rg_2023 %>%
  select(RF1_E_Total) %>%
  mutate(Rg_MJ = as.numeric(RF1_E_Total)) %>%
  uncount(weights = 2) %>%  # 🔁 duplicate each row twice, since the original dataset is hourly, not halfhourly.
  mutate(Rg = Rg_MJ * 277.78) %>% # transforming Rj from MJ/m2 to W/m2
  bind_cols(time_df_2023) %>%
  select(-c(RF1_E_Total, Year,Rg_MJ))

# combine all into one data frame
ec_23_p12 <- bind_cols(time_df_2023,co2_p12_2023,le_p12_2023,h_p12_2023,ustar_p12_2023,tair_p12_2023,rh_p12_2023,vpd_p12_2023) %>% 
  left_join(rg_2023, by = c("DoY", "Hour"))
ec_23_p12 <- rbind(units_row, ec_23_p12)

write.table(ec_23_p12, file = "ec_23_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2024

# filtered
co2_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2024 <- co2_p12_2024 %>% rename(NEE = V1)
sum(is.na(co2_p12_2024))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2024)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2024 <- data.frame(
  Year = rep(2024, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2024 <- le_p12_2024 %>% rename(LE = V1)

h_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2024 <- h_p12_2024 %>% rename(H = V1)

ustar_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2024 <- ustar_p12_2024 %>% rename(Ustar = V1)

tair_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2024 <- tair_p12_2024 %>% rename(Tair = V1)
tair_p12_2024$Tair <-  tair_p12_2024$Tair - 273.15
sum(is.na(tair_p12_2024))
sum(!is.na(tair_p12_2024))

# other sources

# temperature
obs_temp_2024 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE) 
obs_temp_2024 <- obs_temp_2024 %>% rename(Tair = V1)
sum(is.na(obs_temp_2024))
sum(!is.na(obs_temp_2024))

# Replace NA or NaN in tair_p12_2024 with values from obs_temp_2024
tair_p12_2024$Tair[is.na(tair_p12_2024$Tair) | is.nan(tair_p12_2024$Tair)] <- 
  obs_temp_2024$Tair[is.na(tair_p12_2024$Tair) | is.nan(tair_p12_2024$Tair)]
sum(is.na(tair_p12_2024$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p12_2024$V1))
sum(!is.na(rh_p12_2024$V1))
rh_p12_2024 <- rh_p12_2024 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p12_2024$V1))
vpd_p12_2024 <- vpd_p12_2024 %>% rename(VPD = V1)

# rg

rg_p12_2024_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2024_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2024_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2024_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2024 <- rg_p12_2024_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p3_2024_raw[[cur_column()]]))) # i had initially used first the measurements from p3, and complemented with measurements from plot 3. Then, the overall result for radiation for 2024 was high compared to other years (~180), so I decided to do it the other way; use mostly data from plot 12 and complement with data from plot 3. This seems to show a more reasonable final number, comparable to other years.
sum(is.na(rg_p12_2024_raw))
sum(is.na(rg_p3_2024_raw))
sum(is.na(rg_2024$Rg))
sum(!is.na(rg_2024$Rg))

rg_2024 <- rg_2024 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2024 <- bind_cols(time_df_2024, rg_2024) %>%
  select(-Year)

rg_2024 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_24_p12 <- bind_cols(time_df_2024,co2_p12_2024,le_p12_2024,h_p12_2024,ustar_p12_2024,tair_p12_2024,rh_p12_2024,vpd_p12_2024)%>% 
  left_join(rg_2024, by = c("DoY", "Hour"))
ec_24_p12 <- rbind(units_row, ec_24_p12)

write.table(ec_24_p12, file = "ec_24_p12", sep = "\t", row.names = FALSE, quote = FALSE)

########################################################

########################################################
########## tower p12 - EC + FG  data ###################

# plot 12

# 2018

ec_18_p12_in <- read.table("ec_18_p12", header = TRUE) # this is the original file with EC data I had created
ec_18_p12_in <- ec_18_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2018_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2018)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2018 <- ec_18_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2018_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = coalesce(NEE, CO2_flux_umol)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

sum(is.na(combined_ec_fg_full_data_p1_2018$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2018$NEE))

write.table(combined_ec_fg_full_data_p1_2018, file = "combined_ec_fg_full_data_allyear_p1_2018", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2019

ec_19_p12_in <- read.table("ec_19_p12", header = TRUE) # this is the original file with EC data I had created
ec_19_p12_in <- ec_19_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2019_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2019)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)


str(ec_19_p12_in)

combined_ec_fg_full_data_p1_2019 <- ec_19_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2019_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = coalesce(CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

sum(is.na(combined_ec_fg_full_data_p1_2019$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2019$NEE))

write.table(combined_ec_fg_full_data_p1_2019, file = "combined_ec_fg_full_data_allyear_p1_2019", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2020

ec_20_p12_in <- read.table("ec_20_p12", header = TRUE) # this is the original file with EC data I had created
ec_20_p12_in <- ec_20_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2020_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2020)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2020 <- ec_20_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2020_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

sum(is.na(combined_ec_fg_full_data_p1_2020$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2020$NEE))

write.table(combined_ec_fg_full_data_p1_2020, file = "combined_ec_fg_full_data_allyear_p1_2020", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2021

ec_21_p12_in <- read.table("ec_21_p12", header = TRUE) # this is the original file with EC data I had created
ec_21_p12_in <- ec_21_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2021_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2021)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2021 <- ec_21_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2021_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

sum(is.na(combined_ec_fg_full_data_p1_2021$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2021$NEE))

write.table(combined_ec_fg_full_data_p1_2021, file = "combined_ec_fg_full_data_allyear_p1_2021", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2022

ec_22_p12_in <- read.table("ec_22_p12", header = TRUE) # this is the original file with EC data I had created
ec_22_p12_in <- ec_22_p12_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2022_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2022)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2022 <- ec_22_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2022_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p1_2022, file = "combined_ec_fg_full_data_allyear_p1_2022", sep = "\t", row.names = FALSE, quote = FALSE) 

sum(is.na(combined_ec_fg_full_data_p1_2022$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2022$NEE))

# 2023

ec_23_p12_in <- read.table("ec_23_p12", header = TRUE) # this is the original file with EC data I had created
ec_23_p12_in <- ec_23_p12_in %>%
  mutate(across(
    .cols = c(NEE),                 # all columns EXCEPT these
    .fns = ~ ifelse(DoY >= 273, NA, .),          # set to NA if DoY >= 273
    .names = "{.col}"                            # keep original names
  )) %>% # filtering out all the data before the change from CSAT to IRGASON, which we confirmed had some errors after changing. I need to discard all EC data from october 2023 to April 2024.
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p1p2_2023_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2023)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p1_2023 <- ec_23_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2023_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p1_2023, file = "combined_ec_fg_full_data_allyear_p1_2023", sep = "\t", row.names = FALSE, quote = FALSE) 

sum(is.na(combined_ec_fg_full_data_p1_2023$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2023$NEE))

# 2024

# we are discarding all data from P12 irgason (installed in october 2023) up to the end of the experiment

# ec_24_p12_in <- read.table("ec_24_p12", header = TRUE) # this is the original file with EC data I had created
# ec_24_p12_in <- ec_24_p12_in %>%
#   mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>%
#   mutate(Date = round(Date, digits=2),
#          source_ec = "ec",
#          NEE = as.numeric(NEE))

flux_grad_p1p2_2024_to_merge <- flux_grad_p1p2_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2024)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

# combined_ec_fg_full_data_p1_2024 <- ec_24_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
#   left_join(
#     flux_grad_p1p2_2024_to_merge, 
#     by = c("Date")
#   ) %>% 
#   mutate(
#     source = ifelse(is.na(NEE), source_fg, source_ec),
#     Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
#     H = ifelse(is.na(NEE),` H`, H),
#     NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
# 
#   ) %>%
#   select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

combined_ec_fg_full_data_p1_2024 <- ec_24_p12_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p1p2_2024_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = source_fg,
    Ustar = ` u*`,
    H = ` H`,
    NEE = CO2_flux_umol) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p1_2024, file = "combined_ec_fg_full_data_allyear_p1_2024", sep = "\t", row.names = FALSE, quote = FALSE) 

sum(is.na(combined_ec_fg_full_data_p1_2024$NEE))
sum(!is.na(combined_ec_fg_full_data_p1_2024$NEE))

########################################################

###################################
########## tower p3 - EC data only ##############

units_row <- tibble(
  Year = "-", 
  DoY = "-", 
  Hour = "-", 
  NEE = "umolm-2s-1", 
  LE = "Wm-2", 
  H = "Wm-2", 
  Ustar = "ms-1", 
  Tair = "degC", 
  RH = "%", 
  VPD = "hPa", 
  Rg = "Wm-2"
)

# read raw data

# 2018

# co2
co2_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2018 <- co2_p3_2018 %>% rename(NEE = V1)
sum(is.na(co2_p3_2018))
sum(!is.na(co2_p3_2018))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2018)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n) # the time-stamp has to start at 0.5 in each day and finish at 24 for that same day! This is super important for REddyProc inputs.

time_df_2018 <- data.frame(
  Year = rep(2018, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

#le
le_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2018 <- le_p3_2018 %>% rename(LE = V1)
sum(is.na(le_p3_2018))
sum(!is.na(le_p3_2018))

#h
h_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2018 <- h_p3_2018 %>% rename(H = V1)
sum(is.na(h_p3_2018))
sum(!is.na(h_p3_2018))

#ustar
ustar_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2018 <- ustar_p3_2018 %>% rename(Ustar = V1)
sum(is.na(ustar_p3_2018))
sum(!is.na(ustar_p3_2018))

#tair
tair_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2018 <- tair_p3_2018 %>% rename(Tair = V1)
tair_p3_2018$Tair <-  tair_p3_2018$Tair - 273.15
sum(!is.na(tair_p3_2018))
sum(is.na(tair_p3_2018)) # a lot of NAs remaining in this dataset

# other sources

# temperature from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_temp_2018 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
obs_temp_2018 <- obs_temp_2018 %>% 
  bind_cols(
    time_df_2018
  ) %>% rename(Tair = V1)
sum(is.na(obs_temp_2018$Tair))
sum(!is.na(obs_temp_2018$Tair))

# Replace NA or NaN in tair_p3_2018 with values from obs_temp_2018
tair_p3_2018$Tair[is.na(tair_p3_2018$Tair) | is.nan(tair_p3_2018$Tair)] <- 
  obs_temp_2018$Tair[is.na(tair_p3_2018$Tair) | is.nan(tair_p3_2018$Tair)]
sum(is.na(tair_p3_2018$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2018$V1))
sum(!is.na(rh_p3_2018$V1))
rh_p3_2018 <- rh_p3_2018 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2018", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2018$V1))
sum(!is.na(vpd_p3_2018$V1))
vpd_p3_2018 <- vpd_p3_2018 %>% rename(VPD = V1)

# Rg from other files that patrick had created, and that should be good in terms of time stamp (for other years, I had to puleed from a different source to fix the time stamp)

rg_p3_2018_raw <- fLoadTXTIntoDataframe("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2018_R.txt") # used p12 to keep it consistent with the other tower, as this should be the same across towers.
rg_p3_2018 <- rg_p3_2018_raw %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )
rg_p3_2018 <- rg_p3_2018 %>% rename(Rg = Rg)  # Already named
rg_p3_2018 <- bind_cols(time_df_2018, rg_p3_2018) %>% 
  select(-c(Year, DoY, Hour))

sum(is.na(rg_p3_2018))
sum(!is.na(rg_p3_2018))

# combine all into one data frame
ec_18_p3 <- bind_cols(time_df_2018,co2_p3_2018,le_p3_2018,h_p3_2018,ustar_p3_2018,tair_p3_2018,rh_p3_2018,vpd_p3_2018,rg_p3_2018)
ec_18_p3 <- rbind(units_row, ec_18_p3)

write.table(ec_18_p3, file = "ec_18_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_18_p12_mean <- ec_18_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_18_p3_mean <- ec_18_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_18_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)+
geom_point(data = ec_18_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)
  
# 2019

# filtered
co2_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2019 <- co2_p3_2019 %>% rename(NEE = V1)
sum(is.na(co2_p3_2019))
sum(!is.na(co2_p3_2019))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2019)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2019 <- data.frame(
  Year = rep(2019, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2019 <- le_p3_2019 %>% rename(LE = V1)
sum(is.na(le_p3_2019))
sum(!is.na(le_p3_2019))

h_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2019 <- h_p3_2019 %>% rename(H = V1)
sum(is.na(h_p3_2019))
sum(!is.na(h_p3_2019))

ustar_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2019 <- ustar_p3_2019 %>% rename(Ustar = V1)
sum(is.na(ustar_p3_2019))
sum(!is.na(ustar_p3_2019))

tair_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2019 <- tair_p3_2019 %>% rename(Tair = V1)
tair_p3_2019$Tair <-  tair_p3_2019$Tair - 273.15
sum(is.na(tair_p3_2019)) 
sum(!is.na(tair_p3_2019)) # after an initial run, it seems like the original fieltered vectore file had a lot of gaps, and that may be causing issues with the gap-filling. I will add some additional data to this dataset.

# other sources

# temperature from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_temp_2019 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2019_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2019 <- obs_temp_2019[-1,]
obs_temp_2019 <- obs_temp_2019 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2019
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2019$Tair))
sum(!is.na(obs_temp_2019$Tair))

# Replace NA or NaN in tair_p3_2019 with values from obs_temp_2019
tair_p3_2019$Tair[is.na(tair_p3_2019$Tair) | is.nan(tair_p3_2019$Tair)] <- 
  obs_temp_2019$Tair[is.na(tair_p3_2019$Tair) | is.nan(tair_p3_2019$Tair)]
sum(is.na(tair_p3_2019$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2019", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2019$V1))
rh_p3_2019 <- rh_p3_2019 %>% rename(RH = V1)

# rh from ECCC weather station or other sources, as this variable is critical for reddyproc

obs_rh_2019 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2019_30min_avg.csv", skip = 3, header = TRUE)
obs_rh_2019 <- obs_rh_2019[-1,]
obs_rh_2019 <- obs_rh_2019 %>%
  select(RH_avg) %>%
  mutate(
    RH = as.numeric(RH_avg)
  ) %>% bind_cols(
    time_df_2019
  ) %>% 
  select(!RH_avg)
sum(is.na(obs_rh_2019$RH))
sum(!is.na(obs_rh_2019$RH))

# Replace NA or NaN in tair_p12_2019 with values from obs_temp_2019
rh_p3_2019$RH[is.na(rh_p3_2019$RH) | is.nan(rh_p3_2019$RH)] <- 
  obs_rh_2019$RH[is.na(rh_p3_2019$RH) | is.nan(rh_p3_2019$RH)]
sum(is.na(rh_p3_2019$RH))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2019", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2019$V1))
vpd_p3_2019 <- vpd_p3_2019 %>% rename(VPD = V1)
sum(is.na(vpd_p3_2019))
sum(!is.na(vpd_p3_2019))

# exctracting from a different source (old files) - For Rg i used the same as in p12 to keep it consistent
rg_p12_2019_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2019_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2019_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2019_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2019 <- rg_p3_2019_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2019_raw[[cur_column()]])))
rg_2019 <- rg_2019 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2019 <- bind_cols(time_df_2019, rg_2019) %>%
  select(-Year)

rg_2019 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

sum(is.na(rg_2019$Rg))
sum(!is.na(rg_2019$Rg))

# combine all into one data frame
ec_19_p3 <- bind_cols(time_df_2019,co2_p3_2019,le_p3_2019,h_p3_2019,ustar_p3_2019,tair_p3_2019,rh_p3_2019,vpd_p3_2019) %>%   left_join(rg_2019, by = c("DoY", "Hour"))
ec_19_p3 <- rbind(units_row, ec_19_p3)

write.table(ec_19_p3, file = "ec_19_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_19_p12_mean <- ec_19_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_19_p3_mean <- ec_19_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_19_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)+
  geom_point(data = ec_19_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)

# 2020

# filtered
co2_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2020 <- co2_p3_2020 %>% rename(NEE = V1)
sum(is.na(co2_p3_2020))
sum(!is.na(co2_p3_2020))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2020)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2020 <- data.frame(
  Year = rep(2020, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2020 <- le_p3_2020 %>% rename(LE = V1)
sum(is.na(le_p3_2020))
sum(!is.na(le_p3_2020))

h_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2020 <- h_p3_2020 %>% rename(H = V1)
sum(is.na(h_p3_2020))
sum(!is.na(h_p3_2020))

ustar_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2020 <- ustar_p3_2020 %>% rename(Ustar = V1)
sum(is.na(h_p3_2020))
sum(!is.na(h_p3_2020))

tair_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2020 <- tair_p3_2020 %>% rename(Tair = V1)
tair_p3_2020$Tair <-  tair_p3_2020$Tair - 273.15

sum(is.na(tair_p3_2020)) # after an initial run, it seems like the original fieltered vectore file had a lot of gaps, and that may be causing issues with the gap-filling. I will add some additional data to this dataset.
sum(!is.na(tair_p3_2020))

# other sources

# temperature

obs_temp_2020 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2020_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2020 <- obs_temp_2020[-1,]
obs_temp_2020 <- obs_temp_2020 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2020
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2020$Tair))
sum(!is.na(obs_temp_2020$Tair))

tair_p3_2020<- obs_temp_2020 %>% 
  select(Tair)
sum(!is.na(tair_p3_2020$Tair))
sum(is.na(tair_p3_2020$Tair))

# Replace NA or NaN in tair_p3_2020 with values from obs_temp_2020
tair_p3_2020$Tair[is.na(tair_p3_2020$Tair) | is.nan(tair_p3_2020$Tair)] <- 
  obs_temp_2020$Tair[is.na(tair_p3_2020$Tair) | is.nan(tair_p3_2020$Tair)]
sum(is.na(tair_p3_2020$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2020", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2020$V1))
rh_p3_2020 <- rh_p3_2020 %>% rename(RH = V1)
sum(is.na(rh_p3_2020))
sum(!is.na(rh_p3_2020))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2020", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2020$V1))
vpd_p3_2020 <- vpd_p3_2020 %>% rename(VPD = V1)
sum(is.na(vpd_p3_2020))
sum(!is.na(vpd_p3_2020))

# rg - using the same as in p12 to keep it consistent

rg_p12_2020_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2020_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2020_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2020_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2020 <- rg_p3_2020_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2020_raw[[cur_column()]])))
sum(is.na(rg_p12_2020_raw))
sum(is.na(rg_p3_2020_raw))
sum(is.na(rg_2020))

rg_2020 <- rg_2020 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2020 <- bind_cols(time_df_2020, rg_2020) %>%
  select(-Year)

rg_2020 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_20_p3 <- bind_cols(time_df_2020,co2_p3_2020,le_p3_2020,h_p3_2020,ustar_p3_2020,tair_p3_2020,rh_p3_2020,vpd_p3_2020) %>%
  left_join(rg_2020, by = c("DoY", "Hour"))
ec_20_p3 <- rbind(units_row, ec_20_p3)
sum(is.na(as.numeric(ec_20_p3$Tair)))

write.table(ec_20_p3, file = "ec_20_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_20_p12_mean <- ec_20_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_20_p3_mean <- ec_20_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_20_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)+
  geom_point(data = ec_20_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)

# 2021

# filtered
co2_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2021 <- co2_p3_2021 %>% rename(NEE = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2021)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2021 <- data.frame(
  Year = rep(2021, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2021 <- le_p3_2021 %>% rename(LE = V1)

h_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2021 <- h_p3_2021 %>% rename(H = V1)

ustar_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2021 <- ustar_p3_2021 %>% rename(Ustar = V1)

tair_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2021 <- tair_p3_2021 %>% rename(Tair = V1)
tair_p3_2021$Tair <- tair_p3_2021$Tair - 273.15

sum(is.na(tair_p3_2021))
sum(!is.na(tair_p3_2021))

# other sources

# temperature

obs_temp_2021 <- read.csv("obs_data/measured_co2/data_extraction/temp/EnvCan_Weather_2021_30min_avg.csv", skip = 3, header = TRUE)
obs_temp_2021 <- obs_temp_2021[-1,]
obs_temp_2021 <- obs_temp_2021 %>%
  select(Air_temp_avg) %>%
  mutate(
    Tair = as.numeric(Air_temp_avg)
  ) %>% bind_cols(
    time_df_2021
  ) %>% 
  select(!Air_temp_avg)
sum(is.na(obs_temp_2021$Tair))
sum(!is.na(obs_temp_2021$Tair))

tair_p3_2021 <-obs_temp_2021 %>% 
  select(Tair)

sum(is.na(as.numeric(tair_p3_2021$Tair)))

# Replace NA or NaN in tair_p3_2021 with values from obs_temp_2021
tair_p3_2021$Tair[is.na(tair_p3_2021$Tair) | is.nan(tair_p3_2021$Tair)] <- 
  obs_temp_2021$Tair[is.na(tair_p3_2021$Tair) | is.nan(tair_p3_2021$Tair)]
sum(is.na(tair_p3_2021$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2021", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2021$V1))
rh_p3_2021 <- rh_p3_2021 %>% rename(RH = V1)
sum(is.na(rh_p3_2021))

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2021", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2021$V1))
vpd_p3_2021 <- vpd_p3_2021 %>% rename(VPD = V1)
sum(is.na(vpd_p3_2021))

# rg - using the same as p12 to keep it consistent

rg_p12_2021_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2021_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2021_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2021_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2021 <- rg_p3_2021_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p12_2021_raw[[cur_column()]])))
sum(is.na(rg_p12_2021_raw))
sum(is.na(rg_p3_2021_raw))
sum(is.na(rg_2021))

rg_2021 <- rg_2021 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2021 <- bind_cols(time_df_2021, rg_2021) %>%
  select(-Year)

rg_2021 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_21_p3 <- bind_cols(time_df_2021,co2_p3_2021,le_p3_2021,h_p3_2021,ustar_p3_2021,tair_p3_2021,rh_p3_2021,vpd_p3_2021) %>% 
  left_join(rg_2021, by = c("DoY", "Hour"))
ec_21_p3 <- rbind(units_row, ec_21_p3)

write.table(ec_21_p3, file = "ec_21_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_21_p12_mean <- ec_21_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_21_p3_mean <- ec_21_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_21_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)+
  geom_point(data = ec_21_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)

ggplot()+
  geom_point(data = ec_21_p3, aes(x = DoY, y =as.numeric(Tair)), color = "red", alpha = 0.5)+
  geom_point(data = ec_21_p3, aes(x = DoY, y =as.numeric(Tair)), color = "blue", alpha = 0.5)

# 2022

# filtered
co2_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
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

le_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2022 <- le_p3_2022 %>% rename(LE = V1)

h_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2022 <- h_p3_2022 %>% rename(H = V1)

ustar_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2022 <- ustar_p3_2022 %>% rename(Ustar = V1)

tair_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2022 <- tair_p3_2022 %>% rename(Tair = V1)
tair_p3_2022$Tair <-  tair_p3_2022$Tair - 273.15
sum(is.na(tair_p3_2022))
sum(!is.na(tair_p3_2022))

# other sources

# temperature
obs_temp_2022 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE) # here I decided to use this file instead of the ECCC available in the database becuase the ECCC seemed to have a lot of NAs
obs_temp_2022 <- obs_temp_2022 %>% rename(Tair = V1)
sum(is.na(obs_temp_2022))
sum(!is.na(obs_temp_2022))

# Replace NA or NaN in tair_p3_2022 with values from obs_temp_2022
tair_p3_2022$Tair[is.na(tair_p3_2022$Tair) | is.nan(tair_p3_2022$Tair)] <- 
  obs_temp_2022$Tair[is.na(tair_p3_2022$Tair) | is.nan(tair_p3_2022$Tair)]
sum(is.na(tair_p3_2022$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2022$V1))
sum(!is.na(rh_p3_2022$V1))
rh_p3_2022 <- rh_p3_2022 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2022", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2022$V1))
vpd_p3_2022 <- vpd_p3_2022 %>% rename(VPD = V1)

# rg

rg_p3_2022_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2022_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# rg_p3_2022_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2022_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# the data from p3 looked weird, so I cotinued only with data from p3 for 2022; it did have sufficient datapoints I think

rg_2022 <- rg_p3_2022_raw 
sum(is.na(rg_p3_2022_raw))

rg_2022 <- rg_2022 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )
sum(is.na(rg_2022$Rg))

rg_2022 <- bind_cols(time_df_2022, rg_2022) %>%
  select(-Year)

rg_2022 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_22_p3 <- bind_cols(time_df_2022,co2_p3_2022,le_p3_2022,h_p3_2022,ustar_p3_2022,tair_p3_2022,rh_p3_2022,vpd_p3_2022) %>%
  left_join(rg_2022, by = c("DoY", "Hour"))
ec_22_p3 <- rbind(units_row, ec_22_p3)

write.table(ec_22_p3, file = "ec_22_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_22_p12_mean <- ec_22_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_22_p3_mean <- ec_22_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_22_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)+
  geom_point(data = ec_22_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)

ggplot()+
  geom_point(data = ec_22_p3, aes(x = DoY, y =as.numeric(Tair)), color = "red", alpha = 0.5)+
  geom_point(data = ec_22_p3, aes(x = DoY, y =as.numeric(Tair)), color = "blue", alpha = 0.5)

# 2023

# filtered
co2_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2023 <- co2_p3_2023 %>% rename(NEE = V1)
sum(is.na(co2_p3_2023))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2023)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2023 <- data.frame(
  Year = rep(2023, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2023 <- le_p3_2023 %>% rename(LE = V1)

h_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2023 <- h_p3_2023 %>% rename(H = V1)

ustar_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2023 <- ustar_p3_2023 %>% rename(Ustar = V1)

tair_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2023 <- tair_p3_2023 %>% rename(Tair = V1)
tair_p3_2023$Tair <-  tair_p3_2023$Tair - 273.15

sum(is.na(tair_p3_2023))
sum(!is.na(tair_p3_2023))

# other sources

# temperature
obs_temp_2023 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE) 
obs_temp_2023 <- obs_temp_2023 %>% rename(Tair = V1)
sum(is.na(obs_temp_2023))
sum(!is.na(obs_temp_2023))

# Replace NA or NaN in tair_p3_2023 with values from obs_temp_2023
tair_p3_2023$Tair[is.na(tair_p3_2023$Tair) | is.nan(tair_p3_2023$Tair)] <- 
  obs_temp_2023$Tair[is.na(tair_p3_2023$Tair) | is.nan(tair_p3_2023$Tair)]
sum(is.na(tair_p3_2023$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2023$V1))
rh_p3_2023 <- rh_p3_2023 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2023", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2023$V1))
vpd_p3_2023 <- vpd_p3_2023 %>% rename(VPD = V1)

# rg

# radiation data from the previous source for 2023 looks higher than other years; discussing with Claudia we agreed to use weather station data.

obs_rg_2023 <- read.csv("obs_data/measured_co2/data_extraction/rg/ERS_WX2_weather_2023.csv",  header = TRUE)
colnames(obs_rg_2023) <- obs_rg_2023[1,]
obs_rg_2023 <- obs_rg_2023[-c(1:3),]
rg_2023 <- obs_rg_2023 %>%
  select(RF1_E_Total) %>%
  mutate(Rg_MJ = as.numeric(RF1_E_Total)) %>%
  uncount(weights = 2) %>%  # 🔁 duplicate each row twice, since the original dataset is hourly, not halfhourly.
  mutate(Rg = Rg_MJ * 277.78) %>% # transforming Rj from MJ/m2 to W/m2
  bind_cols(time_df_2023) %>%
  select(-c(RF1_E_Total, Year,Rg_MJ))

# combine all into one data frame
ec_23_p3 <- bind_cols(time_df_2023,co2_p3_2023,le_p3_2023,h_p3_2023,ustar_p3_2023,tair_p3_2023,rh_p3_2023,vpd_p3_2023) %>%
  left_join(rg_2023, by = c("DoY", "Hour"))
ec_23_p3 <- rbind(units_row, ec_23_p3)

write.table(ec_23_p3, file = "ec_23_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_23_p12_mean <- ec_23_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_23_p3_mean <- ec_23_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_23_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)+
  geom_point(data = ec_23_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)

ggplot()+
  geom_point(data = ec_23_p3, aes(x = DoY, y =as.numeric(Tair)), color = "red", alpha = 0.5)+
  geom_point(data = ec_23_p3, aes(x = DoY, y =as.numeric(Tair)), color = "blue", alpha = 0.5)

# 2024

# filtered
co2_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2024 <- co2_p3_2024 %>% rename(NEE = V1)
sum(is.na(co2_p3_2024))

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2024)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df_2024 <- data.frame(
  Year = rep(2024, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

le_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2024 <- le_p3_2024 %>% rename(LE = V1)

h_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2024 <- h_p3_2024 %>% rename(H = V1)

ustar_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2024 <- ustar_p3_2024 %>% rename(Ustar = V1)

tair_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2024 <- tair_p3_2024 %>% rename(Tair = V1)
tair_p3_2024$Tair <-  tair_p3_2024$Tair - 273.15
sum(is.na(tair_p3_2024))
sum(!is.na(tair_p3_2024))

# other sources

# temperature
obs_temp_2024 <- read.table("obs_data/measured_co2/data_extraction/temp/Air_temp_avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE) 
obs_temp_2024 <- obs_temp_2024 %>% rename(Tair = V1)
sum(is.na(obs_temp_2024))
sum(!is.na(obs_temp_2024))

# Replace NA or NaN in tair_p3_2024 with values from obs_temp_2024
tair_p3_2024$Tair[is.na(tair_p3_2024$Tair) | is.nan(tair_p3_2024$Tair)] <- 
  obs_temp_2024$Tair[is.na(tair_p3_2024$Tair) | is.nan(tair_p3_2024$Tair)]
sum(is.na(tair_p3_2024$Tair))

# rh data from FG_data folder in the database (ers4>year) an 'RH_avg' vector

rh_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(rh_p3_2024$V1))
sum(!is.na(rh_p3_2024$V1))
rh_p3_2024 <- rh_p3_2024 %>% rename(RH = V1)

# vpd from ers4/[year]/ERSWeatherStation/Raw_vectors/ there is already a 'VPD_wx' vector

vpd_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/VPD_wx_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
sum(is.na(vpd_p3_2024$V1))
vpd_p3_2024 <- vpd_p3_2024 %>% rename(VPD = V1)

# rg

rg_p12_2024_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2024_p12", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2024_raw <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2024_p3", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

rg_2024 <- rg_p12_2024_raw %>%
  mutate(across(everything(), ~ coalesce(., rg_p3_2024_raw[[cur_column()]]))) # i had initially used first the measurements from p3, and complemented with measurements from plot 3. Then, the overall result for radiation for 2024 was high compared to other years (~180), so I decided to do it the other way; use mostly data from plot 12 and complement with data from plot 3. This seems to show a more reasonable final number, comparable to other years.
sum(is.na(rg_p12_2024_raw))
sum(is.na(rg_p3_2024_raw))
sum(is.na(rg_2024$Rg))
sum(!is.na(rg_2024$Rg))

rg_2024 <- rg_2024 %>% rename(Rg = V1) %>% 
  select(Rg) %>% 
  mutate(
    Rg = as.numeric(Rg)
  )

rg_2024 <- bind_cols(time_df_2024, rg_2024) %>%
  select(-Year)

rg_2024 %>%
  ggplot()+
  geom_point(aes(x = Hour, y =Rg))+
  geom_vline(xintercept = 12.5)

# combine all into one data frame
ec_24_p3 <- bind_cols(time_df_2024,co2_p3_2024,le_p3_2024,h_p3_2024,ustar_p3_2024,tair_p3_2024,rh_p3_2024,vpd_p3_2024)%>% 
  left_join(rg_2024, by = c("DoY", "Hour"))
ec_24_p3 <- rbind(units_row, ec_24_p3)

write.table(ec_24_p3, file = "ec_24_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# comparing temperatures between p12 adn p3

ec_24_p12_mean <- ec_24_p12 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ec_24_p3_mean <- ec_24_p3 %>% 
  group_by(DoY) %>% 
  summarise(
    temp = mean(as.numeric(Tair), na.rm = TRUE)
  )

ggplot()+
  geom_point(data = ec_24_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "red", alpha = 0.5)+
  geom_point(data = ec_24_p3_mean, aes(x = DoY, y =as.numeric(temp)), color = "blue", alpha = 0.5)

ggplot()+
  geom_point(data = ec_24_p3, aes(x = DoY, y =as.numeric(Tair)), color = "blue", alpha = 0.5)+
  geom_point(data = ec_24_p3, aes(x = DoY, y =as.numeric(Tair)), color = "red", alpha = 0.5)

#################################

########################################################
########## tower p3 - EC + FG  data ####################

# 2018

ec_18_p3_in <- read.table("ec_18_p3", header = TRUE) # this is the original file with EC data I had created
ec_18_p3_in <- ec_18_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2018_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2018)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2018 <- ec_18_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2018_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2018, file = "combined_ec_fg_full_data_allyear_p3_2018", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2019

ec_19_p3_in <- read.table("ec_19_p3", header = TRUE) # this is the original file with EC data I had created
ec_19_p3_in <- ec_19_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2019_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2019)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2019 <- ec_19_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2019_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2019, file = "combined_ec_fg_full_data_allyear_p3_2019", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2020

ec_20_p3_in <- read.table("ec_20_p3", header = TRUE) # this is the original file with EC data I had created
ec_20_p3_in <- ec_20_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

sum(is.na(as.numeric(ec_20_p3_in$Tair)))

flux_grad_p3_2020_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2020)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>%  
  select(!Year)

combined_ec_fg_full_data_p3_2020 <- ec_20_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2020_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2020, file = "combined_ec_fg_full_data_allyear_p3_2020", sep = "\t", row.names = FALSE, quote = FALSE) 

sum(is.na(as.numeric(combined_ec_fg_full_data_p3_2020$Tair)))

# 2021

ec_21_p3_in <- read.table("ec_21_p3", header = TRUE) # this is the original file with EC data I had created
ec_21_p3_in <- ec_21_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2021_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2021)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2021 <- ec_21_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2021_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2021, file = "combined_ec_fg_full_data_allyear_p3_2021", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2022

ec_22_p3_in <- read.table("ec_22_p3", header = TRUE) # this is the original file with EC data I had created
ec_22_p3_in <- ec_22_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2022_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2022)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2022 <- ec_22_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2022_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2022, file = "combined_ec_fg_full_data_allyear_p3_2022", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2023

ec_23_p3_in <- read.table("ec_23_p3", header = TRUE) # this is the original file with EC data I had created
ec_23_p3_in <- ec_23_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2023_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2023)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2023 <- ec_23_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2023_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2023, file = "combined_ec_fg_full_data_allyear_p3_2023", sep = "\t", row.names = FALSE, quote = FALSE) 

# 2024

ec_24_p3_in <- read.table("ec_24_p3", header = TRUE) # this is the original file with EC data I had created
ec_24_p3_in <- ec_24_p3_in %>%
  mutate(Date = as.numeric(DoY) + (as.numeric(Hour) / 24)) %>% 
  mutate(Date = round(Date, digits=2),
         source_ec = "ec",
         NEE = as.numeric(NEE))

flux_grad_p3_2024_to_merge <- flux_grad_p3p4_2018_2024 %>% # bringing the flux gradient data, and filtering for the specific days to gapfill
  select(Year, Date, CO2_flux_umol, ` u*`, ` H`) %>% 
  mutate(DOY = floor(Date) + 1) %>%
  filter((Year == 2024)) %>% 
  mutate(Date = round(Date, 2),
         source_fg = "fg") %>% 
  select(!Year)

combined_ec_fg_full_data_p3_2024 <- ec_24_p3_in %>% # merging all the datasets, and replacing the values only for the dates where the gaps exist
  left_join(
    flux_grad_p3_2024_to_merge, 
    by = c("Date")
  ) %>% 
  mutate(
    source = ifelse(is.na(NEE), source_fg, source_ec),
    Ustar = ifelse(is.na(NEE), ` u*`, Ustar),
    H = ifelse(is.na(NEE),` H`, H),
    NEE = ifelse(is.na(NEE), CO2_flux_umol, NEE)
  ) %>%
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg, source)

write.table(combined_ec_fg_full_data_p3_2024, file = "combined_ec_fg_full_data_allyear_p3_2024", sep = "\t", row.names = FALSE, quote = FALSE) 

########################################################

################# data comparison #####################

# comparing air temperature with soil temperature

obs_soil_temp <- read_excel("obs_data/measured_soil_temp/temp_2018_2024.xlsx")
obs_soil_temp_p12 <- obs_soil_temp %>% 
  filter(group ==  "P1_P2")
obs_soil_temp_p34 <- obs_soil_temp %>% 
  filter(group ==  "P3_P4")


str(combined_ec_fg_full_data_p1_2020)
air_temp<- EddyData_p1 %>% 
  select(Year, DoY, Hour, Tair) %>%
  mutate(Tair = as.numeric(Tair)) %>% 
  group_by(DoY, Year) %>% 
  summarise(
    tair = mean(Tair, na.rm = TRUE)
  ) %>% 
  mutate(doy = as.numeric(DoY),
         year = Year)
str(air_temp)

obs_soil_temp_p34%>% 
  left_join(air_temp, by = c("doy", "year")) %>% 
  select(year, doy,temp_5_mean, tair) %>% 
  filter(year==2024) %>%
  ggplot()+
  geom_line(aes(x=doy, y=tair), color = "blue")+
  geom_line(aes(x=doy, y=temp_5_mean ), color = "darkgreen")+
  theme_bw()+
  facet_wrap(year~., ncol=2)


#################################
############# old ###############
########## tower p3 - New ##############

units_row <- tibble(
  Year = "-", 
  DoY = "-", 
  Hour = "-", 
  NEE = "umolm-2s-1", 
  LE = "Wm-2", 
  H = "Wm-2", 
  Ustar = "ms-1", 
  Tair = "degC", 
  RH = "%", 
  VPD = "hPa", 
  Rg = "Wm-2"
)

# read raw data

# 2020

# filtered
co2_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2020 <- co2_p3_2020 %>% rename(NEE = V1)

le_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2020 <- le_p3_2020 %>% rename(LE = V1)

h_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2020 <- h_p3_2020 %>% rename(H = V1)

ustar_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2020 <- ustar_p3_2020 %>% rename(Ustar = V1)

tair_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2020 <- tair_p3_2020 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2020 <- rh_p12_2020
vpd_p3_2020 <- vpd_p12_2020

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2020)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2020, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2020_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2020_R.txt", header = TRUE)
rg_p3_2020 <- rg_p3_2020_raw %>% 
  slice(-c(1)) %>% 
  select(Rg) 
rg_p3_2020 <- rg_p3_2020 %>% rename(Rg = Rg)  # Already named

# combine all into one data frame
ec_18_p3 <- bind_cols(time_df,co2_p3_2020,le_p3_2020,h_p3_2020,ustar_p3_2020,tair_p3_2020,rh_p3_2020,vpd_p3_2020,rg_p3_2020)
ec_18_p3$Tair <-  ec_18_p3$Tair - 273.15 # converting temperature from kelvin to celsius
ec_18_p3 <- rbind(units_row, ec_18_p3)

write.table(ec_18_p3, file = "ec_18_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2019

# filtered
co2_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2019 <- co2_p3_2019 %>% rename(NEE = V1)

le_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2019 <- le_p3_2019 %>% rename(LE = V1)

h_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2019 <- h_p3_2019 %>% rename(H = V1)

ustar_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2019 <- ustar_p3_2019 %>% rename(Ustar = V1)

tair_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2019 <- tair_p3_2019 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2019 <- rh_p12_2019
vpd_p3_2019 <- vpd_p12_2019

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2019)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2019, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2019_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2019_R.txt", header = TRUE)
rg_p3_2019 <- rg_p3_2019_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p3_2019 <- rg_p3_2019 %>% rename(Rg = Rg)  # Already named
nrow(rg_p3_2019)

# combine all into one data frame
ec_19_p3 <- bind_cols(time_df,co2_p3_2019,le_p3_2019,h_p3_2019,ustar_p3_2019,tair_p3_2019,rh_p3_2019,vpd_p3_2019) %>% 
  left_join(rg_p3_2019, by = c("DoY", "Hour"))
ec_19_p3$Tair <-  ec_19_p3$Tair - 273.15
ec_19_p3 <- rbind(units_row, ec_19_p3)

write.table(ec_19_p3, file = "ec_19_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2020

# filtered
co2_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2020 <- co2_p3_2020 %>% rename(NEE = V1)

le_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2020 <- le_p3_2020 %>% rename(LE = V1)

h_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2020 <- h_p3_2020 %>% rename(H = V1)

ustar_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2020 <- ustar_p3_2020 %>% rename(Ustar = V1)

tair_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2020 <- tair_p3_2020 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2020 <- rh_p12_2020
vpd_p3_2020 <- vpd_p12_2020

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2020)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2020, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2020_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2020_R.txt", header = TRUE)
rg_p3_2020 <- rg_p3_2020_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p3_2020 <- rg_p3_2020 %>% rename(Rg = Rg)  # Already named
nrow(rg_p3_2020)

# combine all into one data frame
ec_20_p3 <- bind_cols(time_df,co2_p3_2020,le_p3_2020,h_p3_2020,ustar_p3_2020,tair_p3_2020,rh_p3_2020,vpd_p3_2020) %>% 
  left_join(rg_p3_2020, by = c("DoY", "Hour"))
ec_20_p3$Tair <-  ec_20_p3$Tair - 273.15
ec_20_p3 <- rbind(units_row, ec_20_p3)

write.table(ec_20_p3, file = "ec_20_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2021

# filtered
co2_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2021 <- co2_p3_2021 %>% rename(NEE = V1)

le_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2021 <- le_p3_2021 %>% rename(LE = V1)

h_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2021 <- h_p3_2021 %>% rename(H = V1)

ustar_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2021 <- ustar_p3_2021 %>% rename(Ustar = V1)

tair_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2021 <- tair_p3_2021 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2021 <- rh_p12_2021
vpd_p3_2021 <- vpd_p12_2021

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2021)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2021, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2021_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2021_R.txt", header = TRUE)
rg_p3_2021 <- rg_p3_2021_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p3_2021 <- rg_p3_2021 %>% rename(Rg = Rg)  # Already named
nrow(rg_p3_2021)

# combine all into one data frame
ec_21_p3 <- bind_cols(time_df,co2_p3_2021,le_p3_2021,h_p3_2021,ustar_p3_2021,tair_p3_2021,rh_p3_2021,vpd_p3_2021) %>% 
  left_join(rg_p3_2021, by = c("DoY", "Hour"))
ec_21_p3$Tair <- ec_21_p3$Tair - 273.15
ec_21_p3 <- rbind(units_row, ec_21_p3)

write.table(ec_21_p3, file = "ec_21_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2022

# filtered
co2_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2022 <- co2_p3_2022 %>% rename(NEE = V1)

le_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2022 <- le_p3_2022 %>% rename(LE = V1)

h_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2022 <- h_p3_2022 %>% rename(H = V1)

ustar_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2022 <- ustar_p3_2022 %>% rename(Ustar = V1)

tair_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2022 <- tair_p3_2022 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2022 <- rh_p12_2022
vpd_p3_2022 <- vpd_p12_2022

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2022_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2022_R.txt", header = TRUE)
rg_p3_2022 <- rg_p3_2022_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p3_2022 <- rg_p3_2022 %>% rename(Rg = Rg)  # Already named
nrow(rg_p3_2022)

# combine all into one data frame
ec_22_p3 <- bind_cols(time_df,co2_p3_2022,le_p3_2022,h_p3_2022,ustar_p3_2022,tair_p3_2022,rh_p3_2022,vpd_p3_2022) %>% 
  left_join(rg_p3_2022, by = c("DoY", "Hour"))
ec_22_p3$Tair <-  ec_22_p3$Tair - 273.15
ec_22_p3 <- rbind(units_row, ec_22_p3)

write.table(ec_22_p3, file = "ec_22_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2023

# filtered
co2_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2023 <- co2_p3_2023 %>% rename(NEE = V1)

le_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2023 <- le_p3_2023 %>% rename(LE = V1)

h_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2023 <- h_p3_2023 %>% rename(H = V1)

ustar_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2023 <- ustar_p3_2023 %>% rename(Ustar = V1)

tair_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2023 <- tair_p3_2023 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2023 <- rh_p12_2023
vpd_p3_2023 <- vpd_p12_2023

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2023)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2023, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2023_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2023_R.txt", header = TRUE)
rg_p3_2023 <- rg_p3_2023_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p3_2023 <- rg_p3_2023 %>% rename(Rg = Rg)  # Already named
nrow(rg_p3_2023)

# combine all into one data frame
ec_23_p3 <- bind_cols(time_df,co2_p3_2023,le_p3_2023,h_p3_2023,ustar_p3_2023,tair_p3_2023,rh_p3_2023,vpd_p3_2023) %>% 
  left_join(rg_p3_2023, by = c("DoY", "Hour"))
ec_23_p3$Tair <-  ec_23_p3$Tair - 273.15
ec_23_p3 <- rbind(units_row, ec_23_p3)

write.table(ec_23_p3, file = "ec_23_p3", sep = "\t", row.names = FALSE, quote = FALSE)

# 2024

# filtered
co2_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2024 <- co2_p3_2024 %>% rename(NEE = V1)

le_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2024 <- le_p3_2024 %>% rename(LE = V1)

h_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2024 <- h_p3_2024 %>% rename(H = V1)

ustar_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2024 <- ustar_p3_2024 %>% rename(Ustar = V1)

tair_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2024 <- tair_p3_2024 %>% rename(Tair = V1)

# other sources
# since the values were averages, we are using the same for p12 and p3
rh_p3_2024 <- rh_p12_2024
vpd_p3_2024 <- vpd_p12_2024

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2024)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2024, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)

rg_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p3_2024 <- rg_p3_2024 %>% rename(Rg = V1)
nrow(rg_p3_2024)

# combine all into one data frame
ec_24_p3 <- bind_cols(time_df,co2_p3_2024,le_p3_2024,h_p3_2024,ustar_p3_2024,tair_p3_2024,rh_p3_2024,vpd_p3_2024, rg_p3_2024)
ec_24_p3$Tair <-  ec_24_p3$Tair - 273.15
ec_24_p3 <- rbind(units_row, ec_24_p3)

write.table(ec_24_p3, file = "ec_24_p3", sep = "\t", row.names = FALSE, quote = FALSE)

########## tower p12 - OLD ##############

# read in file from eddy pro

ec_jan_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p12/eddypro_Plot12_Jan_2024_full_output_2024-06-13T152131_adv.csv", header = FALSE)
colnames(ec_jan_24_p1) <- ec_jan_24_p1[2, ]
ec_jan_24_p1 <- ec_jan_24_p1[-c(1:3), ]

ec_feb_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p12/eddypro_Plot12_Feb_2024_full_output_2024-06-13T153100_adv.csv", header = FALSE)
colnames(ec_feb_24_p1) <- ec_feb_24_p1[2, ]
ec_feb_24_p1 <- ec_feb_24_p1[-c(1:3), ]

ec_mar_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p12/eddypro_Plot12_Mar_2024_full_output_2024-08-09T131913_adv.csv", header = FALSE)
colnames(ec_mar_24_p1) <- ec_mar_24_p1[2, ]
ec_mar_24_p1 <- ec_mar_24_p1[-c(1:3), ]

ec_apr_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p12/eddypro_Plot12_Apr_2024_full_output_2024-08-09T133421_adv.csv", header = FALSE)
colnames(ec_apr_24_p1) <- ec_apr_24_p1[2, ]
ec_apr_24_p1 <- ec_apr_24_p1[-c(1:3), ]

ec_may_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p12/eddypro_Plot12_May_2024_full_output_2024-08-09T135136_adv.csv", header = FALSE)
colnames(ec_may_24_p1) <- ec_may_24_p1[2, ]
ec_may_24_p1 <- ec_may_24_p1[-c(1:3), ]

ec_24_p1 <- bind_rows(ec_jan_24_p1, ec_feb_24_p1, ec_mar_24_p1, ec_apr_24_p1, ec_may_24_p1)

# read in file with Rg
rg_24_p1 <- read.csv("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2024", header = FALSE)
colnames(rg_24_p1) <- "Rg"

# creating doy and hour column

# I explored multiple options to see how the values were set up; after looking at the results, the one that seemed to match the values was:

# considering all days start at 0 Hour and finish at 23.5 (this is different from what I see in other EC REddyProc input files)

rg_24_p1 <- rg_24_p1 %>%
  mutate(
    DoY = rep(1:366, each = 48)[1:n()],  # Repeat DoY for each day (48 values per day)
    Hour = rep(seq(0, 23.5, by = 0.5), times = 366)[1:n()]  # Sequence of hours
  )

# other options I had considered

# considering all days start at 0.5 Hour and finish at 24

# rg_24_p1 <- rg_24_p1 %>%
#   mutate(
#     DoY = rep(1:366, each = 48)[1:n()],  # Repeat DoY for each day (48 values per day)
#     Hour = rep(seq(0.5, 24, by = 0.5), times = 366)[1:n()]  # Sequence of hours
#   )

# considering the first day starts at 0.5 and finished at 23.5 and all other days start at 0 and go up to Hour 23.5

# rg_24_p1 <- rg_24_p1 %>%
#   mutate(
#     DoY = rep(1:366, each = 48)[1:n()],  # Repeat DoY for each day (48 values per day)
#     Hour = rep(seq(0.5, 24, by = 0.5), times = 366)[1:n()]  # Sequence of hours
#   )


# combine files
ec_24_p1 <- ec_24_p1 %>% 
  select(date, time, DOY, H, LE, co2_flux, air_temperature, RH, VPD, `u*`) %>% 
  mutate(
    Year = year(mdy(date)),
    DoY = yday(mdy(date)),
    Hour = hour(hm(time)) + minute(hm(time)) / 60,
    NEE = co2_flux,
    Ustar = `u*`,
    Tair = ifelse(air_temperature == "-9999", "-9999", as.numeric(air_temperature) - 273.15),
    VPD = as.numeric(VPD)/100
  ) %>% 
  left_join(rg_24_p1, by = c("DoY", "Hour")) %>% 
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)

# export file

write.table(ec_24_p1, "P12_tower_2024_R.txt", row.names = FALSE, quote = FALSE, sep = "\t")

######## tower p3 - OLD ###############

# read in file from eddy pro

# first file

ec_jan_mar_24_p3 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p3/eddypro_Plot3_Jan_Mar_2024_full_output_2024-08-12T094614_adv.csv", header = FALSE)
colnames(ec_jan_mar_24_p3) <- ec_jan_mar_24_p3[2, ]
ec_jan_mar_24_p3 <- ec_jan_mar_24_p3[-c(1:3), ]
ec_jan_mar_24_p3 <- ec_jan_mar_24_p3 %>% 
  filter(as.numeric(DOY) < 80) # seems like the first dataset that was generated with eddypro, has full day data up to doy = 79
ec_jan_mar_24_p3 <- ec_jan_mar_24_p3 %>%
  mutate(date = format(mdy(date), "%Y%m%d"))

# second file
# some days were missing, so I tried temporarily fixing it this way.
date_seq <- seq(ymd("2024-01-01"), ymd("2025-05-31"), by = "day")
time_seq <- format(seq(from = as.POSIXct("00:00", format = "%H:%M"),
                       to = as.POSIXct("23:30", format = "%H:%M"),
                       by = "30 min"), "%H:%M")
date_time_df <- expand.grid(date = date_seq, time = time_seq) %>% 
  as_tibble()%>%
  mutate(DOY = yday(date))
date_time_df <- date_time_df %>% 
  filter(!as.numeric(DOY) < 80) %>% 
  select(-DOY)
date_time_df <- date_time_df %>%
  mutate(date = format(ymd(date), "%Y%m%d"))

ec_mar_may_24_p3 <- read.csv("obs_data/measured_co2/data_extraction/eddy_pro_output/p3/eddypro_eddypro_Plot3_MarchtoMay_full_output_2025-03-04T123521_adv.csv", header = FALSE)
colnames(ec_mar_may_24_p3) <- ec_mar_may_24_p3[2, ]
ec_mar_may_24_p3 <- ec_mar_may_24_p3[-c(1:3), ]
ec_mar_may_24_p3 <- ec_mar_may_24_p3 %>% 
  filter(!as.numeric(DOY) < 80) # I am using data from this second dataset starting in doy = 80
ec_mar_may_24_p3 <- ec_mar_may_24_p3 %>%
  mutate(date = format(ymd(date), "%Y%m%d"))

date_time_df <- date_time_df %>% 
  left_join(ec_mar_may_24_p3, by = c("date", "time"))

common_cols <- intersect(names(ec_jan_mar_24_p3), names(ec_mar_may_24_p3))
ec_jan_mar_24_p3 <- ec_jan_mar_24_p3 %>% select(all_of(common_cols))
ec_mar_may_24_p3 <- ec_mar_may_24_p3 %>% select(all_of(common_cols))

ec_24_p3 <- bind_rows(ec_jan_mar_24_p3, date_time_df)
ec_24_p3$DoY <- ec_24_p3$DOY

# read in file with Rg
rg_24_p3 <- read.csv("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2024", header = FALSE)
colnames(rg_24_p3) <- "Rg"

# creating doy and hour column

# considering all days start at 0 Hour and finish at 23.5 (this is different from what I see in other EC REddyProc input files)

rg_24_p3 <- rg_24_p3 %>%
  mutate(
    DoY = rep(1:366, each = 48)[1:n()],  # Repeat DoY for each day (48 values per day)
    Hour = rep(seq(0, 23.5, by = 0.5), times = 366)[1:n()]  # Sequence of hours
  )

# combine files
ec_24_p3$date <- as.Date(ec_24_p3$date, format = "%Y%m%d")
ec_24_p3$time <- format(strptime(ec_24_p3$time, format = "%H:%M"), "%H:%M")


ec_24_p3_final <- ec_24_p3 %>% 
  select(date, time, DOY, H, LE, co2_flux, air_temperature, RH, VPD, `u*`) %>% 
  mutate(
    Year = year(ymd(date)), # depending on the format of the date column in the input file, this ymd() (year, month, date) may need to be changed by "dmy()" or something similar; same below.
    DoY = yday(ymd(date)),
    Hour = hour(hm(time)) + minute(hm(time)) / 60,
    NEE = co2_flux,
    Ustar = `u*`,
    Tair = ifelse(air_temperature == "-9999", "-9999", as.numeric(air_temperature) - 273.15),
    VPD = as.numeric(VPD)/100,
    date  = date
  ) %>% 
  left_join(rg_24_p3, by = c("DoY", "Hour")) %>% 
  arrange(date, Hour) %>% 
  select(Year, DoY, Hour, NEE, LE, H, Ustar, Tair, RH, VPD, Rg)


# export file

write.table(ec_24_p3_final, "P3_tower_2024_R.txt", row.names = FALSE, quote = FALSE, sep = "\t")

#################################

