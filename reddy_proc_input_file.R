# creation of reddyproc input based on eddyprot full output
# 03/03/2024
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
###################################

###################################
########## tower p12 - New ##############

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

# filtered
co2_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2018 <- co2_p12_2018 %>% rename(NEE = V1)

le_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2018 <- le_p12_2018 %>% rename(LE = V1)

h_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2018 <- h_p12_2018 %>% rename(H = V1)

ustar_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2018 <- ustar_p12_2018 %>% rename(Ustar = V1)

tair_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2018 <- tair_p12_2018 %>% rename(Tair = V1)

# raw vectors
rh_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2018 <- rh_p12_2018 %>% rename(RH = V1)

vpd_p12_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2018 <- vpd_p12_2018 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2018)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2018, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2018_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2018_R.txt", header = TRUE)
rg_p12_2018 <- rg_p12_2018_raw %>% 
  slice(-c(1)) %>% 
  select(Rg) 
rg_p12_2018 <- rg_p12_2018 %>% rename(Rg = Rg)  # Already named

# combine all into one data frame
ec_18_p12 <- bind_cols(time_df,co2_p12_2018,le_p12_2018,h_p12_2018,ustar_p12_2018,tair_p12_2018,rh_p12_2018,vpd_p12_2018,rg_p12_2018)
ec_18_p12$Tair <-  ec_18_p12$Tair - 273.15 # converting temperature from kelvin to celsius
ec_18_p12 <- rbind(units_row, ec_18_p12)

write.table(ec_18_p12, file = "ec_18_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2019

# filtered
co2_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2019 <- co2_p12_2019 %>% rename(NEE = V1)

le_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2019 <- le_p12_2019 %>% rename(LE = V1)

h_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2019 <- h_p12_2019 %>% rename(H = V1)

ustar_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2019 <- ustar_p12_2019 %>% rename(Ustar = V1)

tair_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2019 <- tair_p12_2019 %>% rename(Tair = V1)

# raw vectors
rh_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2019 <- rh_p12_2019 %>% rename(RH = V1)

vpd_p12_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2019 <- vpd_p12_2019 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2019)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2019, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2019_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2019_R.txt", header = TRUE)
rg_p12_2019 <- rg_p12_2019_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p12_2019 <- rg_p12_2019 %>% rename(Rg = Rg)  # Already named
nrow(rg_p12_2019)

# combine all into one data frame
ec_19_p12 <- bind_cols(time_df,co2_p12_2019,le_p12_2019,h_p12_2019,ustar_p12_2019,tair_p12_2019,rh_p12_2019,vpd_p12_2019) %>% 
  left_join(rg_p12_2019, by = c("DoY", "Hour"))
ec_19_p12$Tair <-  ec_19_p12$Tair - 273.15
ec_19_p12 <- rbind(units_row, ec_19_p12)

write.table(ec_19_p12, file = "ec_19_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2020

# filtered
co2_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2020 <- co2_p12_2020 %>% rename(NEE = V1)

le_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2020 <- le_p12_2020 %>% rename(LE = V1)

h_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2020 <- h_p12_2020 %>% rename(H = V1)

ustar_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2020 <- ustar_p12_2020 %>% rename(Ustar = V1)

tair_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2020 <- tair_p12_2020 %>% rename(Tair = V1)

# raw vectors
rh_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2020 <- rh_p12_2020 %>% rename(RH = V1)

vpd_p12_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2020 <- vpd_p12_2020 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2020)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2020, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2020_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2020_R.txt", header = TRUE)
rg_p12_2020 <- rg_p12_2020_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p12_2020 <- rg_p12_2020 %>% rename(Rg = Rg)  # Already named
nrow(rg_p12_2020)

# combine all into one data frame
ec_20_p12 <- bind_cols(time_df,co2_p12_2020,le_p12_2020,h_p12_2020,ustar_p12_2020,tair_p12_2020,rh_p12_2020,vpd_p12_2020) %>% 
  left_join(rg_p12_2020, by = c("DoY", "Hour"))
ec_20_p12$Tair <-  ec_20_p12$Tair - 273.15
ec_20_p12 <- rbind(units_row, ec_20_p12)

write.table(ec_20_p12, file = "ec_20_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2021

# filtered
co2_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2021 <- co2_p12_2021 %>% rename(NEE = V1)

le_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2021 <- le_p12_2021 %>% rename(LE = V1)

h_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2021 <- h_p12_2021 %>% rename(H = V1)

ustar_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2021 <- ustar_p12_2021 %>% rename(Ustar = V1)

tair_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2021 <- tair_p12_2021 %>% rename(Tair = V1)

# raw vectors
rh_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2021 <- rh_p12_2021 %>% rename(RH = V1)

vpd_p12_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2021 <- vpd_p12_2021 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2021)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2021, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2021_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2021_R.txt", header = TRUE)
rg_p12_2021 <- rg_p12_2021_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p12_2021 <- rg_p12_2021 %>% rename(Rg = Rg)  # Already named
nrow(rg_p12_2021)

# combine all into one data frame
ec_21_p12 <- bind_cols(time_df,co2_p12_2021,le_p12_2021,h_p12_2021,ustar_p12_2021,tair_p12_2021,rh_p12_2021,vpd_p12_2021) %>% 
  left_join(rg_p12_2021, by = c("DoY", "Hour"))
ec_21_p12$Tair <- ec_21_p12$Tair - 273.15
ec_21_p12 <- rbind(units_row, ec_21_p12)

write.table(ec_21_p12, file = "ec_21_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2022

# filtered
co2_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2022 <- co2_p12_2022 %>% rename(NEE = V1)

le_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2022 <- le_p12_2022 %>% rename(LE = V1)

h_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2022 <- h_p12_2022 %>% rename(H = V1)

ustar_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2022 <- ustar_p12_2022 %>% rename(Ustar = V1)

tair_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2022 <- tair_p12_2022 %>% rename(Tair = V1)

# raw vectors
rh_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2022 <- rh_p12_2022 %>% rename(RH = V1)

vpd_p12_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2022 <- vpd_p12_2022 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2022)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2022, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2022_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2022_R.txt", header = TRUE)
rg_p12_2022 <- rg_p12_2022_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p12_2022 <- rg_p12_2022 %>% rename(Rg = Rg)  # Already named
nrow(rg_p12_2022)

# combine all into one data frame
ec_22_p12 <- bind_cols(time_df,co2_p12_2022,le_p12_2022,h_p12_2022,ustar_p12_2022,tair_p12_2022,rh_p12_2022,vpd_p12_2022) %>% 
  left_join(rg_p12_2022, by = c("DoY", "Hour"))
ec_22_p12$Tair <-  ec_22_p12$Tair - 273.15
ec_22_p12 <- rbind(units_row, ec_22_p12)

write.table(ec_22_p12, file = "ec_22_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2023

# filtered
co2_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2023 <- co2_p12_2023 %>% rename(NEE = V1)

le_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2023 <- le_p12_2023 %>% rename(LE = V1)

h_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2023 <- h_p12_2023 %>% rename(H = V1)

ustar_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2023 <- ustar_p12_2023 %>% rename(Ustar = V1)

tair_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2023 <- tair_p12_2023 %>% rename(Tair = V1)

# raw vectors
rh_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2023 <- rh_p12_2023 %>% rename(RH = V1)

vpd_p12_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2023 <- vpd_p12_2023 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2023)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2023, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p12_2023_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/P12_tower_2023_R.txt", header = TRUE)
rg_p12_2023 <- rg_p12_2023_raw %>% 
  slice(-c(1)) %>% 
  select(Rg, DoY, Hour) %>% 
  mutate(
    Rg = as.numeric(Rg),
    DoY = as.integer(DoY),
    Hour = as.numeric(Hour)
  )
rg_p12_2023 <- rg_p12_2023 %>% rename(Rg = Rg)  # Already named
nrow(rg_p12_2023)

# combine all into one data frame
ec_23_p12 <- bind_cols(time_df,co2_p12_2023,le_p12_2023,h_p12_2023,ustar_p12_2023,tair_p12_2023,rh_p12_2023,vpd_p12_2023) %>% 
  left_join(rg_p12_2023, by = c("DoY", "Hour"))
ec_23_p12$Tair <-  ec_23_p12$Tair - 273.15
ec_23_p12 <- rbind(units_row, ec_23_p12)

write.table(ec_23_p12, file = "ec_23_p12", sep = "\t", row.names = FALSE, quote = FALSE)

# 2024

# filtered
co2_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p12_2024 <- co2_p12_2024 %>% rename(NEE = V1)

le_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p12_2024 <- le_p12_2024 %>% rename(LE = V1)

h_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p12_2024 <- h_p12_2024 %>% rename(H = V1)

ustar_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p12_2024 <- ustar_p12_2024 %>% rename(Ustar = V1)

tair_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p12_2024 <- tair_p12_2024 %>% rename(Tair = V1)

# raw vectors
rh_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2024 <- rh_p12_2024 %>% rename(RH = V1)

vpd_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/P12_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p12_2024 <- vpd_p12_2024 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p12_2024)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2024, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)

rg_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rg_p12_2024 <- rg_p12_2024 %>% rename(Rg = V1)
nrow(rg_p12_2024)

# combine all into one data frame
ec_24_p12 <- bind_cols(time_df,co2_p12_2024,le_p12_2024,h_p12_2024,ustar_p12_2024,tair_p12_2024,rh_p12_2024,vpd_p12_2024, rg_p12_2024)
ec_24_p12$Tair <-  ec_24_p12$Tair - 273.15
ec_24_p12 <- rbind(units_row, ec_24_p12)

write.table(ec_24_p12, file = "ec_24_p12", sep = "\t", row.names = FALSE, quote = FALSE)

#################################

###################################
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

# 2018

# filtered
co2_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/co2_flux_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
co2_p3_2018 <- co2_p3_2018 %>% rename(NEE = V1)

le_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/LE_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
le_p3_2018 <- le_p3_2018 %>% rename(LE = V1)

h_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/H_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
h_p3_2018 <- h_p3_2018 %>% rename(H = V1)

ustar_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/u_star_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
ustar_p3_2018 <- ustar_p3_2018 %>% rename(Ustar = V1)

tair_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Filtered_vectors/air_temperature_filtered", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
tair_p3_2018 <- tair_p3_2018 %>% rename(Tair = V1)

# raw vectors
rh_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2018 <- rh_p3_2018 %>% rename(RH = V1)

vpd_p3_2018 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2018/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2018 <- vpd_p3_2018 %>% rename(VPD = V1)

# create time columns (assuming 48 half-hourly measurements per day)
n <- nrow(co2_p3_2018)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2018, n),
  DoY = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# exctracting from a different source (old files)
rg_p3_2018_raw <- read.table("obs_data/measured_co2/data_extraction/shared_by_pat_ec/p3_tower_2018_R.txt", header = TRUE)
rg_p3_2018 <- rg_p3_2018_raw %>% 
  slice(-c(1)) %>% 
  select(Rg) 
rg_p3_2018 <- rg_p3_2018 %>% rename(Rg = Rg)  # Already named

# combine all into one data frame
ec_18_p3 <- bind_cols(time_df,co2_p3_2018,le_p3_2018,h_p3_2018,ustar_p3_2018,tair_p3_2018,rh_p3_2018,vpd_p3_2018,rg_p3_2018)
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

# raw vectors
rh_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2019 <- rh_p3_2019 %>% rename(RH = V1)

vpd_p3_2019 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2019/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2019 <- vpd_p3_2019 %>% rename(VPD = V1)

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

# raw vectors
rh_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2020 <- rh_p3_2020 %>% rename(RH = V1)

vpd_p3_2020 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2020/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2020 <- vpd_p3_2020 %>% rename(VPD = V1)

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

# raw vectors
rh_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2021 <- rh_p3_2021 %>% rename(RH = V1)

vpd_p3_2021 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2021/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2021 <- vpd_p3_2021 %>% rename(VPD = V1)

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

# raw vectors
rh_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2022 <- rh_p3_2022 %>% rename(RH = V1)

vpd_p3_2022 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2022/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2022 <- vpd_p3_2022 %>% rename(VPD = V1)

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

# raw vectors
rh_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2023 <- rh_p3_2023 %>% rename(RH = V1)

vpd_p3_2023 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2023/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2023 <- vpd_p3_2023 %>% rename(VPD = V1)

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

# raw vectors
rh_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Raw_vectors/RH", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p3_2024 <- rh_p3_2024 %>% rename(RH = V1)

vpd_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/2024/p3_tower/Raw_vectors/VPD", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
vpd_p3_2024 <- vpd_p3_2024 %>% rename(VPD = V1)

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

#################################


###################################
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

#################################

#################################
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
