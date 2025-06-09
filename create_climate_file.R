# 25/10/2024
# file to extract climate data from Elora weather station, to use in DNDC modeling

##########################
######### DNDC 2023 ###########

# for 2023 I used a file called ERS_WX2_weather_2023, which contained all the variables of interest.

# import main file with all variables

obs_climate <- read.csv("Z:/E26/database/ers4/2023/ERSWeatherStation/ERS_WX2_weather_2023.csv", header = FALSE)
colnames(obs_climate) <- obs_climate[2, ]
obs_climate <- obs_climate[-c(1:4),]

# select the variables of interest

obs_climate1 <- select(obs_climate, TIMESTAMP, m1_Humidity, `h1_Temp(1)`, `m60_Wind_Vector(1)`, m60_TBRG_Tot, RF1_E_Total)
obs_climate1 <- obs_climate1 %>%
  mutate(date = as.Date(parse_date_time(TIMESTAMP, orders = "mdy HM")))
obs_climate1 <- obs_climate1 %>%
  mutate(across(-c(TIMESTAMP,date), as.numeric))
head(obs_climate1$TIMESTAMP, 10)  # Check the first 10 values of the TIMESTAMP column

# estimate max temp

obs_max_temp_c <- obs_climate1 %>% 
  group_by(date) %>% 
  summarise(
    obs_max_temp_c = round(max(`h1_Temp(1)`, na.rm = TRUE), digits = 2)
  )

# estimate min temp

obs_min_temp_c <- obs_climate1 %>% 
  group_by(date) %>% 
  summarise(
    obs_min_temp_c = round(min(`h1_Temp(1)`, na.rm = TRUE), digits = 2)
  )

# estimate rh

obs_rh_pct <- obs_climate1 %>% 
  group_by(date) %>% 
  summarise(
    obs_rh_pct = round(mean(m1_Humidity, na.rm = TRUE), digits = 2)
  )

# estimate wind

obs_wind2m_ms <- obs_climate1 %>% 
  group_by(date) %>% 
  summarise(
    obs_wind_2m_m_s = round(mean(`m60_Wind_Vector(1)`, na.rm = TRUE), digits = 2)
  )

# estimate precipitation

obs_precip_cm <- obs_climate1 %>% 
  group_by(date) %>% 
  summarise(
    obs_precip_cm = round(sum(m60_TBRG_Tot, na.rm = TRUE), digits = 2)/10
  )

# estimate total radiation

obs_rad_mjm2 <- obs_climate1 %>% 
  filter(RF1_E_Total>0) %>% 
  group_by(date) %>% 
  summarise(
    obs_rad_mjm2 = round(sum(`RF1_E_Total`, na.rm = TRUE), digits = 2)
  )

# merge the data

dfs_to_merge  <- list(obs_max_temp_c, obs_min_temp_c, obs_precip_cm, obs_wind2m_ms, obs_rad_mjm2, obs_rh_pct)
merged_data <- reduce(dfs_to_merge, full_join, by = "date")
merged_data <- merged_data %>%
  mutate(row_number = row_number()) %>%  # Create a new column with row numbers
  select(row_number, everything()) %>%    # Select the new column first, followed by all other columns
  select(-2) 

# exporting .txt file
write.table(merged_data, "data/climate/elora2023.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# plots to check data

mean(merged_data$obs_max_temp_c, na.rm = TRUE)
mean(obs_min_temp_c$obs_min_temp_c, na.rm = TRUE)
sum(obs_precip_cm$obs_precip_cm, na.rm = TRUE)
mean(obs_wind2m_ms$obs_wind_2m_m_s, na.rm = TRUE)
sum(obs_rad_mjm2$obs_rad_mjm2, na.rm = TRUE)
mean(obs_rh_pct$obs_rh_pct, na.rm = TRUE)

# comparing with other years

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2", "obs_rh_pct")
data2022 <- read.table("obs_data/climate/elora2022.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
data2022 <- data2022 %>%
  mutate(across(-c(julian_day), as.numeric))

mean(data2022$obs_max_temp_c, na.rm = TRUE)
mean(data2022$obs_min_temp_c, na.rm = TRUE)
sum(data2022$obs_precip_cm, na.rm = TRUE)
data2022 %>% 
  filter(!abs(obs_wind2m_ms)>500) %>% 
  summarise(
    mean = mean(obs_wind2m_ms)
  )
data2022 %>% 
  filter(!abs(obs_rad_mjm2)>500) %>% 
  summarise(
    sum = sum(obs_rad_mjm2)
  )
sum(data2022$obs_rad_mjm2, na.rm = TRUE)
mean(data2022$obs_rh_pct, na.rm = TRUE)

##########################

##########################
######## DNDC 2024 ############

# for 2024 I initially tried to use the same file that in 2023, but it was incomplete; then I attempted to use monthly files from the ECCC website, but those had a lot of gaps too; so I ended up using that file only for a few items, the "daily" file from ECCC weather station for a few other items, and the rest from our own database.

# create time columns (assuming 48 half-hourly measurements per day) - used to match data from our own databas that was measured half-hourly

n <- nrow(rg_p12_2024)
days <- n %/% 48
hour_seq <- rep(seq(0.5, 24, by = 0.5), length.out = n)

time_df <- data.frame(
  Year = rep(2024, n),
  doy = rep(1:days, each = 48)[1:n],
  Hour = hour_seq
)

# estimate rh - from our own database.

rh_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/2025_05_01_new_data/new_vpd_rh_data/RH_avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
rh_p12_2024 <- rh_p12_2024 %>% rename(RH = V1)

rh_p12_2024 <- bind_cols(time_df, rh_p12_2024)

obs_rh_pct <- rh_p12_2024 %>% 
  group_by(doy) %>% 
  summarise(
    obs_rh_pct = round(mean(RH, na.rm = TRUE), digits = 2)
  ) 

sum(is.na(obs_rh_pct$obs_rh_pct))

# estimate precipitation - from our own database, it looked too low from the ECCC database.

obs_precip <- read.csv("Z:/E26/database/ers4/2024/ERSWeatherStation/EnvCan/2024_Daily.csv", header = TRUE)
obs_precip$obs_precip_cm <- obs_precip$Total.Precip..mm./10
obs_precip_cm <- obs_precip %>%
  mutate(
    datetime = mdy(Date.Time), # parse datetime
    doy = yday(datetime)
  ) %>% 
  select(doy, obs_precip_cm) 

sum(is.na(obs_precip_cm$obs_precip_cm))

# estimate total radiation - from our own database; I used p3 here because it seems to show similar values to other years (instead of p1); I complemented this data with values from p2 and values from ECCC to fill gaps. Our own data for p2 and p3 had a lot of NAs

rg_p12_2024 <- read.table("obs_data/measured_co2/data_extraction/rg/p12/CM3Up_Avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE) # the unit for this data is W/m²; however, weather data for DNDC needs to be in MJ/m²
rg_p12_2024 <- rg_p12_2024 %>% rename(Rg_p12 = V1)
rg_p12_2024 <- bind_cols(time_df, rg_p12_2024)

rg_p3_2024 <- read.table("obs_data/measured_co2/data_extraction/rg/p3/CM3Up_Avg_2024", header = FALSE, sep = "\t", stringsAsFactors = FALSE) # the unit for this data is W/m²; however, weather data for DNDC needs to be in MJ/m²
rg_p3_2024 <- rg_p3_2024 %>% rename(Rg_p3 = V1)
rg_p3_2024 <- bind_cols(time_df, rg_p3_2024)


# merging our two towers and calculating daily values

rg_p123_2024<- rg_p3_2024 %>% 
  left_join(rg_p12_2024,join_by(Year, doy, Hour)) %>% 
  mutate(
    Rg = ifelse(is.na(Rg_p3), Rg_p12, Rg_p3) 
  ) 

sum(is.na(rg_p123_2024$Rg))
sum(is.na(rg_p123_2024$Rg_p12))
sum(is.na(rg_p123_2024$Rg_p3))
sum(is.na(rg_p123_2024$Rg_new))

rg_p123_2024_na <- rg_p123_2024 %>% 
  filter(is.na(Rg_new)) %>% 
  summarise(
    n = n()
  )

obs_rad_mjm2_daily_p123 <- rg_p123_2024 %>%
  group_by(doy) %>%
  summarise(
    obs_na = sum(is.na(Rg_new)),
    obs_rad_mjm2 = ifelse(
      sum(is.na(Rg_new)) > 2,
      NA,
      round(sum(Rg_new, na.rm = TRUE) * 0.0018, 2)  # Convert to MJ/m²
    ),
    .groups = "drop"
  )

# bringing in more data from ECCC

obs_climate_2024 <- read.csv("Z:/E26/database/ers4/2024/ERSWeatherStation/ERS_WX2_weather_2024.csv", header = FALSE) # i did not use this dataset to start with, because it is missing a lot of data at the beginning
colnames(obs_climate_2024) <- obs_climate_2024[2, ]
obs_climate_2024 <- obs_climate_2024[-c(1:4),]

obs_climate_20241 <- select(obs_climate_2024, TIMESTAMP, m1_Humidity, `h1_Temp(1)`, `m60_Wind_Vector(1)`, m60_TBRG_Tot, RF1_E_Total)
obs_climate_20241 <- obs_climate_20241 %>%
  mutate(date = as.Date(parse_date_time(TIMESTAMP, orders = "ymd HMS")))
obs_climate_20241 <- obs_climate_20241 %>%
  mutate(across(-c(TIMESTAMP,date), as.numeric))
head(obs_climate_20241$TIMESTAMP, 10)  # Check the first 10 values of the TIMESTAMP column

obs_climate_20241_rad <- obs_climate_20241 %>%
  mutate(
    timestamp_posix = ymd_hms(TIMESTAMP),  # Convert to POSIXct
    doy = yday(timestamp_posix),
  ) %>%
  select(-timestamp_posix) %>% 
  group_by(doy) %>% 
  summarise(
    obs_na = sum(is.na(RF1_E_Total)),
    obs_rad_mjm2_eccc = ifelse(
      sum(is.na(RF1_E_Total)) > 2,
      NA,
      round(sum(RF1_E_Total, na.rm = TRUE), 2)  # Convert to MJ/m²
    ),
    .groups = "drop"
  )

# merging final sources
obs_rad_mjm2_final<- obs_rad_mjm2_daily_p123 %>% 
  left_join(obs_climate_20241_rad,join_by(doy)) %>% 
  mutate(
    obs_rad_mjm2_final  = ifelse(is.na(obs_rad_mjm2), obs_rad_mjm2_eccc, obs_rad_mjm2) 
  ) %>% 
  select(doy, obs_rad_mjm2_final)

sum(is.na(rg_p123eccc_2024$obs_rad_mjm2_final))


# estimating temperature - from ECCC file

obs_temp <- read.csv("Z:/E26/database/ers4/2024/ERSWeatherStation/EnvCan/2024_Daily.csv", header = TRUE)
obs_temp <- obs_temp %>%
  mutate(
    datetime = mdy(Date.Time), # parse datetime
    doy = yday(datetime)
  )

# estimate max temp
obs_max_temp_c <- obs_temp %>% 
  mutate(
    obs_max_temp_c = Max.Temp...C.
  ) %>% 
  select(doy, obs_max_temp_c)

# estimate min temp

obs_min_temp_c <- obs_temp %>% 
  mutate(
    obs_min_temp_c = Min.Temp...C.
  ) %>% 
  select(doy, obs_min_temp_c)

# estimate wind - this I kept from the ECCC database (a few different sources)

#  pulling data from monthly ECCC files

# Create an empty list to store data frames
obs_climate_list <- list()

# Loop through months 01 to 12
for (month in sprintf("%02d", 1:12)) {
  # Construct the file path
  file_path <- paste0("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/EnvCan_2024/2024", month, ".csv")
  
  # Read the CSV file
  obs_climate <- read.csv(file_path, header = FALSE)
  
  # Assign column names from the first row
  colnames(obs_climate) <- obs_climate[1, ]
  
  # Remove the first row (since it was used for column names)
  obs_climate <- obs_climate[-1, ]
  
  # Add a column to identify the month
  obs_climate$Month <- month
  
  # Store the data frame in the list
  obs_climate_list[[month]] <- obs_climate
}

# Ensure all data frames have the same columns
obs_climate_list <- lapply(obs_climate_list, function(df) {
  df <- df[, intersect(names(df), names(obs_climate_list$"01"))] # Keep only common columns
  return(df)
})

# Combine all data frames into one
obs_climate_all <- do.call(rbind, obs_climate_list)

# create a doy column

obs_climate_all <- obs_climate_all %>%
  mutate(
    datetime = ymd_hm(`Date/Time (LST)`), # parse datetime
    doy = yday(datetime)                 # extract day of year
  )

obs_climate_all$`Wind Spd (km/h)`<- as.numeric(obs_climate_all$`Wind Spd (km/h)` )

obs_wind2m_ms <- obs_climate_all %>% 
  group_by(doy) %>% 
  summarise(
    obs_wind_2m_m_s = round(mean(`Wind Spd (km/h)`, na.rm = TRUE)*0.277778, digits = 2)
  )

# bring in additional wind data from ECCC

obs_climate_20241_wind_daily <- obs_climate_20241 %>%
  mutate(
    timestamp_posix = ymd_hms(TIMESTAMP),  # Convert to POSIXct
    doy = yday(timestamp_posix),
  ) %>%
  select(-timestamp_posix) %>% 
  group_by(doy) %>% 
  summarise(
    obs_wind_2m_m_s_eccc = round(mean(`m60_Wind_Vector(1)`, na.rm = TRUE), digits = 2)
    ) %>% 
  ungroup()

# merge wind data

obs_wind2m_ms <- obs_wind2m_ms %>% 
  left_join(obs_climate_20241_wind_daily, by = "doy") %>% 
  mutate(
    obs_wind_2m_m_s = ifelse(is.na(obs_wind_2m_m_s), obs_wind_2m_m_s_eccc, obs_wind_2m_m_s)
  ) %>% 
  select(doy, obs_wind_2m_m_s)

# merge all the weather the data

dfs_to_merge  <- list(obs_max_temp_c, obs_min_temp_c, obs_precip_cm, obs_wind2m_ms, obs_rad_mjm2_final, obs_rh_pct)
merged_data <- reduce(dfs_to_merge, full_join, by = "doy")
merged_data <- merged_data %>%
  mutate(row_number = row_number()) %>%  # Create a new column with row numbers
  select(row_number, everything()) %>%    # Select the new column first, followed by all other columns
  select(-1) 

merged_data_clean <- merged_data %>%
  ungroup() %>%  # remove grouping to avoid method error
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), "-999", .)))

# exporting .txt file
write.table(merged_data_clean, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2024.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# plots to check data

mean(merged_data_clean$obs_max_temp_c, na.rm = TRUE)
mean(merged_data_clean$obs_min_temp_c, na.rm = TRUE)
sum(merged_data_clean$obs_precip_cm, na.rm = TRUE)
mean(merged_data_clean$obs_wind_2m_m_s, na.rm = TRUE)
sum(merged_data_clean$obs_rad_mjm2, na.rm = TRUE)
mean(merged_data_clean$obs_rh_pct, na.rm = TRUE)

# comparing with other years

#2022

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2", "obs_rh_pct")
data2022 <- read.table("obs_data/climate/elora2022.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
data2022 <- data2022 %>%
  mutate(across(-c(julian_day), as.numeric))

mean(data2022$obs_max_temp_c, na.rm = TRUE)
mean(data2022$obs_min_temp_c, na.rm = TRUE)
sum(data2022$obs_precip_cm, na.rm = TRUE)
data2022 %>% 
  filter(!abs(obs_wind2m_ms)>500) %>% 
  summarise(
    mean = mean(obs_wind2m_ms)
  )
data2022 %>% 
  filter(!abs(obs_rad_mjm2)>500) %>% 
  summarise(
    sum = sum(obs_rad_mjm2)
  )
sum(data2022$obs_rad_mjm2, na.rm = TRUE)
mean(data2022$obs_rh_pct, na.rm = TRUE)

# 2023

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2", "obs_rh_pct")
data2023 <- read.table("obs_data/climate/elora2023.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
data2023 <- data2023 %>%
  mutate(across(-c(julian_day), as.numeric))

mean(data2023$obs_max_temp_c, na.rm = TRUE)
mean(data2023$obs_min_temp_c, na.rm = TRUE)
sum(data2023$obs_precip_cm, na.rm = TRUE)
data2023 %>% 
  filter(!abs(obs_wind2m_ms)>500) %>% 
  summarise(
    mean = mean(obs_wind2m_ms)
  )
data2023 %>% 
  filter(!abs(obs_rad_mjm2)>500) %>% 
  summarise(
    sum = sum(obs_rad_mjm2)
  )
sum(data2023$obs_rad_mjm2, na.rm = TRUE)
mean(data2023$obs_rh_pct, na.rm = TRUE)

# 2019

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2", "obs_rh_pct")
data2019 <- read.table("obs_data/climate/elora2019.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
data2019 <- data2019 %>%
  mutate(across(-c(julian_day), as.numeric))

mean(data2019$obs_max_temp_c, na.rm = TRUE)
mean(data2019$obs_min_temp_c, na.rm = TRUE)
sum(data2019$obs_precip_cm, na.rm = TRUE)
data2019 %>% 
  filter(!abs(obs_wind2m_ms)>500) %>% 
  summarise(
    mean = mean(obs_wind2m_ms)
  )
data2019 %>% 
  filter(!abs(obs_rad_mjm2)>500) %>% 
  summarise(
    sum = sum(obs_rad_mjm2)
  )
sum(data2019$obs_rad_mjm2, na.rm = TRUE)
mean(data2019$obs_rh_pct, na.rm = TRUE)


##########################

###################################
##### updating DNDC climate files ######

# i updated some of the climate information i was using, when dealing with reddyproc. I wanted to make sure the climate information I am using in DNDC was consitent with the latest files I used in DNDC, so I updated the dndc climate files, based on the variables I used in Reddyproc (temperature, rh, and rg)

#2018

ec_18_p12_climate <- read.table("ec_18_p12", header = TRUE) # this is the original file with EC data I had created
str(ec_18_p12_climate)
ec_18_p12_climate <- ec_18_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2018 <- read.table("obs_data/climate/original_data/elora2018.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2018 <- orig_data_2018 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2018_updated <- orig_data_2018 %>%
  left_join(ec_18_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2018_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2018.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2018" on the first line

#2019

ec_19_p12_climate <- read.table("ec_19_p12", header = TRUE) # this is the original file with EC data I had created
ec_19_p12_climate <- ec_19_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))


col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2019 <- read.table("obs_data/climate/original_data/elora2019.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2019 <- orig_data_2019 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2019_updated <- orig_data_2019 %>%
  left_join(ec_19_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2019_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2019.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2019" on the first line


#2020

ec_20_p12_climate <- read.table("ec_20_p12", header = TRUE) # this is the original file with EC data I had created
ec_20_p12_climate <- ec_20_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2020 <- read.table("obs_data/climate/original_data/elora2020.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2020 <- orig_data_2020 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2020_updated <- orig_data_2020 %>%
  left_join(ec_20_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2020_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2020.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2020" on the first line

#2021

ec_21_p12_climate <- read.table("ec_21_p12", header = TRUE) # this is the original file with EC data I had created
ec_21_p12_climate <- ec_21_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2021 <- read.table("obs_data/climate/original_data/elora2021.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2021 <- orig_data_2021 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2021_updated <- orig_data_2021 %>%
  left_join(ec_21_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2021_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2021.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2021" on the first line

#2022

ec_22_p12_climate <- read.table("ec_22_p12", header = TRUE) # this is the original file with EC data I had created
ec_22_p12_climate <- ec_22_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2022 <- read.table("obs_data/climate/original_data/elora2022.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2022 <- orig_data_2022 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2022_updated <- orig_data_2022 %>%
  left_join(ec_22_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2022_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2022.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2022" on the first line


#2023

ec_23_p12_climate <- read.table("ec_23_p12", header = TRUE) # this is the original file with EC data I had created
ec_23_p12_climate <- ec_23_p12_climate %>% 
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2023 <- read.table("obs_data/climate/original_data/elora2023.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2023 <- orig_data_2023 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2023_updated <- orig_data_2023 %>%
  left_join(ec_23_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2023_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2023.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)

# after creating the climate file, we need to add manually "elora2023" on the first line

#2024

ec_24_p12_climate <- read.table("ec_24_p12", header = TRUE) # this is the original file with EC data I had created
ec_24_p12_climate <- ec_24_p12_climate %>%
  mutate(
    Tair = as.numeric(Tair),
    RH = as.numeric(RH),
    Rg = as.numeric(Rg)
  ) %>%
  group_by(Year, DoY) %>%
  summarise(
    obs_max_temp_c_new = if (all(is.na(Tair))) NA_real_ else max(Tair, na.rm = TRUE),
    obs_min_temp_c_new = if (all(is.na(Tair))) NA_real_ else min(Tair, na.rm = TRUE),
    obs_rh_pct_new = if (all(is.na(RH))) NA_real_ else mean(RH, na.rm = TRUE),
    obs_rad_mjm2day_new = if (all(is.na(Rg))) NA_real_ else sum(Rg * 0.0018, na.rm = TRUE)
  ) %>%
  mutate(julian_day = as.integer(DoY))

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")
orig_data_2024 <- read.table("obs_data/climate/original_data/elora2024.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
orig_data_2024 <- orig_data_2024 %>%
  mutate(across(-c(julian_day), as.numeric))

orig_data_2024_updated <- orig_data_2024 %>%
  left_join(ec_24_p12_climate, by = "julian_day") %>%
  mutate(
    obs_max_temp_c = ifelse(is.na(obs_max_temp_c_new), obs_max_temp_c, obs_max_temp_c_new),
    obs_min_temp_c = ifelse(is.na(obs_min_temp_c_new), obs_min_temp_c, obs_min_temp_c_new),
    obs_rad_mjm2day = ifelse(is.na(obs_rad_mjm2day_new), obs_rad_mjm2day, obs_rad_mjm2day_new),  # From EC data
    obs_rh_pct = ifelse(is.na(obs_rh_pct_new), obs_rh_pct, obs_rh_pct_new)
  ) %>%
  select(-ends_with("_new"), -Year, -DoY)

# exporting .txt file
write.table(orig_data_2024_updated, "C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/climate/elora2024.txt", sep = "\t", row.names = FALSE,col.names = FALSE, quote = FALSE)


# after creating the climate file, we need to add manually "elora2024" on the first line


###################################

###########################################################
###### generating new soil and climated observed data ######

######### soil water ######################################

# 2018

vwc_2018_p1 <- read.csv("obs_data/measured_vwc/raw_data/2018/P1_vwc.csv", header = TRUE)
vwc_2018_p2 <- read.csv("obs_data/measured_vwc/raw_data/2018/P2_vwc.csv", header = TRUE)
vwc_2018_p3 <- read.csv("obs_data/measured_vwc/raw_data/2018/P3_vwc.csv", header = TRUE)
vwc_2018_p4 <- read.csv("obs_data/measured_vwc/raw_data/2018/P4_vwc.csv", header = TRUE)

vwc_2018 <- bind_rows(vwc_2018_p1, vwc_2018_p2, vwc_2018_p3, vwc_2018_p4)
vwc_2018 <- vwc_2018 %>%
  mutate(doy_int = as.integer(DOY))

vwc_2018_daily <- vwc_2018 %>%
  group_by(Year, doy_int, plot) %>%
  summarise(
    vwc_5 = mean(VWC_5, na.rm = TRUE),
    vwc_25 = mean(VWC_25, na.rm = TRUE),
    vwc_55 = mean(VWC_55, na.rm = TRUE),
    vwc_85 = mean(VWC_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2018_daily %>% 
  ggplot(aes(x = doy_int, y = vwc_5, color = as.character(plot)))+
  geom_line()+
  theme_bw()

vwc_2018_summary <- vwc_2018_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(vwc_5, na.rm = TRUE),
    vwc_5_sd = sd(vwc_5, na.rm = TRUE),
    vwc_25_mean = mean(vwc_25, na.rm = TRUE),
    vwc_25_sd = sd(vwc_25, na.rm = TRUE),
    vwc_55_mean = mean(vwc_55, na.rm = TRUE),
    vwc_55_sd = sd(vwc_55, na.rm = TRUE),
    vwc_85_mean = mean(vwc_85, na.rm = TRUE),
    vwc_85_sd = sd(vwc_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2018_summary <- vwc_2018_summary %>%
  mutate(
    year = Year,
    doy = doy_int
  )

# plot

ggplot(vwc_2018_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

# 2019

vwc_2019_p1 <- read.csv("obs_data/measured_vwc/raw_data/2019/P1_vwc.csv", header = TRUE)
vwc_2019_p2 <- read.csv("obs_data/measured_vwc/raw_data/2019/P2_vwc.csv", header = TRUE)
vwc_2019_p3 <- read.csv("obs_data/measured_vwc/raw_data/2019/P3_vwc.csv", header = TRUE)
vwc_2019_p4 <- read.csv("obs_data/measured_vwc/raw_data/2019/P4_vwc.csv", header = TRUE)

vwc_2019 <- bind_rows(vwc_2019_p1, vwc_2019_p2, vwc_2019_p3, vwc_2019_p4)
vwc_2019 <- vwc_2019 %>%
  mutate(doy_int = as.integer(DOY))

vwc_2019_daily <- vwc_2019 %>%
  group_by(Year, doy_int, plot) %>%
  summarise(
    vwc_5 = mean(VWC_5 , na.rm = TRUE),
    vwc_25 = mean(VWC_25, na.rm = TRUE),
    vwc_55 = mean(VWC_55, na.rm = TRUE),
    vwc_85 = mean(VWC_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2019_daily %>% 
  ggplot(aes(x = doy_int, y = vwc_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

vwc_2019_summary <- vwc_2019_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(vwc_5, na.rm = TRUE),
    vwc_5_sd = sd(vwc_5, na.rm = TRUE),
    vwc_25_mean = mean(vwc_25, na.rm = TRUE),
    vwc_25_sd = sd(vwc_25, na.rm = TRUE),
    vwc_55_mean = mean(vwc_55, na.rm = TRUE),
    vwc_55_sd = sd(vwc_55, na.rm = TRUE),
    vwc_85_mean = mean(vwc_85, na.rm = TRUE),
    vwc_85_sd = sd(vwc_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2019_summary <- vwc_2019_summary %>%
  mutate(
    year = Year,
    doy = doy_int
  ) %>% 
  select(-Year, -doy_int)

# plot

ggplot(vwc_2019_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()


# 2020

vwc_2020_p1 <- read.csv("obs_data/measured_vwc/raw_data/2020/P1_vwc.csv", header = TRUE)
vwc_2020_p2 <- read.csv("obs_data/measured_vwc/raw_data/2020/P2_vwc.csv", header = TRUE)
vwc_2020_p3 <- read.csv("obs_data/measured_vwc/raw_data/2020/P3_vwc.csv", header = TRUE)
vwc_2020_p4 <- read.csv("obs_data/measured_vwc/raw_data/2020/P4_vwc.csv", header = TRUE)

vwc_2020 <- bind_rows(vwc_2020_p1, vwc_2020_p2, vwc_2020_p3, vwc_2020_p4)
vwc_2020 <- vwc_2020 %>%
  mutate(doy_int = as.integer(DOY))

vwc_2020_daily <- vwc_2020 %>%
  group_by(Year, doy_int, plot) %>%
  summarise(
    vwc_5 = mean(VWC_5, na.rm = TRUE),
    vwc_25 = mean(VWC_25, na.rm = TRUE),
    vwc_55 = mean(VWC_55, na.rm = TRUE),
    vwc_85 = mean(VWC_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2020_daily %>% 
  ggplot(aes(x = doy_int, y = vwc_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

vwc_2020_summary <- vwc_2020_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(vwc_5, na.rm = TRUE),
    vwc_5_sd = sd(vwc_5, na.rm = TRUE),
    vwc_25_mean = mean(vwc_25, na.rm = TRUE),
    vwc_25_sd = sd(vwc_25, na.rm = TRUE),
    vwc_55_mean = mean(vwc_55, na.rm = TRUE),
    vwc_55_sd = sd(vwc_55, na.rm = TRUE),
    vwc_85_mean = mean(vwc_85, na.rm = TRUE),
    vwc_85_sd = sd(vwc_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2020_summary <- vwc_2020_summary %>%
  mutate(
    year = Year,
    doy = doy_int
  ) %>% 
  select(-Year, -doy_int)

# plot

ggplot(vwc_2020_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

# 2021

vwc_2021_p1 <- read.csv("obs_data/measured_vwc/raw_data/2021/P1_vwc.csv", header = TRUE)
vwc_2021_p2 <- read.csv("obs_data/measured_vwc/raw_data/2021/P2_vwc.csv", header = TRUE)
vwc_2021_p3 <- read.csv("obs_data/measured_vwc/raw_data/2021/P3_vwc.csv", header = TRUE)
vwc_2021_p4 <- read.csv("obs_data/measured_vwc/raw_data/2021/P4_vwc.csv", header = TRUE)

vwc_2021 <- bind_rows(vwc_2021_p1, vwc_2021_p2, vwc_2021_p3, vwc_2021_p4)
vwc_2021 <- vwc_2021 %>%
  mutate(doy_int = as.integer(DOY))

vwc_2021_daily <- vwc_2021 %>%
  group_by(Year, doy_int, plot) %>%
  summarise(
    vwc_5 = mean(VWC_5, na.rm = TRUE),
    vwc_25 = mean(VWC_25, na.rm = TRUE),
    vwc_55 = mean(VWC_55, na.rm = TRUE),
    vwc_85 = mean(VWC_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2021_daily %>% 
  ggplot(aes(x = doy_int, y = vwc_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

vwc_2021_summary <- vwc_2021_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(vwc_5, na.rm = TRUE),
    vwc_5_sd = sd(vwc_5, na.rm = TRUE),
    vwc_25_mean = mean(vwc_25, na.rm = TRUE),
    vwc_25_sd = sd(vwc_25, na.rm = TRUE),
    vwc_55_mean = mean(vwc_55, na.rm = TRUE),
    vwc_55_sd = sd(vwc_55, na.rm = TRUE),
    vwc_85_mean = mean(vwc_85, na.rm = TRUE),
    vwc_85_sd = sd(vwc_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2021_summary <- vwc_2021_summary %>%
  mutate(
    year = Year,
    doy = doy_int
  ) %>% 
  select(-Year, -doy_int)

# plot

ggplot(vwc_2021_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

# 2022

vwc_2022_p1 <- read.csv("obs_data/measured_vwc/raw_data/2022/P1_vwc.csv", header = TRUE)
vwc_2022_p2 <- read.csv("obs_data/measured_vwc/raw_data/2022/P2_vwc.csv", header = TRUE)
vwc_2022_p3 <- read.csv("obs_data/measured_vwc/raw_data/2022/P3_vwc.csv", header = TRUE)
vwc_2022_p4 <- read.csv("obs_data/measured_vwc/raw_data/2022/P4_vwc.csv", header = TRUE)

vwc_2022 <- bind_rows(vwc_2022_p1, vwc_2022_p2, vwc_2022_p3, vwc_2022_p4)
vwc_2022 <- vwc_2022 %>%
  mutate(doy_int = as.integer(DOY))

vwc_2022_daily <- vwc_2022 %>%
  group_by(Year, doy_int, plot) %>%
  summarise(
    vwc_5 = mean(VWC_5, na.rm = TRUE),
    vwc_25 = mean(VWC_25, na.rm = TRUE),
    vwc_55 = mean(VWC_55, na.rm = TRUE),
    vwc_85 = mean(VWC_85, na.rm = TRUE),
    .groups = "drop"
  )

vwc_2022_daily %>%
  ggplot(aes(x = doy_int, y = vwc_5, color = as.character(plot)))+
  geom_line()+
  theme_bw()

vwc_2022_summary <- vwc_2022_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(vwc_5, na.rm = TRUE),
    vwc_5_sd = sd(vwc_5, na.rm = TRUE),
    vwc_25_mean = mean(vwc_25, na.rm = TRUE),
    vwc_25_sd = sd(vwc_25, na.rm = TRUE),
    vwc_55_mean = mean(vwc_55, na.rm = TRUE),
    vwc_55_sd = sd(vwc_55, na.rm = TRUE),
    vwc_85_mean = mean(vwc_85, na.rm = TRUE),
    vwc_85_sd = sd(vwc_85, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  filter(doy<179) # moisture values for 55 and 85 cm seemed to contain strange interpolation values after doy 178 (when measurements for doy 5 and 25 end), so i decided to remove them.

vwc_2022_summary <- vwc_2022_summary %>%
  mutate(
    year = Year,
    doy = doy_int
  ) %>% 
  select(-Year, -doy_int)

# plot

ggplot(vwc_2022_summary, aes(x = doy_int, y = vwc_25_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_25_mean - vwc_25_sd, ymax = vwc_25_mean + vwc_25_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()


# 2023

vwc_2023_p1 <- read.csv("obs_data/measured_vwc/raw_data/2023/Plot_1_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2023_p1) <- vwc_2023_p1[1,]
vwc_2023_p1 <- vwc_2023_p1[-1,]
vwc_2023_p2 <- read.csv("obs_data/measured_vwc/raw_data/2023/Plot_2_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2023_p2) <- vwc_2023_p2[1,]
vwc_2023_p2 <- vwc_2023_p2[-1,]
vwc_2023_p3 <- read.csv("obs_data/measured_vwc/raw_data/2023/Plot_3_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2023_p3) <- vwc_2023_p3[1,]
vwc_2023_p3 <- vwc_2023_p3[-1,]
vwc_2023_p4 <- read.csv("obs_data/measured_vwc/raw_data/2023/Plot_4_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2023_p4) <- vwc_2023_p4[1,]
vwc_2023_p4 <- vwc_2023_p4[-1,]

vwc_2023 <- bind_rows(vwc_2023_p1, vwc_2023_p2, vwc_2023_p3, vwc_2023_p4)
vwc_2023 <- vwc_2023 %>%
  mutate(doy_int = as.integer(DOY),
         year = `% Year`,
         W_5_1 = as.numeric(W_5_1),
         W_5_2 = as.numeric(W_5_2),
         W_25_1 = as.numeric(W_25_1),
         W_25_2 = as.numeric(W_25_2)
         )

vwc_2023_daily <- vwc_2023 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    W_5_1 = mean(W_5_1, na.rm = TRUE),
    W_5_2 = mean(W_5_2, na.rm = TRUE),
    W_25_1 = mean(W_25_1, na.rm = TRUE),
    W_25_2 = mean(W_25_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("W_"),
    names_to = c(".value", "subsample"),
    names_pattern = "(W_\\d+)_(\\d)"
  )
  
vwc_2023_daily %>% 
  ggplot(aes(x = doy_int, y = W_25  , color = subsample))+
  geom_line()+
  facet_wrap(plot~.)+
  theme_bw()

vwc_2023_summary <- vwc_2023_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(W_5, na.rm = TRUE),
    vwc_5_sd = sd(W_5, na.rm = TRUE),
    vwc_25_mean = mean(W_25, na.rm = TRUE),
    vwc_25_sd = sd(W_25, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(vwc_2023_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

vwc_2023_summary <- vwc_2023_summary %>%
  mutate(
    doy = doy_int,
    year = as.integer(year)
  ) %>%
  select(-doy_int)

# 2024

vwc_2024_p1 <- read.csv("obs_data/measured_vwc/raw_data/2024/Plot_1_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2024_p1) <- vwc_2024_p1[1,]
vwc_2024_p1 <- vwc_2024_p1[-1,]
vwc_2024_p2 <- read.csv("obs_data/measured_vwc/raw_data/2024/Plot_2_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2024_p2) <- vwc_2024_p2[1,]
vwc_2024_p2 <- vwc_2024_p2[-1,]
vwc_2024_p3 <- read.csv("obs_data/measured_vwc/raw_data/2024/Plot_3_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2024_p3) <- vwc_2024_p3[1,]
vwc_2024_p3 <- vwc_2024_p3[-1,]
vwc_2024_p4 <- read.csv("obs_data/measured_vwc/raw_data/2024/Plot_4_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(vwc_2024_p4) <- vwc_2024_p4[1,]
vwc_2024_p4 <- vwc_2024_p4[-1,]

vwc_2024 <- bind_rows(vwc_2024_p1, vwc_2024_p2, vwc_2024_p3, vwc_2024_p4)
vwc_2024 <- vwc_2024 %>%
  mutate(doy_int = as.integer(DOY),
         year = `% Year`,
         W_5_1 = as.numeric(W_5_1),
         W_5_2 = as.numeric(W_5_2),
         W_25_1 = as.numeric(W_25_1),
         W_25_2 = as.numeric(W_25_2)
  )

vwc_2024_daily <- vwc_2024 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    W_5_1 = mean(W_5_1, na.rm = TRUE),
    W_5_2 = mean(W_5_2, na.rm = TRUE),
    W_25_1 = mean(W_25_1, na.rm = TRUE),
    W_25_2 = mean(W_25_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("W_"),
    names_to = c(".value", "subsample"),
    names_pattern = "(W_\\d+)_(\\d)"
  ) %>% 
  filter(!c(plot == 3 & subsample == 2 & doy_int > 160)) # after inspection I realized data meeting these conditions were outliers

vwc_2024_daily %>% 
  ggplot(aes(x = doy_int, y = W_5  , color = subsample))+
  geom_line()+
  facet_wrap(plot~.)+
  theme_bw()

vwc_2024_summary <- vwc_2024_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    vwc_5_mean = mean(W_5, na.rm = TRUE),
    vwc_5_sd = sd(W_5, na.rm = TRUE),
    vwc_25_mean = mean(W_25, na.rm = TRUE),
    vwc_25_sd = sd(W_25, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(vwc_2024_summary, aes(x = doy_int, y = vwc_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "vwc (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

vwc_2024_summary <- vwc_2024_summary %>%
  mutate(
    doy = doy_int,
    year = as.integer(year)
  ) %>%
  filter(doy<150) %>% 
  select(-doy_int)

# merging all soil water data

vwc_2018_2024 <- bind_rows(vwc_2018_summary, vwc_2019_summary, vwc_2020_summary, vwc_2021_summary, vwc_2022_summary, vwc_2023_summary, vwc_2024_summary)

vwc_2018_2024 %>% 
  write_xlsx("obs_data/measured_vwc/vwc_2018_2024.xlsx") 

######### soil temperature ######################################

# 2018

temp_2018_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2018/P1_Temp.csv", header = TRUE)
temp_2018_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2018/P2_Temp.csv", header = TRUE)
temp_2018_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2018/P3_Temp.csv", header = TRUE)
temp_2018_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2018/P4_Temp.csv", header = TRUE)

temp_2018 <- bind_rows(temp_2018_p1, temp_2018_p2, temp_2018_p3, temp_2018_p4)
temp_2018 <- temp_2018 %>%
  mutate(doy_int = as.integer(doy))

temp_2018_daily <- temp_2018 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    temp_5 = mean(T_5, na.rm = TRUE),
    temp_25 = mean(T_25, na.rm = TRUE),
    temp_55 = mean(T_55, na.rm = TRUE),
    temp_85 = mean(T_85, na.rm = TRUE),
    .groups = "drop"
  )

temp_2018_daily %>%
  ggplot(aes(x = doy_int, y = temp_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

temp_2018_summary <- temp_2018_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(temp_5, na.rm = TRUE),
    temp_5_sd = sd(temp_5, na.rm = TRUE),
    temp_25_mean = mean(temp_25, na.rm = TRUE),
    temp_25_sd = sd(temp_25, na.rm = TRUE),
    temp_55_mean = mean(temp_55, na.rm = TRUE),
    temp_55_sd = sd(temp_55, na.rm = TRUE),
    temp_85_mean = mean(temp_85, na.rm = TRUE),
    temp_85_sd = sd(temp_85, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(temp_2018_summary, aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2018_summary <- temp_2018_summary %>%
  mutate(
    doy = doy_int
  ) %>% 
  select(-doy_int) 

# 2019

temp_2019_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2019/P1_Temp.csv", header = TRUE)
temp_2019_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2019/P2_Temp.csv", header = TRUE)
temp_2019_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2019/P3_Temp.csv", header = TRUE)
temp_2019_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2019/P4_Temp.csv", header = TRUE)

temp_2019 <- bind_rows(temp_2019_p1, temp_2019_p2, temp_2019_p3, temp_2019_p4)
temp_2019 <- temp_2019 %>%
  mutate(doy_int = as.integer(doy))

temp_2019_daily <- temp_2019 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    temp_5 = mean(T_5, na.rm = TRUE),
    temp_25 = mean(T_25, na.rm = TRUE),
    temp_55 = mean(T_55, na.rm = TRUE),
    temp_85 = mean(T_85, na.rm = TRUE),
    .groups = "drop"
  )

temp_2019_daily %>%
  ggplot(aes(x = doy_int, y = temp_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

temp_2019_summary <- temp_2019_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(temp_5, na.rm = TRUE),
    temp_5_sd = sd(temp_5, na.rm = TRUE),
    temp_25_mean = mean(temp_25, na.rm = TRUE),
    temp_25_sd = sd(temp_25, na.rm = TRUE),
    temp_55_mean = mean(temp_55, na.rm = TRUE),
    temp_55_sd = sd(temp_55, na.rm = TRUE),
    temp_85_mean = mean(temp_85, na.rm = TRUE),
    temp_85_sd = sd(temp_85, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(temp_2019_summary, aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Soil temperature at 5 cm",
    x = "Day of Year",
    y = "temp (C)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2019_summary <- temp_2019_summary %>%
  mutate(
    doy = doy_int
  ) %>% 
  select(-doy_int)

# 2020

temp_2020_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2020/P1_Temp.csv", header = TRUE)
temp_2020_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2020/P2_Temp.csv", header = TRUE)
temp_2020_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2020/P3_Temp.csv", header = TRUE)
temp_2020_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2020/P4_Temp.csv", header = TRUE)

temp_2020 <- bind_rows(temp_2020_p1, temp_2020_p2, temp_2020_p3, temp_2020_p4)
temp_2020 <- temp_2020 %>%
  mutate(doy_int = as.integer(doy))

temp_2020_daily <- temp_2020 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    temp_5 = mean(T_5, na.rm = TRUE),
    temp_25 = mean(T_25, na.rm = TRUE),
    temp_55 = mean(T_55, na.rm = TRUE),
    temp_85 = mean(T_85, na.rm = TRUE),
    .groups = "drop"
  )

temp_2020_daily %>%
  ggplot(aes(x = doy_int, y = temp_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

temp_2020_summary <- temp_2020_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(temp_5, na.rm = TRUE),
    temp_5_sd = sd(temp_5, na.rm = TRUE),
    temp_25_mean = mean(temp_25, na.rm = TRUE),
    temp_25_sd = sd(temp_25, na.rm = TRUE),
    temp_55_mean = mean(temp_55, na.rm = TRUE),
    temp_55_sd = sd(temp_55, na.rm = TRUE),
    temp_85_mean = mean(temp_85, na.rm = TRUE),
    temp_85_sd = sd(temp_85, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(temp_2020_summary, aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

# export dataset

temp_2020_summary <- temp_2020_summary %>%
  mutate(
    doy = doy_int
  ) %>% 
  select(-doy_int)

# 2021

temp_2021_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2021/P1_Temp.csv", header = TRUE)
temp_2021_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2021/P2_Temp.csv", header = TRUE)
temp_2021_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2021/P3_Temp.csv", header = TRUE)
temp_2021_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2021/P4_Temp.csv", header = TRUE)

temp_2021 <- bind_rows(temp_2021_p1, temp_2021_p2, temp_2021_p3, temp_2021_p4)
temp_2021 <- temp_2021 %>%
  mutate(doy_int = as.integer(doy))

temp_2021_daily <- temp_2021 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    temp_5 = mean(T_5, na.rm = TRUE),
    temp_25 = mean(T_25, na.rm = TRUE),
    temp_55 = mean(T_55, na.rm = TRUE),
    temp_85 = mean(T_85, na.rm = TRUE),
    .groups = "drop"
  )

temp_2021_daily %>%
  ggplot(aes(x = doy_int, y = temp_85, color = as.character(plot)))+
  geom_line()+
  theme_bw()

temp_2021_summary <- temp_2021_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(temp_5, na.rm = TRUE),
    temp_5_sd = sd(temp_5, na.rm = TRUE),
    temp_25_mean = mean(temp_25, na.rm = TRUE),
    temp_25_sd = sd(temp_25, na.rm = TRUE),
    temp_55_mean = mean(temp_55, na.rm = TRUE),
    temp_55_sd = sd(temp_55, na.rm = TRUE),
    temp_85_mean = mean(temp_85, na.rm = TRUE),
    temp_85_sd = sd(temp_85, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(temp_2021_summary, aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2021_summary <- temp_2021_summary %>%
  mutate(
    doy = doy_int
  ) %>% 
  select(-doy_int)

# 2022

temp_2022_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2022/P1_Temp.csv", header = TRUE)
temp_2022_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2022/P2_Temp.csv", header = TRUE)
temp_2022_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2022/P3_Temp.csv", header = TRUE)
temp_2022_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2022/P4_Temp.csv", header = TRUE)

temp_2022 <- bind_rows(temp_2022_p1, temp_2022_p2, temp_2022_p3, temp_2022_p4)
temp_2022 <- temp_2022 %>%
  mutate(doy_int = as.integer(doy))

temp_2022_daily <- temp_2022 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    temp_5 = mean(T_5, na.rm = TRUE),
    temp_25 = mean(T_25, na.rm = TRUE),
    temp_55 = mean(T_55, na.rm = TRUE),
    temp_85 = mean(T_85, na.rm = TRUE),
    .groups = "drop"
  )

temp_2022_daily %>%
  ggplot(aes(x = doy_int, y = temp_5, color = as.character(plot)))+
  geom_line()+
  theme_bw()

temp_2022_summary <- temp_2022_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(temp_5, na.rm = TRUE),
    temp_5_sd = sd(temp_5, na.rm = TRUE),
    temp_25_mean = mean(temp_25, na.rm = TRUE),
    temp_25_sd = sd(temp_25, na.rm = TRUE),
    temp_55_mean = mean(temp_55, na.rm = TRUE),
    temp_55_sd = sd(temp_55, na.rm = TRUE),
    temp_85_mean = mean(temp_85, na.rm = TRUE),
    temp_85_sd = sd(temp_85, na.rm = TRUE),
    .groups = "drop"
  )

# plot

ggplot(temp_2022_summary, aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2022_summary <- temp_2022_summary %>%
  mutate(
    doy = doy_int
  ) %>% 
  select(-doy_int)

# 2023

temp_2023_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2023/Plot_1_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2023_p1) <- temp_2023_p1[1,]
temp_2023_p1 <- temp_2023_p1[-1,]
temp_2023_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2023/Plot_2_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2023_p2) <- temp_2023_p2[1,]
temp_2023_p2 <- temp_2023_p2[-1,]
temp_2023_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2023/Plot_3_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2023_p3) <- temp_2023_p3[1,]
temp_2023_p3 <- temp_2023_p3[-1,]
temp_2023_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2023/Plot_4_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2023_p4) <- temp_2023_p4[1,]
temp_2023_p4 <- temp_2023_p4[-1,]

temp_2023 <- bind_rows(temp_2023_p1, temp_2023_p2, temp_2023_p3, temp_2023_p4)
temp_2023 <- temp_2023 %>%
  mutate(doy_int = as.integer(DOY),
         year = `% Year`,
         T_5_1 = as.numeric(T_5_1),
         T_5_2 = as.numeric(T_5_2),
         T_25_1 = as.numeric(T_25_1),
         T_25_2 = as.numeric(T_25_2)
  )


temp_2023_daily <- temp_2023 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    T_5_1 = mean(T_5_1, na.rm = TRUE),
    T_5_2 = mean(T_5_2, na.rm = TRUE),
    T_25_1 = mean(T_25_1, na.rm = TRUE),
    T_25_2 = mean(T_25_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("T_"),
    names_to = c(".value", "subsample"),
    names_pattern = "(T_\\d+)_(\\d)"
  ) %>% 
  filter(!c(plot == 2 & subsample == 1 & doy_int > 350)) # after exploration I saw that values from subsample 1, plot 2 after doy 350 were outliers

temp_2023_daily %>% 
  ggplot(aes(x = doy_int, y = T_5, color = subsample))+
  geom_line()+
  facet_wrap(plot~.)+
  theme_bw()

temp_2023_summary <- temp_2023_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(T_5, na.rm = TRUE),
    temp_5_sd = sd(T_5, na.rm = TRUE),
    temp_25_mean = mean(T_25, na.rm = TRUE),
    temp_25_sd = sd(T_25, na.rm = TRUE),
    .groups = "drop"
  )

# plot

temp_2023_summary %>% 
  ggplot(aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "Volumetric Water Content at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2023_summary <- temp_2023_summary %>%
  mutate(
    doy = doy_int,
    year = as.integer(year)
  ) %>%
  select(-doy_int)

# 2024

temp_2024_p1 <- read.csv("obs_data/measured_soil_temp/raw_data/2024/Plot_1_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2024_p1) <- temp_2024_p1[1,]
temp_2024_p1 <- temp_2024_p1[-1,]
temp_2024_p2 <- read.csv("obs_data/measured_soil_temp/raw_data/2024/Plot_2_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2024_p2) <- temp_2024_p2[1,]
temp_2024_p2 <- temp_2024_p2[-1,]
temp_2024_p3 <- read.csv("obs_data/measured_soil_temp/raw_data/2024/Plot_3_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2024_p3) <- temp_2024_p3[1,]
temp_2024_p3 <- temp_2024_p3[-1,]
temp_2024_p4 <- read.csv("obs_data/measured_soil_temp/raw_data/2024/Plot_4_shallow_soil_temp_moisture.csv", header = FALSE, , skip = 6)
colnames(temp_2024_p4) <- temp_2024_p4[1,]
temp_2024_p4 <- temp_2024_p4[-1,]

temp_2024 <- bind_rows(temp_2024_p1, temp_2024_p2, temp_2024_p3, temp_2024_p4)
temp_2024 <- temp_2024 %>%
  mutate(doy_int = as.integer(DOY),
         year = `% Year`,
         T_5_1 = as.numeric(T_5_1),
         T_5_2 = as.numeric(T_5_2),
         T_25_1 = as.numeric(T_25_1),
         T_25_2 = as.numeric(T_25_2)
  )

temp_2024_daily <- temp_2024 %>%
  group_by(year, doy_int, plot) %>%
  summarise(
    T_5_1 = mean(T_5_1, na.rm = TRUE),
    T_5_2 = mean(T_5_2, na.rm = TRUE),
    T_25_1 = mean(T_25_1, na.rm = TRUE),
    T_25_2 = mean(T_25_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("T_"),
    names_to = c(".value", "subsample"),
    names_pattern = "(T_\\d+)_(\\d)"
  ) %>% 
  filter(!c(plot == 2 & subsample == 1)) # after some exploration I realized that subsample 1 y plot 2 had some outliers, so i decided not to consider it.

temp_2024_daily %>% 
  ggplot(aes(x = doy_int, y = T_25, color = subsample))+
  geom_line()+
  facet_wrap(plot~.)+
  theme_bw()

temp_2024_summary <- temp_2024_daily %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(year, doy_int, group) %>%
  summarise(
    temp_5_mean = mean(T_5, na.rm = TRUE),
    temp_5_sd = sd(T_5, na.rm = TRUE),
    temp_25_mean = mean(T_25, na.rm = TRUE),
    temp_25_sd = sd(T_25, na.rm = TRUE),
    .groups = "drop"
  )

# plot

temp_2024_summary %>% 
  ggplot(aes(x = doy_int, y = temp_5_mean, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.3, color = NA) +
  labs(
    title = "temp at 5 cm",
    x = "Day of Year",
    y = "temp (cm³/cm³)",
    color = "Plot Group",
    fill = "Plot Group"
  ) +
  theme_bw()

temp_2024_summary <- temp_2024_summary %>%
  mutate(
    doy = doy_int,
    year = as.integer(year)
  ) %>%
  select(-doy_int)

# merging all temperature data

temp_2018_2024 <- bind_rows(temp_2018_summary, temp_2019_summary, temp_2020_summary, temp_2021_summary, temp_2022_summary, temp_2023_summary, temp_2024_summary)

temp_2018_2024 %>% 
  write_xlsx("obs_data/measured_soil_temp/temp_2018_2024.xlsx") 


############################################################
