# 25/10/2024
# file to extract climate data from Elora weather station, to use in DNDC modeling

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

# estimate total radiatio

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
data2022 <- read.table("data/climate/elora2022.txt", sep = "\t", header = FALSE, col.names = col_names, skip = 1)
data2022 <- data2022 %>%
  mutate(across(-c(julian_day), as.numeric))

mean(data2022$obs_max_temp_c, na.rm = TRUE)
mean(data2022$obs_min_temp_c, na.rm = TRUE)
sum(data2022$obs_precip_cm, na.rm = TRUE)
mean(data2022$obs_wind_2m_m_s, na.rm = TRUE)
sum(data2022$obs_rad_mjm2, na.rm = TRUE)
mean(data2022$obs_rh_pct, na.rm = TRUE)









