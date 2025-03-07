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
########## tower p12 ##############

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
######## tower p3 ###############

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
