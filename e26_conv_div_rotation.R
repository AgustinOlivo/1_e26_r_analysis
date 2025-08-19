###########################################################################
########## R script - Conventional vs Diverse Rotation Project  ###########
###########################################################################

# owner: Agustin Olivo - aolivo@uoguelph.ca
# project: comparing environmental and productivity outcomes for a conventional and a diverse crop rotation

##################################
####### GENERAL #########

# set wd
getwd()
setwd("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/r_analysis")

# install packages
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("Metrics")
# install.packages("hydroGOF")
# install.packages("lubridate")
# install.packages(c("Metrics", "hydroGOF"))
# install.packages("ggh4x")
# install.packages("writexl")
# install.packages("ggpattern")

# load packages 

library(tidyverse)
#library(dplyr)
library(readxl)
#library(ggplot2)
library(janitor)
#library(lubridate)
library(Metrics)
library(hydroGOF)
library("ggh4x")
library(writexl)
library(usethis)
library(ggpattern)

# suggested colors for graphs
c("#F0E442", "#D55E00","#CC79A7","#E69F00", "#0072B2", "#009E73","#56B4E9" , "#000000" ,"#999999")

# general functions

# Function to convert doy to crop_doy
convert_to_crop_doy <- function(year, doy) {
  if (doy >= 121) {
    return(doy - 120)
  } else {
    return(doy + 245) # 365 - 120 = 245; so doy 1 becomes 246
  }
}

# # Manual function to calculate NSE
# nse <- function(observed, modeled) {
#   # Compute NSE
#   mean_observed <- mean(observed)
#   numerator <- sum((modeled - observed)^2)
#   denominator <- sum((observed - mean_observed)^2)
#   nse_value <- 1 - (numerator / denominator)
#   return(nse_value)
# }
# 
# # Manual function to calculate Index of Agreement (d)
# index_of_agreement <- function(observed, modeled) {
#   # Compute Index of Agreement (d)
#   mean_observed <- mean(observed)
#   numerator <- sum((modeled - observed)^2)
#   denominator <- sum((abs(modeled - mean_observed) + abs(observed - mean_observed))^2)
#   d_value <- 1 - (numerator / denominator)
#   return(d_value)
# }
# 
# # Function to calculate metrics
# calculate_metrics <- function(observed, modeled) {
#   
#   # Remove NA values
#   valid_idx <- !is.na(observed) & !is.na(modeled)
#   observed_valid <- observed[valid_idx]
#   modeled_valid <- modeled[valid_idx]
#   
#   # Calculate metrics
#   pbias_value <- pbias(observed_valid, modeled_valid)
#   nse_value <- nse(observed_valid, modeled_valid)
#   rmse_value <- rmse(observed_valid, modeled_valid)
#   rrms_value <- (rmse_value / mean(observed_valid)) * 100
#   d_value <- index_of_agreement(observed_valid, modeled_valid)
#   r2_value <- cor(observed_valid, modeled_valid)^2
#   
#   # Create a dataset with the results
#   metrics_results <- data.frame(
#     PBIAS = round(pbias_value, digits = 2),
#     NSE = round(nse_value, digits = 2),
#     RMSE = round(rmse_value, digits = 2),
#     RRMSE_percent = round(rrms_value, digits = 2),
#     Index_of_Agreement_d = round(d_value, digits = 2),
#     R2 = round(r2_value, digits = 2)
#   )
#   
#   return(metrics_results)
# }

# Function to read .txt files for annual yield results (year value in line 7 for conv)

# new functions to estimate error metrics:

# Index of Agreement (d)
index_of_agreement <- function(observed, modeled) {
  mean_obs <- mean(observed)
  numerator <- sum((modeled - observed)^2)
  denominator <- sum((abs(modeled - mean_obs) + abs(observed - mean_obs))^2)
  d_value <- 1 - (numerator / denominator)
  return(d_value)
}

# Root Mean Square Error (RMSE)
rmse <- function(observed, modeled) {
  sqrt(mean((modeled - observed)^2))
}

# Normalized RMSE by mean of observed values (NRMSE %)
nrmse_mean <- function(observed, modeled) {
  rmse_val <- sqrt(mean((modeled - observed)^2))
  rrmse_val <- (rmse_val / mean(observed)) * 100
  return(rrmse_val)
}

# Normalized RMSE by range of observed values (NRMSE %)
nrmse_range <- function(observed, modeled) {
  rmse_val <- sqrt(mean((modeled - observed)^2))
  obs_range <- max(observed) - min(observed)
  if (obs_range == 0) return(NA)
  nrmse_val <- (rmse_val / obs_range) * 100
  return(nrmse_val)
}

# Normalized Average Relative Error (NARE %), Option A from the paper
nare <- function(observed, modeled) {
  error_sum <- sum(modeled - observed)
  mean_observed <- mean(observed)
  n <- length(observed)
  nare_value <- (error_sum / (n * mean_observed)) * 100
  return(nare_value)
}

# Main metrics function
calculate_metrics <- function(observed, modeled) {
  # Identify valid (non-NA) pairs
  valid_idx <- !is.na(observed) & !is.na(modeled)
  observed_valid <- observed[valid_idx]
  modeled_valid <- modeled[valid_idx]
  
  # Count valid observations
  n_valid <- length(observed_valid)
  
  # Calculate metrics
  metrics_results <- data.frame(
    n_valid_obs = n_valid,
    Measured_Avg = round(mean(observed_valid), 2),
    Modeled_Avg = round(mean(modeled_valid), 2),
    RMSE =  round(rmse(observed_valid, modeled_valid), 2),
    NRMSE_mean_percent = round(nrmse_mean(observed_valid, modeled_valid), 2),
    NRMSE_range_percent = round(nrmse_range(observed_valid, modeled_valid), 2),
    Index_of_Agreement_d = round(index_of_agreement(observed_valid, modeled_valid), 2),
    NARE_percent = round(nare(observed_valid, modeled_valid), 2)
  )
  
  return(metrics_results)
}

# other functions 
extract_info <- function(file_path) {
  # Read the file
  lines <- readLines(file_path)
  
  # Initialize values as NA
  year_value <- NA
  trt_name <- NA
  crop_name <- NA
  grain_c_value <- NA
  crop_c_value <- NA
  stubble_c_value <- NA
  
  # Extract the "Year" and "Treatment name"
  year_line <- grep("Year", lines, value = TRUE)
  if (length(year_line) > 0) {
    split_year_line <- strsplit(year_line, "\\s+")[[1]]
    if (length(split_year_line) >= 7) {
      year_value <- as.numeric(split_year_line[7])
      trt_name <- as.character(split_year_line[5])
    }
  }
  
  # Extract the "Crop name"
  crop_line <- grep("Crop name", lines, value = TRUE)
  if (length(crop_line) > 0) {
    crop_name <- trimws(sub("Crop name", "", crop_line))
  }
  
  # Extract the "Grain C"
  grain_c_line <- grep("-- Grain C", lines, value = TRUE)
  if (length(grain_c_line) > 0) {
    grain_c_value <- as.numeric(gsub("[^0-9.]", "", grain_c_line))
  }
  
  # Extract the "Crop C"
  crop_c_line <- grep("Crop C", lines, value = TRUE)
  if (length(crop_c_line) > 0) {
    crop_c_value <- as.numeric(gsub("[^0-9.]", "", crop_c_line))
  }
  
  # Extract the "Stubble C"
  stubble_c_line <- grep("Stubble", lines, value = TRUE)
  if (length(stubble_c_line) > 0) {
    stubble_c_value <- as.numeric(gsub("[^0-9.]", "", stubble_c_line))
  }
  
  # Return a data frame with the extracted values
  return(data.frame(trt = trt_name, year = year_value, crop_name = crop_name, crop_c_kg_ha = crop_c_value, 
                    grain_c_kg_ha = grain_c_value, stubble_c_kg_ha = stubble_c_value, 
                    stringsAsFactors = FALSE))
}

extract_info2 <- function(file_path) {
  lines <- readLines(file_path)
  results <- list()  # Initialize results list
  
  # Extract the year
  year_line <- grep("Year", lines, value = TRUE)
  if (length(year_line) > 0) {
    split_year_line <- strsplit(year_line, "\\s+")[[1]]
    year_value <- as.numeric(split_year_line[which(split_year_line == "Year") + 1])  # Get the number after "Year"
    trt_name <- as.character(split_year_line[5])  # Treatment name
  } else {
    year_value <- NA
    trt_name <- NA
  }
  
  # Extract crop sections
  crop_sections <- grep("CROP SECTION", lines)
  
  for (i in 1:length(crop_sections)) {
    if (i == length(crop_sections)) {
      crop_lines <- lines[crop_sections[i]:length(lines)]
    } else {
      crop_lines <- lines[crop_sections[i]:(crop_sections[i + 1] - 1)]
    }
    
    # Print crop lines for debugging
    print(crop_lines)
    
    # Extract crop names
    crop_names_line <- grep("Crop name", crop_lines, value = TRUE)
    if (length(crop_names_line) > 0) {
      crop_names <- unlist(strsplit(trimws(gsub("Crop name", "", crop_names_line)), "\\s+"))
    } else {
      crop_names <- NA
    }
    
    # Extract Crop C, Grain C, and Stubble C
    crop_c_line <- grep("Crop C", crop_lines, value = TRUE)
    crop_c_values <- if (length(crop_c_line) > 0) {
      as.numeric(unlist(strsplit(trimws(gsub("Crop C", "", crop_c_line)), "\\s+")))
    } else {
      rep(NA, length(crop_names))
    }
    
    grain_c_line <- grep("-- Grain C", crop_lines, value = TRUE)
    grain_c_values <- if (length(grain_c_line) > 0) {
      as.numeric(unlist(strsplit(trimws(gsub("-- Grain C", "", grain_c_line)), "\\s+")))
    } else {
      rep(NA, length(crop_names))
    }
    
    stubble_c_line <- grep("Stubble", crop_lines, value = TRUE)
    stubble_c_values <- if (length(stubble_c_line) > 0) {
      as.numeric(unlist(strsplit(trimws(gsub("Stubble", "", stubble_c_line)), "\\s+")))
    } else {
      rep(NA, length(crop_names))
    }
    
    # Append the results for each crop
    for (j in seq_along(crop_names)) {
      results[[length(results) + 1]] <- data.frame(
        trt = trt_name,
        year = year_value,
        crop_name = crop_names[j],
        crop_c_kg_ha = ifelse(j <= length(crop_c_values), crop_c_values[j], NA),
        grain_c_kg_ha = ifelse(j <= length(grain_c_values), grain_c_values[j], NA),
        stubble_c_kg_ha = ifelse(j <= length(stubble_c_values), stubble_c_values[j], NA),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine results into a single data frame
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(data.frame(trt = NA, year = NA, crop_name = NA, crop_c_kg_ha = NA, grain_c_kg_ha = NA, stubble_c_kg_ha = NA, stringsAsFactors = FALSE))
  }
}

#########################

##################################
######## LOADING OBSERVED DATA ####

######## import observed data crop yield ######

obs_yield <- read_excel("obs_data/measured_crop/obs_yield_e26_2025_02_06.xlsx", sheet = "obs_yield_main")
obs_yield$type <- "obs"
obs_yield$grain_t_ha <- as.numeric(obs_yield$grain_t_ha)
obs_yield$crop_yield_obs_kg_ha <- obs_yield$grain_t_ha*1000
str(obs_yield)
obs_yield_mean_sd <- obs_yield %>%
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(year, crop_order, crop_type, group) %>%
  summarise(
    crop_yield_obs_kg_ha_mean = mean(crop_yield_obs_kg_ha, na.rm = TRUE),
    crop_yield_obs_kg_ha_sd = sd(crop_yield_obs_kg_ha , na.rm = TRUE),
    crop_c_removed_obs_kg_ha_mean = mean(grain_c_kg_ha_obs, na.rm = TRUE),
    crop_c_removed_obs_kg_ha_sd = sd(grain_c_kg_ha_obs , na.rm = TRUE),
    grain_c_removed_obs_frac_mean = mean(c_content_grain, na.rm = TRUE),
    grain_c_removed_obs_frac_sd = sd(c_content_grain , na.rm = TRUE),
    .groups = "drop"
  )

# separating data by plot

obs_yield_p1 <- filter(obs_yield, plot == "1")
obs_yield_p2 <- filter(obs_yield, plot == "2")
obs_yield_p3 <- filter(obs_yield, plot == "3")
obs_yield_p4 <- filter(obs_yield, plot == "4")

obs_yield_mean_sd_p12 <- filter(obs_yield_mean_sd, group == "P1_P2")
obs_yield_mean_sd_p34 <- filter(obs_yield_mean_sd, group == "P3_P4")

######## import observed data straw ######

obs_straw_p34 <- read_excel("obs_data/measured_crop/obs_yield_e26_2025_02_06.xlsx", sheet = "straw_winter_wheat")
obs_straw_p34$type <- "obs"
obs_straw_p34_mean_sd <- obs_straw_p34 %>%
  mutate(
    group = case_when(
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(year, group) %>%
  summarise(
    straw_obs_kg_c_ha_mean = mean(straw_average_kg_c_ha, na.rm = TRUE),
    straw_obs_kg_c_ha_sd = sd(straw_average_kg_c_ha , na.rm = TRUE),
    .groups = "drop"
  )


######### import observed data soil temperature  ######

obs_soil_temp <- read_excel("obs_data/measured_soil_temp/temp_2018_2024.xlsx")
obs_soil_temp_p12 <- obs_soil_temp %>% 
  filter(group ==  "P1_P2")
obs_soil_temp_p34 <- obs_soil_temp %>% 
  filter(group ==  "P3_P4")

str(obs_soil_temp_p12)

# obs_soil_temp$date <- as.Date(obs_soil_temp$Date, format = "%m/%d/%Y")
# obs_soil_temp$year <- year(obs_soil_temp$date)
# obs_soil_temp$month <- month(obs_soil_temp$date)

######### import observed data soil volumetric water content  ######

obs_soil_vwc <- read_excel("obs_data/measured_vwc/vwc_2018_2024.xlsx")
obs_soil_vwc_p12 <- obs_soil_vwc %>% 
  filter(group ==  "P1_P2")
obs_soil_vwc_p34 <- obs_soil_vwc %>% 
  filter(group ==  "P3_P4")

######### import observed data soil nitrogen  ######

obs_soil_n <- read.csv("obs_data/measured_nh4_no3/E26_Nh4_NO3_dndc.csv", header = TRUE)
obs_soil_n$date <- parse_date_time(obs_soil_n$date, orders = c("dmy", "d/m/y", "d/m/Y"))
obs_soil_n$date <- as.Date(obs_soil_n$date)
obs_soil_n$doy <- obs_soil_n$date_jul

# estimating mean and sd for plots 1&2 and 3&4
obs_soil_n_mean_sd <- obs_soil_n %>%
  filter(!c(year==2023 & doy == 135 & plot == "P3")) %>%  # filtering out what seems to be an outlier.
  mutate(
    group = case_when(
      plot %in% c("P1", "P2") ~ "P1_P2",
      plot %in% c("P3", "P4") ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(year, group, doy) %>%
  summarise(
    nh4_kg_n_ha_mean = mean(nh4_kg_n_ha, na.rm = TRUE),
    nh4_kg_n_ha_sd = sd(nh4_kg_n_ha, na.rm = TRUE),
    no3_kg_n_ha_mean = mean(no3_kg_n_ha, na.rm = TRUE),
    no3_kg_n_ha_sd = sd(no3_kg_n_ha, na.rm = TRUE),
    .groups = "drop"
  )

obs_soil_n_mean_sd_p12 <- obs_soil_n_mean_sd %>% 
  filter(group == "P1_P2" & year > 2017) %>% 
  filter(!c(year==2021 & doy %in% 245:255)) # filtering out outlier

obs_soil_n_mean_sd_p34 <- obs_soil_n_mean_sd %>% 
  filter(group == "P3_P4" & year > 2017)%>% 
  filter(!c(year==2023 & doy == 185))

# # separating data by plot
# 
# obs_soil_n_p1 <- filter(obs_soil_n, plot == "P1" & year > 2017)
# obs_soil_n_p2 <- filter(obs_soil_n, plot == "P2" & year > 2017)
# obs_soil_n_p3 <- filter(obs_soil_n, plot == "P3" & year > 2017)
# obs_soil_n_p4 <- filter(obs_soil_n, plot == "P4" & year > 2017)
# 
# 
# # separating data by plot
# 
# obs_soil_n_p1_long <- filter(obs_soil_n, plot == "P1")
# obs_soil_n_p2_long <- filter(obs_soil_n, plot == "P2")
# obs_soil_n_p3_long <- filter(obs_soil_n, plot == "P3")
# obs_soil_n_p4_long <- filter(obs_soil_n, plot == "P4")

# graph to check observed data for soil N

# obs_soil_n %>% 
#   ggplot(aes(x = doy, y = nh4_kg_n_ha)) +
#   geom_col(color = "black") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_grid(cols = vars(year), rows = vars(plot)) +
#   ylab("Soil NH4 0-15 cm (kg N/ha)")

###### import observed data co2  ######

# I generated a few different files with outputs from reddyproc

# main variables

obs_co2_p1a <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_nee_conv.xlsx")
obs_co2_p1b <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_reco_dt_conv.xlsx")
obs_co2_p1c <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_reco_nt_conv.xlsx")
obs_co2_p1d <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_gpp_dt_conv.xlsx")
obs_co2_p1e <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_gpp_nt_conv.xlsx")
obs_co2_p1 <- list(
  obs_co2_p1a,
  obs_co2_p1b,
  obs_co2_p1c,
  obs_co2_p1d,
  obs_co2_p1e
) %>%
  reduce(full_join, by = c("Year", "DoY"))
obs_co2_p1$year = obs_co2_p1$Year
obs_co2_p1$doy = obs_co2_p1$DoY

obs_co2_p1 <- obs_co2_p1 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

obs_co2_p1 <- obs_co2_p1 %>%
  mutate(across(-c(crop_year, doy), as.numeric))


obs_co2_p3a <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_nee_div.xlsx")
obs_co2_p3b <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_reco_dt_div.xlsx")
obs_co2_p3c <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_reco_nt_div.xlsx")
obs_co2_p3d <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_gpp_dt_div.xlsx")
obs_co2_p3e <- read_excel("obs_data/measured_co2/mean_sd_daily_gC_all_gpp_nt_div.xlsx")
obs_co2_p3 <- list(
  obs_co2_p3a,
  obs_co2_p3b,
  obs_co2_p3c,
  obs_co2_p3d,
  obs_co2_p3e
) %>%
  reduce(full_join, by = c("Year", "DoY"))
obs_co2_p3$year = obs_co2_p3$Year
obs_co2_p3$doy = obs_co2_p3$DoY

obs_co2_p3 <- obs_co2_p3 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

obs_co2_p3 <- obs_co2_p3 %>%
  mutate(across(-c(crop_year, doy), as.numeric))

# Add crop_doy column that will be used for graph

obs_co2_p1 <- obs_co2_p1 %>%
  filter(!is.na(crop_year)) %>%
  mutate(doy_crop = mapply(convert_to_crop_doy, Year, doy))

obs_co2_p3 <- obs_co2_p3 %>%
  filter(!is.na(crop_year)) %>%
  mutate(doy_crop = mapply(convert_to_crop_doy, Year, doy))

# graph observed co2 data

# head(obs_co2_p1)
obs_co2_p1 %>%
  ggplot(aes(x = doy, y = obs_nee)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year~., nrow = 4) +
  ylab("NEE")

###### import observed data n2o  ######

# importing datasets for each of the four individual plots

obs_n2o_p1p2_plot <- read_excel("obs_data/measured_n2o/flux_grad_p1p2_2018_2024_n2o_daily_plot.xlsx")
obs_n2o_p3p4_plot <- read_excel("obs_data/measured_n2o/flux_grad_p3p4_2018_2024_n2o_daily_plot.xlsx")
obs_n2o_p1p2p3p4 <- bind_rows(obs_n2o_p1p2_plot, obs_n2o_p3p4_plot)

# estimating mean and sd

obs_n2o_p1p2p3p4_mean_sd <- obs_n2o_p1p2p3p4 %>%
  mutate(
    group = case_when(
      plot %in% c(1,2) ~ "P1_P2",
      plot %in% c(3,4) ~ "P3_P4",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(year, doy, group) %>%
  summarise(
    n2o_flux_g_n_ha_day_mean = mean(n2o_flux_g_n_ha_day, na.rm = TRUE),
    n2o_flux_g_n_ha_day_sd = sd(n2o_flux_g_n_ha_day, na.rm = TRUE),
    .groups = "drop"
  )

obs_n2o_p1p2_mean_sd <- filter(obs_n2o_p1p2p3p4_mean_sd, group == "P1_P2")
obs_n2o_p3p4_mean_sd <- filter(obs_n2o_p1p2p3p4_mean_sd, group == "P3_P4")


obs_n2o_p1p2_mean_sd <- obs_n2o_p1p2_mean_sd %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

obs_n2o_p3p4_mean_sd <- obs_n2o_p3p4_mean_sd %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )


###### loading management dates current P1 and P3 to include in graphs  ######

# mgmt_dates_p1 <- as.data.frame( do.call( rbind, list(
#   c(131,2012), # planting
#   c(276,2012), # harvest
#   c(129,2013), # planting
#   c(295,2013), # harvest
#   c(146,2014), # planting
#   c(308,2014), # harvest
#   c(133,2015), # planting
#   c(300,2015), # harvest
#   c(133,2016), # planting
#   c(280,2016), # harvest
#   c(138,2017), # planting
#   c(313,2017), # harvest
#   c(140,2018), # planting
#   c(303,2018), # harvest
#   c(150,2019), # planting
#   c(273,2019), # harvest
#   c(143,2020), # planting
#   c(267,2020), # harvest
#   c(134,2021), # planting
#   c(307,2021), # harvest
#   c(138,2022), # planting
#   c(276,2022), # harvest
#   c(136,2023), # planting
#   c(277,2023), # harvest
#   c(143,2024), # planting
#   c(303,2024) # harvest
# 
# )))
# colnames(mgmt_dates_p1) <- c("doy", "year")

mgmt_dates_p1 <- as.data.frame( do.call( rbind, list(
  c(140,"18/19"), # planting
  c(303,"18/19"), # harvest
  c(150,"19/20"), # planting
  c(273,"19/20"), # harvest
  c(143,"20/21"), # planting
  c(267,"20/21"), # harvest
  c(134,"21/22"), # planting
  c(307,"21/22"), # harvest
  c(138,"22/23"), # planting
  c(276,"22/23"), # harvest
  c(136,"23/24"), # planting
  c(277,"23/24") # harvest
  # c(143,"2024"), # planting
  # c(303,"2024") # harvest
  
)))
colnames(mgmt_dates_p1) <- c("doy", "crop_year")

mgmt_dates_p1 <- mgmt_dates_p1 %>%
  mutate(
    doy_crop = ifelse(as.numeric(doy) >= 121, as.numeric(doy) - 120, as.numeric(doy) + 245)
  )

# mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
#   c(131,2012), # planting
#   c(276,2012), # harvest
#   c(129,2013), # planting
#   c(295,2013), # harvest
#   c(146,2014), # planting
#   c(308,2014), # harvest
#   c(133,2015), # planting
#   c(300,2015), # harvest
#   c(133,2016), # planting
#   c(280,2016), # harvest
#   c(138,2017), # planting
#   c(313,2017), # harvest
#   c(140,2018), # planting
#   c(303,2018), # harvest
#   c(150,2019), # planting
#   c(273,2019), # harvest
#   c(213,2020), # harvest
#   c(134,2021), # planting
#   c(307,2021), # harvest
#   c(138,2022), # planting
#   c(276,2022), # harvest
#   c(221,2023), # harvest
#   c(143,2024), # planting
#   c(303,2024) # harvest
# )))
# colnames(mgmt_dates_p3) <- c("doy", "year")

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(140,"18/19"), # planting
  c(303,"18/19"), # harvest
  c(150,"19/20"), # planting
  c(273,"19/20"), # harvest
  c(213,"20/21"), # harvest
  c(134,"21/22"), # planting
  c(307,"21/22"), # harvest
  c(138,"22/23"), # planting
  c(276,"22/23"), # harvest
  c(221,"23/24") # harvest
)))
colnames(mgmt_dates_p3) <- c("doy", "crop_year")

mgmt_dates_p3 <- mgmt_dates_p3 %>%
  mutate(
    doy_crop = ifelse(as.numeric(doy) >= 121, as.numeric(doy) - 120, as.numeric(doy) + 245)
  )

# loading fertilizer application dates current P1 and P3 to include in graphs

fert_dates_p1 <- as.data.frame( do.call( rbind, list(
  c(293,2012),
  c(318,2013),
  c(178,2014),
  c(332,2014),
  c(132,2015),
  c(133,2015),
  c(132,2016),
  c(133,2016),
  c(138,2017),
  c(139,2017),
  c(137,2018),
  c(138,2018),
  c(150,2019),
  c(168,2021)
)))
colnames(fert_dates_p1) <- c("doy", "year")

fert_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(126,2008),
  c(127,2008),
  c(174,2009),
  c(132,2009),
  c(104,2011),
  c(292,2011),
  c(130,2012),
  c(131,2012),
  c(293,2012),
  c(128,2013),
  c(129,2013),
  c(317,2013),
  c(146,2014),
  c(149,2014),
  c(332,2014),
  c(133,2015),
  c(176,2015),
  c(133,2016),
  c(169,2016),
  c(139,2017),
  c(178,2017),
  c(138,2018),
  c(137,2018),
  c(150,2019),
  c(98,2020),
  c(134,2021),
  c(168,2021),
  c(131,2023),
  c(143,2024),
  c(183,2024)
)))
colnames(fert_dates_p3) <- c("doy", "year")

# loading weather data to include in some of the graphs

file_directory <- "obs_data/climate/"
file_list <- list.files(path = file_directory, pattern = "*.txt", full.names = TRUE)
col_names <- c("doy", "temp_max", "temp_min",  "prec", "wind", "radi", "hum")

# Read each file weather file, add the year column, and remove columns with only white spaces or "unknown"
df_list <- lapply(file_list, function(file_path) {
  # Extract the year from the file name (assuming the file name is in the format "eloraYYYY.txt")
  year <- gsub("elora|\\.txt", "", basename(file_path))  # Extract year from file name
  # Read the file, skipping the first line
  df <- read.table(file_path, header = FALSE, sep = "\t", skip = 1, fill = TRUE)  # fill = TRUE to handle varying columns
  # Ensure that the number of columns is the same as expected
  if (ncol(df) < length(col_names)) {
    # Pad with NAs if there are fewer columns than expected
    df <- cbind(df, matrix(NA, nrow = nrow(df), ncol = length(col_names) - ncol(df)))
  }
  # Assign the column names
  colnames(df) <- col_names
  # Remove columns that are completely empty or only contain white spaces or NAs
  df <- df[, apply(df, 2, function(col) any(trimws(col) != "" & !is.na(col)))]
  # Add the 'year' column
  df$year <- year
  return(df)
})

# Combine all the dataframes into one
climate <- do.call(rbind, df_list)

# plot some climate variables

climate %>%
  select(year, prec) %>%
  filter(!prec == "-999") %>%
  group_by(year) %>%
  summarise(
    prec = sum(prec, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = prec))+
  geom_col()+
  theme_bw()

climate %>%
  select(year, temp_min) %>%
  filter(!temp_min == "-999") %>%
  group_by(year) %>%
  summarise(
    temp_min = mean(temp_min, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = temp_min))+
  geom_col()+
  theme_bw()

climate %>%
  select(year, temp_max) %>%
  filter(!temp_max == "-999") %>%
  group_by(year) %>%
  summarise(
    temp_max = mean(temp_max, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = temp_max))+
  geom_col()+
  theme_bw()

climate %>%
  select(year, wind) %>% 
  filter(!wind == "-999") %>% 
  group_by(year) %>%
  summarise(
    wind = mean(wind, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = wind))+
  geom_col()+
  theme_bw()

climate %>%
  select(year, radi) %>% 
  filter(!radi == "-999") %>% 
  group_by(year) %>%
  summarise(
    radi = sum(radi, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = radi))+
  geom_col()+
  theme_bw()

#################################

###################################
####### CONVENTIONAL ROTATION #####
###################################

# loading data from DNDC ouputs (stored in C drive folder) related to the conventional rotation in my project, running statistics, and creating graphs (all by variable). 

###### CROP YIELD #######

### import modeled yield/c data for p1

file_paths <- list.files(path = "C:/DNDC/Result/Record/Site", pattern = "\\.txt$", full.names = TRUE) 
results <- do.call(rbind, lapply(file_paths, extract_info))
mod_yield_p1 <- results %>%
  #filter(year > 6) %>%
  mutate(year = case_when(
    year == 1 ~ 2008,
    year == 2 ~ 2009,
    year == 3 ~ 2010,
    year == 4 ~ 2011,
    year == 5 ~ 2012,
    year == 6 ~ 2013,
    year == 7 ~ 2014,
    year == 8 ~ 2015,
    year == 9 ~ 2016,
    year == 10 ~ 2017,
    year == 11 ~ 2018,
    year == 12 ~ 2019,
    year == 13 ~ 2020,
    year == 14 ~ 2021,
    year == 15 ~ 2022,
    year == 16 ~ 2023,
    year == 17 ~ 2024
  ))

mod_yield_p1$type <- "mod"
mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
mod_yield_p1 <- mod_yield_p1 %>% mutate(crop_type = case_when(
  crop_name == "New_crop" ~ "corn",
  crop_name == "Winter wheat" ~ "winter wheat",
  crop_name == "Soybean" ~ "soybean",
  crop_name == "Cover crop" ~ "cover crop"
))
mod_yield_p1$crop_yield_mod_kg_ha <- ifelse(mod_yield_p1$crop_type == "corn", mod_yield_p1$grain_c_kg_ha /0.42, ifelse(mod_yield_p1$crop_type == "soybean", mod_yield_p1$grain_c_kg_ha /0.42, ifelse(mod_yield_p1$crop_type == "winter wheat", mod_yield_p1$grain_c_kg_ha /0.41, mod_yield_p1$grain_c_kg_ha /0.40)))
mod_yield_p1$grain_c_kg_ha_mod <- mod_yield_p1$grain_c_kg_ha

### graph

combined_data <- merge(obs_yield_mean_sd_p12, mod_yield_p1, by = c("year", "crop_type")) %>% 
  filter(year>2017)
combined_data <- combined_data %>%
  mutate(
    crop_year = case_when(
      year == 2018 ~ "18/19",
      year == 2019 ~ "19/20",
      year == 2020 ~ "20/21",
      year == 2021 ~ "21/22",
      year == 2022 ~ "22/23",
      year == 2023 ~ "23/24",
      year == 2024 ~ "24/25"
    )
  )

# yield 
library(ggpattern)

combined_data %>%
  filter(year > 2017) %>% 
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha_mean, crop_yield_mod_kg_ha), 
    names_to = "yield_type", 
    values_to = "yield_value"
  ) %>%
  mutate(
    yield_category = ifelse(grepl("obs", yield_type), "O", "M"),
    yield_sd = case_when(
      yield_type == "crop_yield_obs_kg_ha_mean" ~ crop_yield_obs_kg_ha_sd,
      TRUE ~ NA_real_
    )
  ) %>%
  ggplot(aes(
    x = yield_category, 
    y = yield_value,
    pattern = yield_category,
    pattern_fill = yield_category,
    pattern_colour = yield_category,
    fill = yield_category
  )) +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.2,
    pattern_angle = 45,
    pattern_key_scale_factor = 2
  ) +
  geom_errorbar(
    aes(ymin = yield_value - yield_sd, ymax = yield_value + yield_sd),
    position = position_dodge(width = 0.9),
    width = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 13500),
    breaks = seq(0, 13500, by = 2500)
  ) +
  facet_wrap(~ crop_year, nrow = 1) +
  scale_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_manual(values = c("O" = "stripe", "M" = "none")) +
  scale_pattern_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_colour_manual(values = c("O" = "white", "M" = "#999999")) +
  labs(y = expression(Crop~Yield~(kg~ha^{-1}))) +
  geom_text(
    aes(label = round(yield_value, 0), angle = 90), 
    position = position_dodge(width = 0.9), 
    vjust = 0.5,
    hjust = -0.3
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "yield_conv.png",  width = 4.5, height = 3.8, dpi = 300)

## statistics

# yield 

#data frame to be used when comparing multiple (you can load it once, and then comment it out so you keep storing new results in the same dataframe for comparison)
# metrics_yield_df <- data.frame( # data frame to store results from multiple runs
#   one = numeric(0),
#   two = numeric(0),
#   three = numeric(0),
#   four = numeric(0),
#   five = numeric(0),
#   six = numeric(0),
#   seven = numeric(0)
# )

# storing results of statistics in dataframe

# estimation of statistics 

# general

# modeled <- combined_data$crop_yield_mod_kg_ha
# observed <- combined_data$crop_yield_obs_kg_ha_mean
# metrics_yield <- calculate_metrics(observed, modeled)
# metrics_yield_df <- rbind(metrics_yield_df, metrics_yield)
# metrics_yield_df

# corn

modeled <- filter(combined_data, crop_type == "corn")$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "corn")$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# soybean

modeled <- filter(combined_data, crop_type == "soybean")$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "soybean")$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield


###### SOIL TEMPERATURE ###########

# import modeled data

mod_conv_soil_temp <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_conv_soil_temp <- mod_conv_soil_temp[, c(1:27)]
colnames(mod_conv_soil_temp) <- mod_conv_soil_temp[3,]
mod_conv_soil_temp <- mod_conv_soil_temp[-c(1:3),]
mod_conv_soil_temp <- data.frame(lapply(mod_conv_soil_temp, as.numeric))
mod_conv_soil_temp$doy <- mod_conv_soil_temp$Day
mod_conv_soil_temp$temp_25 <- (mod_conv_soil_temp$X20cm + mod_conv_soil_temp$X30cm) / 2
mod_conv_soil_temp$temp_55 <- (mod_conv_soil_temp$X50cm + mod_conv_soil_temp$X60cm) / 2
mod_conv_soil_temp$temp_85 <- (mod_conv_soil_temp$X80cm + mod_conv_soil_temp$X90cm) / 2

mod_conv_soil_temp <- mod_conv_soil_temp %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
  ))

# graphs for plot 1
combined_data_temp_p12 <- merge(obs_soil_temp_p12 , mod_conv_soil_temp, by = c("year", "doy"))

combined_data_temp_p12 <- combined_data_temp_p12 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

combined_data_temp_p12 <- combined_data_temp_p12 %>% 
  filter(!is.na(crop_year))

# soil temperature at 5cm

combined_data_temp_p12 %>% 
  ggplot(aes(x = doy, y = X5cm))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_5_mean), 
            color = "#D55E00", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.2, fill = "#D55E00") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 5 cm (C)")

# soil temperature at 25

combined_data_temp_p12 %>% 
  ggplot(aes(x = doy, y = temp_25))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_25_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_25_mean - temp_25_sd, ymax = temp_25_mean + temp_25_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 25 cm (C)")

# soil temperature at 55cm

combined_data_temp_p12 %>% 
  ggplot(aes(x = doy, y = temp_55))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_55_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_55_mean - temp_55_sd, ymax = temp_55_mean + temp_55_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 55 cm (C)")

# soil temperature at 85cm

combined_data_temp_p12 %>% 
  ggplot(aes(x = doy, y = temp_85))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_85_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_85_mean - temp_85_sd, ymax = temp_85_mean + temp_85_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 85 cm (C)")


### statistics

# temperature 5 cm
observed <- combined_data_temp_p12$temp_5_mean
modeled <- combined_data_temp_p12$X5cm
calculate_metrics(observed, modeled)

# temperature 25 cm
observed <- combined_data_temp_p12$temp_25_mean
modeled <- combined_data_temp_p12$temp_25
calculate_metrics(observed, modeled)

###### SOIL WATER ###########

# import modeled data for p1

mod_soil_water_p12 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_soil_water_p12 <- mod_soil_water_p12[, c(1:5, 28:49)]
colnames(mod_soil_water_p12) <- mod_soil_water_p12[3,]
mod_soil_water_p12 <- mod_soil_water_p12[-c(1:3),]
mod_soil_water_p12 <- data.frame(lapply(mod_soil_water_p12, as.numeric))
mod_soil_water_p12$date_jul <- mod_soil_water_p12$Day
mod_soil_water_p12$wfps_25_mod <- (mod_soil_water_p12$X20cm + mod_soil_water_p12$X30cm) / 2
mod_soil_water_p12$vwc_5 <- mod_soil_water_p12$X5cm * (1.26/2.65)
mod_soil_water_p12$vwc_25 <- c((mod_soil_water_p12$X20cm + mod_soil_water_p12$X30cm)/2 * (1.30/2.65))
mod_soil_water_p12$vwc_55 <- c((mod_soil_water_p12$X50cm + mod_soil_water_p12$X60cm)/2 * (1.35/2.65))
mod_soil_water_p12$vwc_85 <- c((mod_soil_water_p12$X80cm + mod_soil_water_p12$X90cm)/2 * (1.40/2.65))
mod_soil_water_p12$doy <- mod_soil_water_p12$date_jul

mod_soil_water_p12 <- mod_soil_water_p12 %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
  ))

# graphs
combined_data_water_p12 <- left_join(mod_soil_water_p12, obs_soil_vwc_p12, by = c("year", "doy"))

combined_data_water_p12 <- combined_data_water_p12 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

combined_data_water_p12 <- combined_data_water_p12 %>% 
  filter(!is.na(crop_year))

# vwc 5 cm

climate_filtered <- climate %>%
  filter(doy > 90 & doy < 335) %>%  # Filter for the range of days of the year (March 31st to Dec 1st)
  filter(year >= 2018 & year <= 2024)

combined_data_water_p12 %>% 
  filter(doy > 90 & doy < 335) %>%  # Filter for the desired doy range
  ggplot() +
  geom_line(aes(x = doy, y = vwc_5_mean), color = "#D55E00", size = 1, linetype = 1, alpha = 0.7) +
  geom_ribbon(aes(x = doy, ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, fill = "#D55E00") +
  geom_line(aes(x = doy, y = vwc_5), size = 1, color = "darkgray", alpha = 0.7) +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw() +
  #geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2022)), aes(xintercept = DoY), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*0.05), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "VWC at 5 cm (cm3/cm3)", 
    limits = c(0, 0.6),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./0.05,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 12, 2)  # Set the secondary axis from 0 to 6 cm
    )
  )

# vwc 25 cm

combined_data_water_p12 %>% 
  filter(doy > 90 & doy < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot()+
  geom_line(aes(x = doy, y = vwc_25), size = 1, color = "gray")+
  geom_line(aes(x = doy, y = vwc_25_mean), 
            color = "#009E73", size = 1, linetype = 1)+
  geom_ribbon(aes(x = doy, ymin = vwc_25_mean - vwc_25_sd, ymax = vwc_25_mean + vwc_25_sd), 
              alpha = 0.3, fill = "#009E73") +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw()+
  ylab("VWC at 25 cm (cm3/cm3)")+
  #geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*0.05), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "VWC at 25 cm (cm3/cm3)", 
    limits = c(0, 0.6),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./0.05,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 12, 2)  # Set the secondary axis from 0 to 6 cm
    )
  )

### statistics

# vwc 5 cm

# data frame to be used when comparing multiple (you can load it once, and then comment it out so you keep storing new results in the same dataframe for comparison)
# metrics_vwc5_df <- data.frame(
#   one = numeric(0),
#   two = numeric(0),
#   three = numeric(0),
#   four = numeric(0),
#   five = numeric(0),
#   six = numeric(0),
#   seven = numeric(0)
# )

# estimation of statistics 
observed <- filter(combined_data_water_p12, Day > 90 & Day < 335)$vwc_5
modeled <- filter(combined_data_water_p12, Day > 90 & Day < 335)$vwc_5_mean
metrics_vwc5 <- calculate_metrics(observed, modeled)
# storing results of statistics in dataframe
metrics_vwc5_df <- rbind(metrics_vwc5_df, metrics_vwc5)
# visualizing dataframe with results
metrics_vwc5_df

# vwc 25 cm

# data frame to be used when comparing multiple (you can load it once, and then comment it out so you keep storing new results in the same dataframe for comparison)
# metrics_vwc25_df <- data.frame(
#   one = numeric(0),
#   two = numeric(0),
#   three = numeric(0),
#   four = numeric(0),
#   five = numeric(0),
#   six = numeric(0),
#   seven = numeric(0)
# )
# estimation of statistics 
observed <- filter(combined_data_water_p12, Day > 90 & Day < 335)$vwc_25
modeled <- filter(combined_data_water_p12, Day > 90 & Day < 335)$vwc_25_mean
metrics_vwc25 <- calculate_metrics(observed, modeled)
# storing results of statistics in dataframe
metrics_vwc25_df <- rbind(metrics_vwc25_df, metrics_vwc25)
# visualizing dataframe with results
metrics_vwc25_df

###### SOIL NITROGEN ##############

# import modeled data for p1

mod_conv_soil_n <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
combined_names <- paste(mod_conv_soil_n[3, ], mod_conv_soil_n[4, ], sep = "_")
colnames(mod_conv_soil_n) <- combined_names

mod_conv_soil_n <- mod_conv_soil_n[-c(1:4),]
mod_conv_soil_n <- data.frame(lapply(mod_conv_soil_n, as.numeric))

mod_conv_soil_n$date_jul <- mod_conv_soil_n$Day_

mod_conv_soil_n <- mod_conv_soil_n %>%
  #filter(Year_ > 6) %>%
  mutate(year = case_when(
    Year_ == 1 ~ 2008,
    Year_ == 2 ~ 2009,
    Year_ == 3 ~ 2010,
    Year_ == 4 ~ 2011,
    Year_ == 5 ~ 2012,
    Year_ == 6 ~ 2013,
    Year_ == 7 ~ 2014,
    Year_ == 8 ~ 2015,
    Year_ == 9 ~ 2016,
    Year_ == 10 ~ 2017,
    Year_ == 11 ~ 2018,
    Year_ == 12 ~ 2019,
    Year_ == 13 ~ 2020,
    Year_ == 14 ~ 2021,
    Year_ == 15 ~ 2022,
    Year_ == 16 ~ 2023,
    Year_ == 17 ~ 2024
    
  ))

mod_conv_soil_n$date <- as.Date(paste(mod_conv_soil_n$year, mod_conv_soil_n$date_jul), format = "%Y %j")
mod_conv_soil_n$doy <- mod_conv_soil_n$date_jul

mod_conv_soil_n$nh4_0_15 <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
mod_conv_soil_n$no3_0_15 <- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2

mod_conv_soil_n <- mod_conv_soil_n %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )


# plotting soil N 

mod_conv_soil_n %>%
  filter(year %in% 2018:2023) %>% 
  ggplot(aes(x = doy, y = no3_0_15))+
  geom_errorbar(data = filter(obs_soil_n_mean_sd_p12, year %in% c(2018:2023)),
                aes(
                  x=doy,
                  ymin = no3_kg_n_ha_mean - no3_kg_n_ha_sd, 
                  ymax = no3_kg_n_ha_mean + no3_kg_n_ha_sd),
                position = position_dodge(width = 1),
                width = 0.3, inherit.aes = FALSE, colour = "#009E73"
  ) +
  geom_errorbar(data = filter(obs_soil_n_mean_sd_p12, year %in% c(2018:2024)),
                aes(
                  x=doy,
                  ymin = nh4_kg_n_ha_mean - nh4_kg_n_ha_sd, 
                  ymax = nh4_kg_n_ha_mean + nh4_kg_n_ha_sd),
                position = position_dodge(width = 1),
                width = 0.3, inherit.aes = FALSE, colour = "#D55E00"
  ) +
  geom_line(colour = "#009E73", alpha = 0.75, size = 0.75)+
  geom_point(data = filter(obs_soil_n_mean_sd_p12, year %in% c(2018:2023)), aes(x=doy, y=no3_kg_n_ha_mean), colour = "#009E73", alpha=0.5)+
  geom_point(data = filter(obs_soil_n_mean_sd_p12, year %in% c(2018:2023)), aes(x=doy, y=nh4_kg_n_ha_mean), colour = "#D55E00", alpha=0.5)+
  geom_line(data = filter(mod_conv_soil_n, year %in% 2018:2023), aes(x=doy, y=nh4_0_15), colour = "#D55E00", alpha=0.75, size = 0.75)+
  facet_wrap(year~.)+
  theme_bw()+
  #geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  #geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  ylab("Soil N (kg N/ha)")

ggsave(filename = "soil_n_conv.png",  width = 10, height = 6, dpi = 300)

# plot all N losses as reported by DNDC

mod_conv_soil_n %>%
  filter(!is.na(crop_year)) %>% 
  group_by(crop_year) %>%
  summarise(
    n2o = sum(N2O.flux_),
    n2 = sum(N2.flux_),
    nh3 = sum(NH3.flux_),
    no3_l = sum(NO3.leach_),
    sum = n2o + n2 + nh3 + no3_l
  ) %>%
  pivot_longer(
    cols = c(n2o, n2,  nh3, no3_l,  sum),
    names_to = "losses",
    values_to = "value"
  ) %>%
  ggplot(aes(x = losses, y = value, fill = losses))+
  geom_col(position = position_dodge(), color = "black")+
  facet_wrap(crop_year~.)+
  theme_bw()+
  ylab("Annual N losses (kg N/ha)")+
  xlab("Pathways")+
  geom_text(aes(label = round(value, digits = 1)))

ggsave(filename = "n_lossess_conv.png",  width = 10, height = 6, dpi = 300)

### statistics

combined_data_n <- merge(mod_conv_soil_n, obs_soil_n_mean_sd_p12, by = c("year", "doy"))
combined_data_n <- combined_data_n %>% 
  filter(year>2017)

# nh4

# metrics_nh4_0_15_df <- data.frame(
#   one = numeric(0),
#   two = numeric(0),
#   three = numeric(0),
#   four = numeric(0),
#   five = numeric(0),
#   six = numeric(0),
#   seven = numeric(0)
# )

observed <- combined_data_n$nh4_kg_n_ha_mean 
modeled <- combined_data_n$nh4_0_15

metrics_nh4_0_15<- calculate_metrics(observed, modeled)

metrics_nh4_0_15_df <- rbind(metrics_nh4_0_15_df, metrics_nh4_0_15)
metrics_nh4_0_15_df

# no3

# metrics_no3_0_15_df <- data.frame(
#   one = numeric(0),
#   two = numeric(0),
#   three = numeric(0),
#   four = numeric(0),
#   five = numeric(0),
#   six = numeric(0),
#   seven = numeric(0)
# )

observed <- combined_data_n$no3_kg_n_ha_mean 
modeled <- combined_data_n$no3_0_15

metrics_no3_0_15<- calculate_metrics(observed, modeled)

metrics_no3_0_15_df <- rbind(metrics_no3_0_15_df, metrics_no3_0_15)
metrics_no3_0_15_df

###### CO2 ############

# import modeled data for p1

mod_co2_p1 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilC_1.csv", header = FALSE)
colnames(mod_co2_p1) <- mod_co2_p1[2, ]
colnames(mod_co2_p1) <- make.names(colnames(mod_co2_p1), unique = TRUE)
mod_co2_p1 <- mod_co2_p1[-c(1,2),]
mod_co2_p1 <- data.frame(lapply(mod_co2_p1, as.numeric))

mod_co2_p1$date_jul <- mod_co2_p1$Day

mod_co2_p1$mod_resp <- mod_co2_p1$Eco.respiration*1000/10000
mod_co2_p1$mod_gpp <- mod_co2_p1$Photosynthesis*(-1000/10000)
mod_co2_p1$mod_nee <- mod_co2_p1$X.NEE*1000/10000
mod_co2_p1 <- mod_co2_p1 %>%
  #filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
    
  ))

mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
mod_co2_p1$doy <- as.numeric(mod_co2_p1$Day)

# additional variables needed for graphs

mod_co2_p1 <- mod_co2_p1 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19",
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))

  )

mod_co2_p1 <- mod_co2_p1 %>%
  filter(!is.na(crop_year)) %>%
  mutate(
    doy_crop = case_when(
      doy >= 121 ~ doy - 120,          # DOY 121 → 1, 122 → 2, ..., 365 → 245
      doy < 121  ~ doy + (365 - 120)   # DOY 1 → 246, ..., 120 → 365
    )
  )

obs_co2_p1$year = obs_co2_p1$Year

#### statistics CO2 - calibration ####

merged_data_co2 <- mod_co2_p1 %>% 
  left_join(obs_co2_p1, by = c("year", "doy")) %>%
  mutate(crop_year = crop_year.x) %>% 
  filter(!is.na(crop_year))

metrics_df_daily_corn <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

metrics_df_daily_soy <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# gpp 

# daytime

# all years

# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_dt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# by crop

# daytime

# corn years calibration
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn <- rbind(metrics_df_daily_corn, metrics_gpp)
metrics_df_daily_corn

# soybean years calibration
predicted <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_soy <- rbind(metrics_df_daily_soy, metrics_gpp)
metrics_df_daily_soy

# nighttime

# all years

# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_nt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# corn years calibration
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn <- rbind(metrics_df_daily_corn, metrics_gpp)
metrics_df_daily_corn

# soybean years calibration
predicted <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_soy <- rbind(metrics_df_daily_soy, metrics_gpp)
metrics_df_daily_soy

# respiration

# daytime

# all years 

# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_dt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn calibration
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn <- rbind(metrics_df_daily_corn, metrics_resp)
metrics_df_daily_corn

# soybeans calibration
predicted <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_soy <- rbind(metrics_df_daily_soy, metrics_resp)
metrics_df_daily_soy

# nightime

# all years

# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_nt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn calibration
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn <- rbind(metrics_df_daily_corn, metrics_resp)
metrics_df_daily_corn

# soybeans calibration
predicted <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_soy <- rbind(metrics_df_daily_soy, metrics_resp)
metrics_df_daily_soy

# nee

# all years

# predicted <- merged_data_co2$mod_nee
# observed <- merged_data_co2$nee_g_c_m2_day
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
# metrics_df_daily

# corn calibration
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily_corn <- rbind(metrics_df_daily_corn, metrics_nee)
metrics_df_daily_corn

# soybeans calibration
predicted <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("19/20", "20/21"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily_soy <- rbind(metrics_df_daily_soy, metrics_nee)
metrics_df_daily_soy

# statistics by week

weekly_co2 <- merged_data_co2 %>%
  mutate(week = week(date)) %>%
  group_by(week, crop_year) %>%
  summarise(
    weekly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    weekly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    weekly_mod_gpp = sum(mod_gpp),
    weekly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    weekly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    weekly_mod_reco = sum(mod_resp),
    weekly_obs_nee = sum(nee_g_c_m2_day),
    weekly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

metrics_df_weekly <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# weekly daytime gpp

#overall

# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
# metrics_df_weekly

# corn years calibration
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years calibration
predicted <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly nighttime gpp

#overall

# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years calibration
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years calibration
predicted <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly reco

# weekly daytime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years calibration
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years calibration
predicted <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# weekly nighttime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years calibration
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years calibration
predicted <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# nee

# all years

# predicted <- weekly_co2$weekly_mod_nee
# observed <- weekly_co2$weekly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
# metrics_df_weekly

# corn years calibration
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# soybean years calibration
predicted <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("19/20", "20/21"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# statistics by year

yearly_co2 <- merged_data_co2 %>%
  group_by(crop_year) %>%
  summarise(
    yearly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    yearly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    yearly_mod_gpp = sum(mod_gpp),
    yearly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    yearly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    yearly_mod_reco = sum(mod_resp),
    yearly_obs_nee = sum(nee_g_c_m2_day),
    yearly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

metrics_df_yearly_corn <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

metrics_df_yearly_soy <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# daytime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years cakibration
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn <- rbind(metrics_df_yearly_corn, metrics_gpp)
metrics_df_yearly_corn

# soybean years calibration
predicted <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_soy <- rbind(metrics_df_yearly_soy, metrics_gpp)
metrics_df_yearly_soy

# nightime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_nt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years calibration
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn <- rbind(metrics_df_yearly_corn, metrics_gpp)
metrics_df_yearly_corn

# soybean years calibration
predicted <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_soy <- rbind(metrics_df_yearly_soy, metrics_gpp)
metrics_df_yearly_soy

# reco

# daytime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years calibration
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn <- rbind(metrics_df_yearly_corn, metrics_reco)
metrics_df_yearly_corn

# soybean years calibration
predicted <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_soy <- rbind(metrics_df_yearly_soy, metrics_reco)
metrics_df_yearly_soy

# nightime reco

#overall 
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years calibration
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn <- rbind(metrics_df_yearly_corn, metrics_reco)
metrics_df_yearly_corn

# soybean years calibration
predicted <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_soy <- rbind(metrics_df_yearly_soy, metrics_reco)
metrics_df_yearly_soy

# nee

# overall
# predicted <- yearly_co2$yearly_mod_nee
# observed <- yearly_co2$yearly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
# metrics_df_yearly

# by crop

# corn years calibration
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn <- rbind(metrics_df_yearly_corn, metrics_nee)
metrics_df_yearly_corn

# soybean years calibration
predicted <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("19/20", "20/21"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly_soy <- rbind(metrics_df_yearly_soy, metrics_nee)
metrics_df_yearly_soy


#### statistics CO2 - validation ####

merged_data_co2 <- mod_co2_p1 %>% 
  left_join(obs_co2_p1, by = c("year", "doy")) %>%
  mutate(crop_year = crop_year.x) %>% 
  filter(!is.na(crop_year))

metrics_df_daily <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# gpp 

# daytime

# all years

# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_dt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# by crop

# daytime

# corn years validation
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# soybean years validation
predicted <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# nighttime

# all years

# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_nt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# corn years validation
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# soybean years validation
predicted <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# respiration

# daytime

# all years 

# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_dt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn validation
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# soybeans validation
predicted <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# nightime

# all years

# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_nt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn validation
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# soybeans validation
predicted <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# nee

# all years

# predicted <- merged_data_co2$mod_nee
# observed <- merged_data_co2$nee_g_c_m2_day
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
# metrics_df_daily

# corn validation
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
metrics_df_daily

# soybeans validation
predicted <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("22/23", "23/24"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
metrics_df_daily

# statistics by week

weekly_co2 <- merged_data_co2 %>%
  mutate(week = week(date)) %>%
  group_by(week, crop_year) %>%
  summarise(
    weekly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    weekly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    weekly_mod_gpp = sum(mod_gpp),
    weekly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    weekly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    weekly_mod_reco = sum(mod_resp),
    weekly_obs_nee = sum(nee_g_c_m2_day),
    weekly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

metrics_df_weekly <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# weekly daytime gpp

#overall

# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
# metrics_df_weekly

# corn years validation
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years validation
predicted <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly nighttime gpp

#overall

# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly 

# corn years validation
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years validation
predicted <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly reco

# weekly daytime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly 

# corn years validation
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years validation
predicted <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# weekly nighttime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly 

# corn years validation
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years validation
predicted <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# nee

# all years

# predicted <- weekly_co2$weekly_mod_nee
# observed <- weekly_co2$weekly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
# metrics_df_weekly

# corn years validation
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# soybean years validation
predicted <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("22/23", "23/24"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# statistics by year

yearly_co2 <- merged_data_co2 %>%
  group_by(crop_year) %>%
  summarise(
    yearly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    yearly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    yearly_mod_gpp = sum(mod_gpp),
    yearly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    yearly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    yearly_mod_reco = sum(mod_resp),
    yearly_obs_nee = sum(nee_g_c_m2_day),
    yearly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

metrics_df_yearly <- data.frame(
  one = numeric(0),
  two = numeric(0),
  three = numeric(0),
  four = numeric(0),
  five = numeric(0),
  six = numeric(0),
  seven = numeric(0)
)

# daytime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years cakibration
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# soybean years validation
predicted <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# nightime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_nt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years validation
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# soybean years validation
predicted <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# reco

# daytime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years validation
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# soybean years validation
predicted <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# nightime reco

#overall 
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years validation
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# soybean years validation
predicted <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# nee

# overall
# predicted <- yearly_co2$yearly_mod_nee
# observed <- yearly_co2$yearly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
# metrics_df_yearly

# by crop

# corn years validation
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
metrics_df_yearly

# soybean years validation
predicted <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("22/23", "23/24"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
metrics_df_yearly

#### graphs ####

# general items for all graphs

# Define the wrapped crop DOY ticks you want on the x-axis
custom_breaks <- c(1, 51, 101, 151, 201, 251, 301, 351)

# Corresponding labels in original DOY terms:
# 1  = 121 (start of crop year)
# 51 = 171
# 101 = 221
# 151 = 271
# 201 = 21  (wrapped back to start of year)
# 245 = 120 (end of crop year)

custom_labels <- c("121", "171", "221", "271", "321", "6", "56", "106")

custom_labels_facets <- c(
  "18/19" = "Year 1 (18-19 | Corn)",
  "19/20" = "Year 2 (19-20 | Soybean)",
  "20/21" = "Year 3 (20-21 | Soybean)",
  "21/22" = "Year 4 (21-22 | Corn)",
  "22/23" = "Year 5 (22-23 | Soybean)",
  "23/24" = "Year 6 (23-24 | Soybean)"
)

# Prepare labels data for management dates + DOY 365 line
mgmt_labels <- mgmt_dates_p1 %>%
  mutate(
    label = paste0("DOY = ", doy),
    y_pos = min(c(mod_co2_p1$mod_nee, obs_co2_p1$nee_g_c_m2_day), na.rm = TRUE) # slightly above max y
  )

# Add a row for the DOY 365 line label
doy365_label <- data.frame(
  doy_crop = 245,  # crop day corresponding to DOY 365 after wrapping
  label = "DOY = 365",
  y_pos = min(mgmt_labels$y_pos)
)

# daily values

# respiration and GPP (displaying either NT or DT-partitioned GPP, in blue and orange, respectively)

ggplot() +
  
  geom_hline(yintercept = 0, color = "black")+
  
  # gpp
  geom_line(
    data = mod_co2_p1,
    aes(x = doy_crop, y = mod_gpp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # reco
  geom_line(
    data = mod_co2_p1,
    aes(x = doy_crop, y = mod_resp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # geom_line(data =  filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#0072B2", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = doy_crop, ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt, ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt), fill = "#0072B2", alpha = 0.3) +
  
  # reco
  
  #  geom_line(data =  filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#D55E00", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt, ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt), fill = "#D55E00", alpha = 0.3) +
  
  
  geom_line(data =  filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_dt), color = "#D55E00", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = reco_mean_g_c_m2_day_dt  - reco_sd_ustar_g_c_dt, ymax = reco_mean_g_c_m2_day_dt  + reco_sd_ustar_g_c_dt), fill = "#D55E00", alpha = 0.3) +
  # 
  # gpp
  #  geom_line(data =  filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_nt), color = "#009E73", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = -gpp_mean_g_c_m2_day_nt  - gpp_sd_ustar_g_c_nt, ymax = -gpp_mean_g_c_m2_day_nt  + gpp_sd_ustar_g_c_nt), fill = "#009E73", alpha = 0.3) +
  # 
  
  geom_line(data =  filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_dt), color = "#009E73", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = -gpp_mean_g_c_m2_day_dt  - gpp_sd_ustar_g_c_dt, ymax = -gpp_mean_g_c_m2_day_dt  + gpp_sd_ustar_g_c_dt), fill = "#009E73", alpha = 0.3) +
  
  geom_vline(
    data = mgmt_dates_p1,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6
  ) +
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown"
  ) +
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "black"
  ) +
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  ylab(expression(CO[2]~fluxes~(g~C~m^{-2}~day^{-1})))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )+
  scale_y_continuous(limits = c(-35, 30), breaks = seq(-30, 30, by = 10))

ggsave(filename = "modeled_reco_gpp_conv.png",  width = 8, height = 5, dpi = 300)

#ggsave(filename = "modeled_reco_gpp_conv.png",  width = 4, height = 10, dpi = 300)

# nee

ggplot() +
  geom_hline(yintercept = 0, color = "black")+
  geom_line(
    data = mod_co2_p1,
    aes(x = doy_crop, y = mod_nee),
    size = 0.6, color = "black", alpha = 0.6
  ) +

  geom_ribbon(data = filter(obs_co2_p1, !is.na(crop_year)), aes(x = doy_crop, ymin = nee_g_c_m2_day  - sd_comb_g_c, ymax = nee_g_c_m2_day  + sd_comb_g_c), fill = "#0072B2", alpha = 0.3) +

  geom_line(
    data = obs_co2_p1,
    aes(x = doy_crop, y = nee_g_c_m2_day),
    color = "#0072B2", size = 0.6, alpha = 0.6
  ) +
  geom_vline(
    data = mgmt_dates_p1,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6
  ) +
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = y_pos, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown"
  ) +
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = y_pos, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "black"
  ) +
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  ylab(expression(NEE~(g~C~m^{-2}~day^{-1})))+
theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )+
  scale_y_continuous(limits = c(-20, 10), breaks = seq(-20, 10, by = 5))


ggsave(filename = "modeled_nee_conv.png",  width = 8, height = 5, dpi = 300)

# annual

# Load the data
obs_co2_p1_annual<- read_excel("obs_data/measured_co2/mean_sdAnnual_gC_all_conv.xlsx")

# Process observed data (Nighttime, Daytime, NEE)
obs_long <- obs_co2_p1_annual %>%
  select(crop_year,
         mean_GPP_U50_f, mean_GPP_DT_U50,
         mean_Reco_U50_f, mean_Reco_DT_U50,
         mean_NEE_U50_f) %>%
  mutate(
    mean_GPP_U50_f = mean_GPP_U50_f * -1,
    mean_GPP_DT_U50 = mean_GPP_DT_U50 * -1
  ) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "var_method",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      grepl("GPP", var_method) ~ "GPP",
      grepl("Reco", var_method) ~ "Reco",
      grepl("NEE", var_method) ~ "NEE"
    ),
    method = case_when(
      grepl("DT", var_method) ~ "Daytime",
      grepl("NEE", var_method) ~ "Nighttime",  # treat NEE as Nighttime
      TRUE ~ "Nighttime"
    )
  ) %>%
  select(crop_year, variable, method, value)

# Process SD data
sd_long <- obs_co2_p1_annual %>%
  select(crop_year,
         sd_GPP_Ustar_NT, sd_GPP_uStar_DT,
         sd_Reco_Ustar_NT, sd_Reco_Ustar_DT,
         sdComb) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "var_method",
    values_to = "sd"
  ) %>%
  mutate(
    variable = case_when(
      grepl("GPP", var_method) ~ "GPP",
      grepl("Reco", var_method) ~ "Reco",
      grepl("sdComb", var_method) ~ "NEE"
    ),
    method = case_when(
      grepl("DT", var_method) ~ "Daytime",
      grepl("sdComb", var_method) ~ "Nighttime",
      TRUE ~ "Nighttime"
    )
  ) %>%
  select(crop_year, variable, method, sd)

# Merge obs + SD and define aesthetics
obs_plot_df <- obs_long %>%
  left_join(sd_long, by = c("crop_year", "variable", "method")) %>%
  mutate(
    source = method,
    fill_color = case_when(
      variable == "GPP" & method == "Nighttime" ~ "#0072B2",
      variable == "GPP" & method == "Daytime"   ~ "#E69F00",
      variable == "Reco" & method == "Nighttime" ~ "#0072B2",
      variable == "Reco" & method == "Daytime"   ~ "#E69F00",
      variable == "NEE" ~ "#CC79A7",  # Special color for observed NEE
      TRUE ~ "#999999"  # Default or DNDC fallback
    ),
    pattern = case_when(
      variable == "Reco" ~ "stripe",
      variable == "NEE" ~ "circle",
      TRUE ~ "none"
    ),
    label_color = if_else(variable == "GPP", "white", "black"),
    label_vjust = case_when(
      variable == "GPP" ~ 0.5,
      variable == "Reco" ~ 0.4,
      variable == "NEE" ~ 0.5
    )
  )

# Process DNDC data
dndc_plot_df <- mod_co2_p1 %>%
  filter(!(Year == 2018 & doy < 121), !(Year == 2024 & doy > 120)) %>%
  group_by(crop_year) %>%
  summarise(
    NEE = sum(mod_nee, na.rm = TRUE),
    GPP = sum(mod_gpp, na.rm = TRUE),
    Reco = sum(mod_resp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(NEE, GPP, Reco), names_to = "variable", values_to = "value") %>%
  mutate(
    method = "DNDC",
    sd = NA,
    source = "DNDC",
    fill_color = "#999999",
    pattern = case_when(
      variable == "Reco" ~ "stripe",
      variable == "NEE" ~ "circle",
      TRUE ~ "none"
    ),
    label_color = if_else(variable == "GPP", "white", "black"),
    label_vjust = case_when(
      variable == "GPP" ~ 0.5,
      variable == "Reco" ~ 0.4,
      variable == "NEE" ~ 0.5
    )
  )

# Combine all
plot_df <- bind_rows(obs_plot_df, dndc_plot_df) %>%
  mutate(
    ymin = value - sd,
    ymax = value + sd,
    variable = factor(variable, levels = c("GPP", "Reco", "NEE")),
    method = factor(method, levels = c("Nighttime", "Daytime", "DNDC", "NEE")),
    var_method = factor(paste(variable, source, sep = "_"), levels = c(
      "GPP_Nighttime", "GPP_Daytime", "GPP_DNDC",
      "Reco_Nighttime", "Reco_Daytime", "Reco_DNDC",
      "NEE_Nighttime", "NEE_DNDC"  # Fix this line
    ))
  )

# --- Final Plot ---


plot_df %>% 
  filter(grepl("Daytime|DNDC", var_method) | var_method == "NEE_Nighttime") %>%
  ggplot(aes(x = var_method, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.9),
    colour = "black",
    pattern_fill = "white",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.1
  ) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge2(width = 0.9),
    width = 0.9,
    color = "black",
    na.rm = TRUE
  ) +
  geom_text(
    aes(label = round(value, 0), vjust = label_vjust, colour = label_color, hjust = -0.5),
    position = position_dodge2(width = 0.9),
    angle = 90,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_pattern_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(-1800, 2000), breaks = seq(-1800, 2000, by = 200)) +
  facet_wrap(~ crop_year, nrow = 1, labeller = labeller(crop_year = custom_labels_facets)) +
  labs(
    #title = "Annual aggregated fluxes - Conventional rotation (P1+P2)",
    #subtitle = "Error bars = SD (NEE = random + u*-filtering uncertainty, GPP & Reco = u*-filtering uncertainty)",
    y = expression(CO[2] ~ "fluxes (g C m"^{-2}~"year"^{-1}~")")
  ) +
  scale_x_discrete(labels = c(
    "GPP_Nighttime" = "GPP NT",
    "GPP_Daytime" = "GPP DT",
    "GPP_DNDC" = "GPP DNDC",
    "Reco_Nighttime" = "Reco NT",
    "Reco_Daytime" = "Reco DT",
    "Reco_DNDC" = "Reco DNDC",
    "NEE_Nighttime" = "NEE",
    "NEE_DNDC" = "NEE DNDC"
  ))+
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "annual_conv.png",  width = 11.5, height = 5, dpi = 300)


###### N2O ######

# import modeled data for p1

mod_n2o_p1 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
colnames(mod_n2o_p1) <- mod_n2o_p1[3, ]
colnames(mod_n2o_p1) <- make.names(colnames(mod_n2o_p1), unique = TRUE)
mod_n2o_p1 <- mod_n2o_p1[-c(1,2, 3, 4),]
mod_n2o_p1 <- data.frame(lapply(mod_n2o_p1, as.numeric))

mod_n2o_p1$date_jul <- mod_n2o_p1$Day

mod_n2o_p1$n2o_flux_g_n_ha_d <- mod_n2o_p1$N2O.flux*1000

mod_n2o_p1 <- mod_n2o_p1 %>%
  #filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024,
  ))

mod_n2o_p1$date <- as.Date(paste(mod_n2o_p1$year, mod_n2o_p1$date_jul), format = "%Y %j")
mod_n2o_p1$doy <- mod_n2o_p1$Day

mod_n2o_p1 <- mod_n2o_p1 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

# graphs 
# obs_n2o_p1p2$doy = obs_n2o_p1p2$DOY
# obs_n2o_p1p2$year = obs_n2o_p1p2$Year
flux_grad_p1p2_2018_2024_n2o$year = flux_grad_p1p2_2018_2024_n2o$Year
flux_grad_p1p2_2018_2024_n2o$doy = flux_grad_p1p2_2018_2024_n2o$DoY

flux_grad_p1p2_2018_2024_n2o_daily <-  flux_grad_p1p2_2018_2024_n2o %>%
  filter(Year == 2024) %>% 
  group_by(year, doy, Plot) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  )

mod_n2o_p1 %>%
  filter(year %in% c(2018:2023)) %>% 
  #filter(year %in% c(2018:2023)) %>% 
  ggplot(aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_d )) +
  geom_line(size = 1, color = "black", alpha = 0.6) +
  geom_line(data = filter(obs_n2o_p1p2_mean_sd, year %in% c(2018:2023)), aes(x = doy, y = n2o_flux_g_n_ha_day_mean), color = "#0072B2", size = 1, alpha = 0.6) +
  geom_ribbon(data = filter(obs_n2o_p1p2_mean_sd, year %in% c(2018:2023)),aes(x = doy, ymin = n2o_flux_g_n_ha_day_mean - n2o_flux_g_n_ha_day_sd, ymax = n2o_flux_g_n_ha_day_mean + n2o_flux_g_n_ha_day_sd),
               alpha = 0.2, fill = "#0072B2", inherit.aes = FALSE) +
  
  #  geom_line(data = filter(flux_grad_p1p2_2018_2024_n2o_daily, year == 2024 & Plot == 2 & doy %in% 150:225), aes(x = doy, y = N2O_flux), color = "#0072B2", size = 1, alpha = 0.65) +
  # 
  # geom_line(data = filter(flux_grad_p1p2_2018_2024_n2o_daily, year == 2024 & Plot == 1 & doy %in% 150:225), aes(x = doy, y = N2O_flux*-1), color = "darkred", size = 1, alpha = 0.5) +
  
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw()+

  #scale_x_continuous(limits = c(0, 365), breaks = seq(0, 225, by = 25))
 geom_bar(data = filter(climate, year %in% c(2018:2023)) , aes(x = doy, y = prec*10), # 15 is an scaling factor
            stat = "identity", fill = "red", alpha = 1, position = "identity", width = 1) +
  # geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "darkred", linetype = "dashed", size = 1)+
  # geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  # geom_point(data = filter(combined_data_water_p12, year %in% c(2018:2023)) , aes(x = doy, y = vwc_5 * 100), size = 1, color = "blue", alpha = 0.5)+
  #  geom_point(data =filter(combined_data_water_p12, year %in% c(2018:2023)) , aes(x = doy, y = vwc_5_mean * 100), size = 1, color = "#D55E00", alpha = 0.5)+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("N2O emissions (g N/ha/day)")

ggsave(filename = "modeled_n2o_conv.png",  width = 12, height = 7, dpi = 300)

# statistics 
merged_data_n2o <- merge(mod_n2o_p1, obs_n2o_p1p2_mean_sd, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs")) %>% 
  filter(year%in%2018:2023)

predicted <- merged_data_n2o$n2o_flux_g_n_ha_d
observed <- merged_data_n2o$n2o_flux_g_n_ha_day_mean
metrics_n2o <- calculate_metrics(observed, predicted)

# metrics_n2o_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

metrics_n2o_df <- rbind(metrics_n2o_df, metrics_n2o)
metrics_n2o_df

# graph aggregated emissions

merged_data <- merge(mod_n2o_p1, obs_n2o_p1p2_mean_sd, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs"))%>% 
  filter(year%in%2018:2023)

# Filter rows with no NA in either variable
filtered_data <- merged_data %>%
  filter(!is.na(n2o_flux_g_n_ha_d) & !is.na(n2o_flux_g_n_ha_day_mean)) %>% 
  select(year, doy, n2o_flux_g_n_ha_d, n2o_flux_g_n_ha_day_mean)

# Summarize by year and type (mod/obs)
summary_data <- filtered_data %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarise(
    mod_sum = sum(n2o_flux_g_n_ha_d),
    obs_sum = sum(n2o_flux_g_n_ha_day_mean),
    n = n(),  # number of valid observations
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mod_sum, obs_sum),
               names_to = "source",
               values_to = "sum") %>%
  mutate(source = ifelse(source == "mod_sum", "Modeled", "Observed"))

# Plot
summary_data %>% 
  filter(!year == 2024) %>% 
  ggplot(aes(x = factor(year), y = sum, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(sum, 1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  geom_text(data = summary_data %>% filter(!year == 2024) %>%  distinct(year, n),
            aes(x = factor(year), y = 7000,
                label = paste("n =", n)),
            inherit.aes = FALSE, size = 3.5) +
  scale_fill_manual(values = c("Observed" = "#0072B2", "Modeled" = "#999999")) +
  labs(x = "Year", y = "Total N2O emissions (g N/ha)", fill = "Source",
       title = "Annual aggregated N2O emissions for days with observed data",
       subtitle = "n = number of daily observations considered") +
  theme_bw()

ggsave(filename = "modeled_n2o_conv_annual.png",  width = 8, height = 7, dpi = 300)

predicted <- summary_data %>% 
  filter(source=="Modeled") %>% 
  select(sum)
observed <- summary_data %>% 
  filter(source=="Observed")%>% 
  select(sum)

metrics_n2o_cum <- calculate_metrics(observed, predicted)
metrics_n2o_cum


###### NECB ######

# estimate C removed, and uncertainty propagation

# This script calculates the propagated uncertainty in the estimation of C_removed = yield × C_content
# using the standard error propagation formula for the product of two variables.

# Relative uncertainty: σ_Cremoved / Cremoved = sqrt( (σ_yield / yield)^2 + (σ_C / C)^2 )

obs_yield_mean_sd_with_uncertainty <- obs_yield_mean_sd %>%
  # Calculate the mean C_removed using yield × C_concentration
  mutate(
    crop_c_removed_derived_mean = crop_yield_obs_kg_ha_mean * grain_c_removed_obs_frac_mean
  ) %>%
  # Propagate uncertainty: C = A × B → σ_C = C × sqrt( (σ_A/A)^2 + (σ_B/B)^2 )
  mutate(
    crop_c_removed_derived_sd = crop_c_removed_derived_mean *
      sqrt(
        (crop_yield_obs_kg_ha_sd / crop_yield_obs_kg_ha_mean)^2 +
          (grain_c_removed_obs_frac_sd  / grain_c_removed_obs_frac_mean)^2
      )
  ) %>% 
  mutate(
    crop_c_removed_derived_mean_gc_m2 = crop_c_removed_derived_mean * 0.1,
    crop_c_removed_derived_sd_gc_m2 = crop_c_removed_derived_sd * 0.1
  ) %>% 
  filter(year>2017 & group == "P1_P2" & crop_type %in% c("corn", "soybean", "winter wheat")) %>%
  select(year, crop_type, group, crop_c_removed_derived_mean_gc_m2, crop_c_removed_derived_sd_gc_m2) %>% 
  mutate(
    crop_year = case_when(
      year == 2018 ~ "18/19",
      year == 2019 ~ "19/20",
      year == 2020 ~ "20/21",
      year == 2021 ~ "21/22",
      year == 2022 ~ "22/23",
      year == 2023 ~ "23/24"
    )
  )

obs_yield_mean_sd_with_uncertainty

# load annual NEE and uncertainty

mean_sd_nee_annual_gc_m2_year <- read_excel("obs_data/measured_co2/mean_sdAnnual_gC_all_conv.xlsx") %>% 
  select(crop_year, mean_NEE_U50_f, sdComb) %>% 
  mutate(mean_nee_u50_f_gc_m2 = mean_NEE_U50_f,
         sdcomb_nee_gc_m2 = sdComb) %>% 
  select(crop_year, mean_nee_u50_f_gc_m2, sdcomb_nee_gc_m2)
mean_sd_nee_annual_gc_m2_year

# total 

necb_obs <- obs_yield_mean_sd_with_uncertainty %>% 
  left_join(mean_sd_nee_annual_gc_m2_year, by = "crop_year") %>% 
  mutate(
    necb_mean_g_c_m2_obs = crop_c_removed_derived_mean_gc_m2 + mean_nee_u50_f_gc_m2 ,
    necb_sd_g_c_m2_obs = sqrt(crop_c_removed_derived_sd_gc_m2^2 + sdcomb_nee_gc_m2^2)
  ) %>% 
  select(crop_year, crop_type, group, necb_mean_g_c_m2_obs, necb_sd_g_c_m2_obs)
necb_obs

# cumulative amounts across the 6 years

# I think what Shannon did for previous papers was propagate using variance addition

# nee

cumulative_NEE_mean <- sum(mean_sd_nee_annual_gc_m2_year$mean_nee_u50_f_gc_m2)
cumulative_NEE_sd <- sqrt(sum(mean_sd_nee_annual_gc_m2_year$sdcomb_nee_gc_m2^2))
cumulative_NEE_mean
cumulative_NEE_sd

# crop c

cumulative_crop_c_mean <- sum(obs_yield_mean_sd_with_uncertainty$crop_c_removed_derived_mean_gc_m2)
cumulative_crop_c_sd <- sqrt(sum(obs_yield_mean_sd_with_uncertainty$crop_c_removed_derived_sd_gc_m2^2))
cumulative_crop_c_mean
cumulative_crop_c_sd

# necb_obs

cumulative_crop_c_mean <- sum(necb_obs$necb_mean_g_c_m2_obs)
cumulative_crop_c_sd <- sqrt(sum(necb_obs$necb_sd_g_c_m2_obs^2))
cumulative_crop_c_mean
cumulative_crop_c_sd

# modeled values

# c removed with harvest

mod_yield_p1$grain_c_kg_ha_mod

c_removed_grain_mod <- mod_yield_p1 %>%
  filter(year > 2017 & year < 2024) %>% 
  select(year, grain_c_kg_ha, crop_type) %>% 
  mutate(
    grain_c_removed_gc_m2_mod = grain_c_kg_ha*0.1,
    crop_year = case_when(
      year == 2018 ~ "18/19",
      year == 2019 ~ "19/20",
      year == 2020 ~ "20/21",
      year == 2021 ~ "21/22",
      year == 2022 ~ "22/23",
      year == 2023 ~ "23/24"
    )
  )

# nee

annual_nee_mod <- mod_co2_p1 %>%
  filter(!is.na(crop_year)) %>% 
  group_by(crop_year) %>%
  summarise(
    method = "DNDC",
    nee_gc_m2_year_mod = sum(mod_nee, na.rm = TRUE),
    # GPP = sum(mod_gpp, na.rm = TRUE),
    # Reco = sum(mod_resp, na.rm = TRUE),
    .groups = "drop"
  ) 

necb_mod <- annual_nee_mod %>% 
  left_join(c_removed_grain_mod, by = "crop_year") %>% 
  mutate(
    necb_mean_g_c_m2_mod = nee_gc_m2_year_mod + grain_c_removed_gc_m2_mod ,
  ) %>% 
  select(crop_year, crop_type, necb_mean_g_c_m2_mod)
necb_mod


# comparing NECB modeled and observed

# # Reshape to long format
# necb_long <- necb_obs %>% 
#   left_join(necb_mod, by = "crop_year") %>%
#   select(crop_year, 
#          necb_mean_g_c_m2_obs, necb_sd_g_c_m2_obs, 
#          necb_mean_g_c_m2_mod) %>%
#   rename(
#     obs_mean = necb_mean_g_c_m2_obs,
#     obs_sd = necb_sd_g_c_m2_obs,
#     mod_mean = necb_mean_g_c_m2_mod
#   ) %>%
#   pivot_longer(cols = c(obs_mean, mod_mean), 
#                names_to = "source", 
#                values_to = "necb_mean") %>%
#   mutate(
#     necb_sd = ifelse(source == "obs_mean", obs_sd, NA_real_),
#     source = recode(source, "obs_mean" = "Observed", "mod_mean" = "Modeled")
#   )
# 
# # Plot
# ggplot(necb_long, aes(x = source, y = necb_mean, fill = source)) +
#   geom_col(position = position_dodge(), width = 0.6, color = "black") +
#   geom_errorbar(
#     data = filter(necb_long, !is.na(necb_sd)),
#     aes(ymin = necb_mean - necb_sd, ymax = necb_mean + necb_sd),
#     width = 0.2,
#     position = position_dodge(0.6)
#   ) +
#   facet_wrap(~ crop_year, nrow=1) +
#   labs(
#     x = "Source",
#     y = expression(NECB~(g~C~m^{-2})),
#     fill = "Data Source"
#   ) +
#   geom_text(
#     aes(label = round(necb_mean, 0)),
#     vjust = -0.5,
#     position = position_dodge(0.6),
#     size = 3
#   ) +
#   theme_bw() +
#   theme(legend.position = "bottom")

# Combine observed components
obs_components <- obs_yield_mean_sd_with_uncertainty %>%
  rename(grain_c_mean = crop_c_removed_derived_mean_gc_m2,
         grain_c_sd = crop_c_removed_derived_sd_gc_m2) %>%
  left_join(mean_sd_nee_annual_gc_m2_year, by = "crop_year") %>%
  rename(nee_mean = mean_nee_u50_f_gc_m2,
         nee_sd = sdcomb_nee_gc_m2) %>%
  left_join(necb_obs, by = c("crop_year", "crop_type")) %>%
  rename(necb_mean = necb_mean_g_c_m2_obs,
         necb_sd = necb_sd_g_c_m2_obs) %>%
  select(crop_year, crop_type, grain_c_mean, grain_c_sd,
         nee_mean, nee_sd, necb_mean, necb_sd) %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "component", values_to = "value") %>%
  mutate(
    sd = case_when(
      component == "grain_c_mean" ~ grain_c_sd,
      component == "nee_mean" ~ nee_sd,
      component == "necb_mean" ~ necb_sd,
      TRUE ~ NA_real_
    ),
    component = recode(component,
                       "grain_c_mean" = "Grain C",
                       "nee_mean" = "NEE",
                       "necb_mean" = "NECB"
    ),
    source = "Obs"
  )



# Combine modeled components
mod_components <- c_removed_grain_mod %>%
  rename(grain_c = grain_c_removed_gc_m2_mod) %>%
  left_join(annual_nee_mod, by = "crop_year") %>%
  rename(nee = nee_gc_m2_year_mod) %>%
  left_join(necb_mod, by = c("crop_year", "crop_type")) %>%
  rename(necb = necb_mean_g_c_m2_mod) %>%
  select(crop_year, crop_type, grain_c, nee, necb) %>%
  pivot_longer(cols = c(grain_c, nee, necb), names_to = "component", values_to = "value") %>%
  mutate(
    sd = NA_real_,
    component = recode(component,
                       "grain_c" = "Grain C",
                       "nee" = "NEE",
                       "necb" = "NECB"
    ),
    source = "Mod"
  )

# Combine all and assign styles
plot_df <- bind_rows(obs_components, mod_components) %>%
  mutate(
    component = factor(component, levels = c("Grain C", "NEE", "NECB")),
    source = factor(source, levels = c("Obs", "Mod")),
    var_source = paste(component, source, sep = "_"),
    
    # Color and pattern settings
    fill_color = case_when(
      source == "Obs" ~ "#1b9e77",
      source == "Mod"  ~ "gray",
    ),
    pattern = case_when(
      component == "Grain C" ~ "none",
      component == "NEE"             ~ "stripe",
      component == "NECB"            ~ "circle"
    ),
    label_color = "black",
    ymin = value - sd,
    ymax = value + sd
  )

plot_df <- plot_df %>%
  mutate(
    var_source = factor(
      var_source,
      levels = c("Grain C_Obs", "Grain C_Mod",
                 "NEE_Obs", "NEE_Mod",
                 "NECB_Obs", "NECB_Mod")
    )
  )

ggplot(plot_df, aes(x = var_source, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.9),
    colour = "black",
    pattern_fill = "white",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.6,
    width = 0.6
  ) +
  geom_errorbar(
    data = filter(plot_df, !is.na(sd)),
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge2(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = round(value, 0), color = label_color),
    position = position_dodge2(width = 0.9),
    vjust = -1.5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_pattern_identity() +
  scale_color_identity() +
  facet_wrap(~ crop_year, nrow = 1) +
  labs(
    y = expression(NECB~components~(g~C~m^{-2}~year^{-1})),
    x = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

ggsave(filename = "necb_conv.png", width = 10, height = 5, dpi = 300)


# statistics

metrics_df_annual_necb <- data.frame(
  n = numeric(0),
  Measured_Avg  = numeric(0),
  Modeled_Avg  = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# necb 

# all years
predicted <- necb_mod$necb_mean_g_c_m2_mod
observed <- necb_obs$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

# corn years
predicted <- filter(necb_mod, crop_year %in% c("18/19", "21/22"))$necb_mean_g_c_m2_mod
observed <- filter(necb_obs, crop_year %in% c("18/19", "21/22"))$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

# soybean years
predicted <- filter(necb_mod, crop_year %in% c("19/20", "20/21", "22/23", "23/24"))$necb_mean_g_c_m2_mod
observed <- filter(necb_obs, crop_year %in% c("19/20", "20/21", "22/23", "23/24"))$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics


###################################

###################################
####### DIVERSE ROTATION ##########
###################################

# loading data from DNDC ouputs (stored in C drive folder) related to the diverse rotation in my project, running statistics, and creating graphs (all by variable). 

###### CROP YIELD #######

### import observed data crop yield specifically for p3 as a few things are different

obs_yield_p34 <- read_excel("obs_data/measured_crop/obs_yield_e26_2025_02_06.xlsx", sheet = "obs_yield_cc")
obs_yield_p34$type <- "obs"
obs_yield_p34$grain_t_ha = as.numeric(obs_yield_p34$grain_t_ha)
obs_yield_p34$crop_yield_obs_kg_ha <- obs_yield_p34$grain_t_ha*1000
#obs_yield_p3 <- filter(obs_yield, plot == "3")

obs_yield_p34_mean_sd <- obs_yield_p34 %>%
  filter(year>2017) %>% 
  mutate(
    group = case_when(
      plot %in% c(1, 2) ~ "P1_P2",
      plot %in% c(3, 4) ~ "P3_P4",
      TRUE ~ NA_character_
    )) %>% 
  filter(!group == "P1_P2")%>% 
  group_by(year, crop_order, crop_type, group) %>%
  summarise(
    crop_yield_obs_kg_ha_mean = mean(crop_yield_obs_kg_ha, na.rm = TRUE),
    crop_yield_obs_kg_ha_sd = sd(crop_yield_obs_kg_ha , na.rm = TRUE),
    .groups = "drop"
  )

### import modeled data for p3

mod_yield <- read.csv("C:/DNDC/Result/Record/Site/Multi_year_summary.csv", header = FALSE)
mod_yield <- mod_yield[, c(1:21)]
colnames(mod_yield) <- mod_yield[2,]
mod_yield <- mod_yield[-c(1:3),]
# Clean column names to remove any leading/trailing whitespace
colnames(mod_yield) <- trimws(colnames(mod_yield))

# Trim whitespace and convert columns to appropriate types
mod_yield <- mod_yield %>%
  mutate(across(everything(), ~ trimws(.))) %>%  # Trim whitespace from all columns
  mutate(
    Year = as.numeric(Year),  # Convert Year to numeric
    Crop1 = as.numeric(Crop1),
    GrainC1 = as.numeric(GrainC1),
    LeafC1 = as.numeric(LeafC1),
    StemC1 = as.numeric(StemC1),
    RootC1 = as.numeric(RootC1),
    Crop2 = as.numeric(Crop2),
    GrainC2 = as.numeric(GrainC2),
    LeafC2 = as.numeric(LeafC2),
    StemC2 = as.numeric(StemC2),
    RootC2 = as.numeric(RootC2),
    Crop3 = as.numeric(Crop3),
    GrainC3 = as.numeric(GrainC3),
    LeafC3 = as.numeric(LeafC3),
    StemC3 = as.numeric(StemC3),
    RootC3 = as.numeric(RootC3),
    Crop4 = as.numeric(Crop4),
    GrainC4 = as.numeric(GrainC4),
    LeafC4 = as.numeric(LeafC4),
    StemC4 = as.numeric(StemC4),
    RootC4 = as.numeric(RootC4)
  )

crop_types <- mod_yield %>%
  select(Year, Crop1, Crop2, Crop3, Crop4) %>%
  pivot_longer(
    cols = starts_with("Crop"),
    names_to = "crop",
    values_to = "crop_type"
  ) %>%
  mutate(crop_order = as.numeric(gsub("Crop", "", crop))) %>%
  select(Year, crop_order, crop_type)

# Reshape the data
long_df <- mod_yield %>%
  pivot_longer(
    cols = c(GrainC1, LeafC1, StemC1, RootC1, GrainC2, LeafC2, StemC2, RootC2, GrainC3, LeafC3, StemC3, RootC3, GrainC4, LeafC4, StemC4, RootC4),
    names_to = c(".value", "crop_type"),
    names_pattern = "(.*)(C[1-4])"
  ) %>%
  mutate(crop_order = as.numeric(gsub("C", "", crop_type))) %>%
  select(Year, crop_order, grain_c = Grain, leaf_c = Leaf, stem_c = Stem, root_c = Root) %>%
  arrange(Year, crop_order)

# Combine with crop types
mod_yield_p3 <- left_join(long_df, crop_types, by = c("Year", "crop_order")) %>%
  replace_na(list(grain_c = 0, leaf_c = 0, stem_c = 0, root_c = 0)) %>%
  #filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
    
    
  )) %>% 
  mutate(crop_type = case_when(
    crop_type == 90 ~ "corn",
    crop_type == 2 ~ "winter wheat",
    crop_type == 3 ~ "soybean",
    crop_type == 15  ~ "rye",
    crop_type == 47  ~ "cover crop",
    crop_type == 10  ~ "clover"
  ))
mod_yield_p3$type <- "mod"
mod_yield_p3 <- mod_yield_p3[order(mod_yield_p3$year), ]
mod_yield_p3$crop_yield_mod_kg_ha <- ifelse(mod_yield_p3$crop_type == "corn", mod_yield_p3$grain_c /0.42, ifelse(mod_yield_p3$crop_type == "soybean", mod_yield_p3$grain_c /0.41, ifelse(mod_yield_p3$crop_type == "winter wheat", mod_yield_p3$grain_c /0.40, ifelse(mod_yield_p3$crop_type %in% c("cover crop", "rye", "clover"), c(mod_yield_p3$grain_c + mod_yield_p3$leaf_c + mod_yield_p3$stem_c)/0.41,mod_yield_p3$grain_c /0.41))))
mod_yield_p3$grain_c_kg_ha_mod <- mod_yield_p3$grain_c

# incorporating actual cover crop biomass from dates in which it was sampled in the field

# importing cover crop data for specific dates

crop_data <- read_csv("C:/DNDC/Result/Record/Site/Day_FieldCrop_1.csv", 
                      skip = 4,   # Adjust based on how many rows to skip
                      col_names = FALSE)
crop_data <- crop_data[, c(1, 2,14, 36:39,48, 70:73, 82, 104:107, 116, 138:141)] 
header_row1 <- c("year", "day", "crop1", "crop1_leaf_c", "crop1_stem_c", "crop1_root_c", 
                 "crop1_grain_c", "crop2", "crop2_leaf_c", "crop2_stem_c", "crop2_root_c", 
                 "crop2_grain_c", "crop3", "crop3_leaf_c", "crop3_stem_c", "crop3_root_c", "crop3_grain_c", "crop4", "crop4_leaf_c", "crop4_stem_c", "crop4_root_c", "crop4_grain_c")    
colnames(crop_data) <- header_row1

crop_data <- crop_data %>%  # day of the year when cover crops were sampled: 2018 (278), 2019 (139), 2020 (315), 2021 (130, 292), 
  filter((year == 11 & day == 278) | 
           (year == 12 & day == 139) | 
           (year == 13 & day == 315) | 
           (year == 14 & day %in% c(130, 292)| 
              (year == 15 & day == 120) | 
              (year == 16 & day == 314) |
              (year == 17 & day == 120)  
            
            
           )) %>% 
  mutate(
    crop1_biomass = (crop1_leaf_c + crop1_stem_c + crop1_grain_c)/0.41,
    crop2_biomass = (crop2_leaf_c + crop2_stem_c + crop2_grain_c)/0.41,
    crop3_biomass = (crop3_leaf_c + crop3_stem_c + crop3_grain_c)/0.41,
    crop4_biomass = (crop4_leaf_c + crop4_stem_c + crop4_grain_c)/0.41
    
  ) %>% 
  select(year, day, crop1, crop2, crop3, crop4, crop1_biomass, crop2_biomass, crop3_biomass, crop4_biomass)

long_data <- crop_data %>%
  pivot_longer(
    cols = starts_with("crop") & ends_with("biomass"),  # Select only biomass columns
    names_to = "biomass_type",  # New column for biomass type
    values_to = "biomass_value"  # Rename the values column
  ) %>%
  mutate(
    crop_order = as.integer(sub("crop(\\d)_biomass", "\\1", biomass_type))  # Correctly extract crop number
  ) %>%
  select(year, day, crop_order, biomass_value) %>% 
  filter((year == 11 & crop_order %in% c(2)) | (year == 12 & crop_order %in% c(1)) | (year == 13 & crop_order == 2) | (year == 14 & crop_order %in% c(1,3)) | (year == 15 & crop_order == 1) | (year == 16 & crop_order == 2) | (year == 17 & crop_order == 1)) %>% 
  filter(!biomass_value == 0)

mod_yield_p3 <- mod_yield_p3 %>%
  left_join(long_data, by = c("Year" = "year", "crop_order")) %>%
  mutate(
    crop_yield_mod_kg_ha = ifelse(!is.na(biomass_value), biomass_value, crop_yield_mod_kg_ha)
  ) %>%
  select(-biomass_value)

mod_yield_p3 <- mod_yield_p3 %>%
  mutate(across(everything(), ~ ifelse(. == "rye", "cover crop", .))) %>% 
  filter(year>2017)

# graph

combined_data <- merge(obs_yield_p34_mean_sd, mod_yield_p3, by = c("year", "crop_type",  "crop_order")) %>% 
  filter(year>2017)

# combined_data %>%
#   filter(year > 2017) %>% 
#   pivot_longer(
#     cols = c(crop_yield_obs_kg_ha_mean, crop_yield_mod_kg_ha), 
#     names_to = "yield_type", 
#     values_to = "yield_value"
#   ) %>%
#   mutate(
#     yield_category = ifelse(grepl("obs", yield_type), "obs", "mod"),
#     yield_sd = case_when(
#       yield_type == "crop_yield_obs_kg_ha_mean" ~ crop_yield_obs_kg_ha_sd,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   ggplot(aes(x = yield_category, y = yield_value, fill = yield_category)) +
#   geom_bar(stat = "identity", position = "dodge", colour = "black") +
#   geom_errorbar(
#     aes(ymin = yield_value - yield_sd, ymax = yield_value + yield_sd),
#     position = position_dodge(width = 0.9),
#     width = 0.3
#   ) +
#   scale_y_continuous(limits = c(0,13500))+
#   facet_grid(cols = vars(year), rows = vars(crop_order)) +
#   scale_fill_manual(values = c("obs" = "#009E73", "mod" = "#999999")) +
#   labs(x = "Data Type", y = "Crop Yield (kg/ha)") +
#   geom_text(aes(label = round(yield_value, 0)), 
#             position = position_dodge(width = 0.9), 
#             vjust = -1) +
#   theme_bw()

# graph 

main_crop_data %>%
  filter(year > 2017) %>% 
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha_mean, crop_yield_mod_kg_ha), 
    names_to = "yield_type", 
    values_to = "yield_value"
  ) %>%
  mutate(
    yield_category = ifelse(grepl("obs", yield_type), "O", "M"),
    yield_sd = case_when(
      yield_type == "crop_yield_obs_kg_ha_mean" ~ crop_yield_obs_kg_ha_sd,
      TRUE ~ NA_real_
    )
  ) %>%
  ggplot(aes(
    x = yield_category, 
    y = yield_value,
    pattern = yield_category,
    pattern_fill = yield_category,
    pattern_colour = yield_category,
    fill = yield_category
  )) +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.2,
    pattern_angle = 45,
    pattern_key_scale_factor = 2
  ) +
  geom_errorbar(
    aes(ymin = yield_value - yield_sd, ymax = yield_value + yield_sd),
    position = position_dodge(width = 0.9),
    width = 0.3
  ) +
  scale_y_continuous(
    limits = c(0, 13500),
    breaks = seq(0, 13500, by = 2500)
  ) +
  facet_wrap(~ crop_year, nrow = 1) +
  scale_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_manual(values = c("O" = "stripe", "M" = "none")) +
  scale_pattern_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_colour_manual(values = c("O" = "white", "M" = "#999999")) +
  labs(y = expression(Crop~Yield~(kg~ha^{-1}))) +
  geom_text(
    aes(label = round(yield_value, 0), angle = 90), 
    position = position_dodge(width = 0.9), 
    vjust = 0.5,
    hjust = -0.3
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "yield_div.png",  width = 4.5, height = 3.8, dpi = 300)

# cover crop graph

# Step 1: Prepare data
cover_crop_data <- combined_data %>%
  filter(crop_type == "cover crop") %>%
  mutate(
    crop_year = case_when(
      year %in% c(2018, 2019) & crop_order %in% 1:2 ~ "18/19",
      year %in% c(2020, 2021) & crop_order %in% 1:2 ~ "20/21",
      year == 2021 & crop_order == 3 ~ "21/22",
      year == 2022 ~ "21/22",
      year %in% c(2023, 2024) & crop_order %in% 1:2 ~ "23/24",
      TRUE ~ NA_character_
    ),
    season = case_when(
      crop_order == 1 ~ "Spring",
      crop_order %in% c(2,3) ~ "Fall",
      TRUE ~ "Other"
    ),
    season = factor(season, levels = c("Fall", "Spring"))
  )

# Step 2: Reshape and assign plotting groups
plot_data <- cover_crop_data %>%
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha_mean, crop_yield_mod_kg_ha),
    names_to = "yield_type",
    values_to = "yield_value"
  ) %>%
  mutate(
    yield_category = ifelse(grepl("obs", yield_type), "O", "M"),
    yield_sd = case_when(
      yield_type == "crop_yield_obs_kg_ha_mean" ~ crop_yield_obs_kg_ha_sd,
      TRUE ~ NA_real_
    )
  )

# Step 3: Plot
ggplot(plot_data, aes(x = season, y = yield_value,
                      pattern = yield_category,
                      pattern_fill = yield_category,
                      pattern_colour = yield_category,
                      fill = yield_category
                      
                      )) +
  #geom_col(position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    colour = "black",
    pattern_density = 0.5,
    pattern_spacing = 0.15,
    pattern_angle = 45,
    pattern_key_scale_factor = 2
  ) +
  scale_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_manual(values = c("O" = "stripe", "M" = "none")) +
  scale_pattern_fill_manual(values = c("O" = "#009E73", "M" = "#999999")) +
  scale_pattern_colour_manual(values = c("O" = "white", "M" = "#999999")) +
  
  geom_errorbar(aes(ymin = yield_value - yield_sd, ymax = yield_value + yield_sd),
                position = position_dodge(width = 0.8), width = 0.2, na.rm = TRUE) +
  geom_text(
    aes(label = round(yield_value, 0), angle = 90), 
    position = position_dodge(width = 0.9), 
    vjust = 0.4,
    hjust = -0.5
  ) +
  facet_wrap(~ crop_year, nrow = 1) +
  #scale_fill_manual(values = c("Obs" = "#009E73", "Mod" = "#999999")) +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  labs(x = NULL, y = expression("Cover crop biomass (kg DM"* ha^{-1} *")")) +
  theme_bw() +
  scale_x_discrete(expand = c(0.25, 0.25))+
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10)
  )

ggsave(filename = "cc_yield_div.png",  width = 4.3, height = 3.8, dpi = 300)



### statistics

## calibration and validation

# yield 

# corn

modeled <- filter(combined_data, crop_type == "corn")$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "corn")$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# soybean

modeled <- filter(combined_data, crop_type == "soybean")$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "soybean")$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# winter wheat

#calibration
modeled <- filter(combined_data, crop_type == "winter wheat" & year == 2020)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "winter wheat" & year == 2020)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# validation
modeled <- filter(combined_data, crop_type == "winter wheat" & year == 2023)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "winter wheat" & year == 2023)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# 2-way cc 

#calibration
modeled <- filter(combined_data, crop_type == "cover crop" & year %in% 2018:2019)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "cover crop" & year %in% 2018:2019)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# validation
modeled <- filter(combined_data, crop_type == "cover crop" & year %in% 2021 & crop_order == 3)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "cover crop" & year %in% 2021 & crop_order == 3)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield


# 4-way cc wheat

#calibration
modeled <- filter(combined_data, crop_type == "cover crop" & year %in% 2020:2021 & crop_order != 3)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "cover crop" & year %in% 2020:2021 & crop_order != 3)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

# validation
modeled <- filter(combined_data, crop_type == "cover crop" & year %in% 2023:2024 & crop_order != 3)$crop_yield_mod_kg_ha
observed <- filter(combined_data, crop_type == "cover crop" & year %in% 2023:2024 & crop_order != 3)$crop_yield_obs_kg_ha_mean
metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield

###### SOIL TEMPERATURE ###########
# import modeled data

mod_conv_soil_temp_p34 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_conv_soil_temp_p34 <- mod_conv_soil_temp_p34[, c(1:27)]
colnames(mod_conv_soil_temp_p34) <- mod_conv_soil_temp_p34[3,]
mod_conv_soil_temp_p34 <- mod_conv_soil_temp_p34[-c(1:3),]
mod_conv_soil_temp_p34 <- data.frame(lapply(mod_conv_soil_temp_p34, as.numeric))
mod_conv_soil_temp_p34$doy <- mod_conv_soil_temp_p34$Day
mod_conv_soil_temp_p34$temp_25 <- (mod_conv_soil_temp_p34$X20cm + mod_conv_soil_temp_p34$X30cm) / 2
mod_conv_soil_temp_p34$temp_55 <- (mod_conv_soil_temp_p34$X50cm + mod_conv_soil_temp_p34$X60cm) / 2
mod_conv_soil_temp_p34$temp_85 <- (mod_conv_soil_temp_p34$X80cm + mod_conv_soil_temp_p34$X90cm) / 2

mod_conv_soil_temp_p34 <- mod_conv_soil_temp_p34 %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
  ))

mod_conv_soil_temp_p34 <- mod_conv_soil_temp_p34 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

mod_conv_soil_temp_p34 <- mod_conv_soil_temp_p34 %>% 
  filter(!is.na(crop_year))

# graphs for plot 1
combined_data_temp_p34 <- merge(obs_soil_temp_p34 , mod_conv_soil_temp_p34, by = c("year", "doy")) %>% 
  filter(!is.na(crop_year))

# t5

combined_data_temp_p34 %>% 
  ggplot(aes(x = doy, y = X5cm))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_5_mean), 
            color = "#D55E00", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_5_mean - temp_5_sd, ymax = temp_5_mean + temp_5_sd), 
              alpha = 0.2, fill = "#D55E00") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Soil temperature at 5 cm (C)")

# t25

combined_data_temp_p34 %>% 
  ggplot(aes(x = doy, y = temp_25))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_25_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_25_mean - temp_25_sd, ymax = temp_25_mean + temp_25_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Soil temperature at 25 cm (C)")

# t55

combined_data_temp_p34 %>% 
  ggplot(aes(x = doy, y = temp_55))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_55_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_55_mean - temp_55_sd, ymax = temp_55_mean + temp_55_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 55 cm (C)")


# t85

combined_data_temp_p34 %>% 
  ggplot(aes(x = doy, y = temp_85))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(aes(x = doy, y = temp_85_mean), 
            color = "#009E73", size = 1, linetype = 1, alpha = 0.7)+
  geom_ribbon(aes(ymin = temp_85_mean - temp_85_sd, ymax = temp_85_mean + temp_85_sd), 
              alpha = 0.3, fill = "#009E73") +
  theme_bw()+
  facet_wrap(year~., ncol = 2)+
  ylab("Temperature at 85 cm (C)")


### statistics

# temperature 5 cm
observed <- combined_data_temp_p34$temp_5_mean
modeled <- combined_data_temp_p34$X5cm
calculate_metrics(observed, modeled)


# temperature 25 cm
observed <- combined_data_temp_p34$temp_25_mean
modeled <- combined_data_temp_p34$temp_25
calculate_metrics(observed, modeled)


###### SOIL WATER ###########

mod_soil_water_p34 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_soil_water_p34 <- mod_soil_water_p34[, c(1:5, 28:49)]
colnames(mod_soil_water_p34) <- mod_soil_water_p34[3,]
mod_soil_water_p34 <- mod_soil_water_p34[-c(1:3),]
mod_soil_water_p34 <- data.frame(lapply(mod_soil_water_p34, as.numeric))
mod_soil_water_p34$date_jul <- mod_soil_water_p34$Day
mod_soil_water_p34$wfps_25_mod <- (mod_soil_water_p34$X20cm + mod_soil_water_p34$X30cm) / 2
mod_soil_water_p34$vwc_5 <- mod_soil_water_p34$X5cm * (1.26/2.65)
mod_soil_water_p34$vwc_25 <- c((mod_soil_water_p34$X20cm + mod_soil_water_p34$X30cm)/2 * (1.30/2.65))
mod_soil_water_p34$vwc_55 <- c((mod_soil_water_p34$X50cm + mod_soil_water_p34$X60cm)/2 * (1.35/2.65))
mod_soil_water_p34$vwc_85 <- c((mod_soil_water_p34$X80cm + mod_soil_water_p34$X90cm)/2 * (1.40/2.65))
mod_soil_water_p34$doy <- mod_soil_water_p34$date_jul

mod_soil_water_p34 <- mod_soil_water_p34 %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
  ))

# graphs
combined_data_water_p34 <- merge(obs_soil_vwc_p34, mod_soil_water_p34, by = c("year", "doy"))

combined_data_water_p34 <- combined_data_water_p34 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

combined_data_water_p34 <- combined_data_water_p34 %>% 
  filter(!is.na(crop_year))

# vwc 5 cm

climate_filtered <- climate %>%
  filter(doy > 90 & doy < 335) %>%  # Filter for the range of days of the year (March 31st to Dec 1st)
  filter(year >= 2018 & year <= 2024)

combined_data_water_p34 %>% 
  filter(doy > 90 & doy < 335) %>%  # Filter for the desired doy range
  ggplot() +
  geom_line(aes(x = doy, y = vwc_5_mean), color = "#D55E00", size = 1, linetype = 1, alpha = 0.7) +
  geom_ribbon(aes(x = doy, ymin = vwc_5_mean - vwc_5_sd, ymax = vwc_5_mean + vwc_5_sd), 
              alpha = 0.3, fill = "#D55E00") +
  geom_line(aes(x = doy, y = vwc_5), size = 1, color = "darkgray", alpha = 0.7) +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw() +
  #geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2022)), aes(xintercept = DoY), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*0.05), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "VWC at 5 cm (cm3/cm3)", 
    limits = c(0, 0.6),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./0.05,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 12, 2)  # Set the secondary axis from 0 to 6 cm
    )
  )

# vwc 25 cm

combined_data_water_p34 %>% 
  filter(doy > 90 & doy < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot()+
  geom_line(aes(x = doy, y = vwc_25), size = 1, color = "gray")+
  geom_line(aes(x = doy, y = vwc_25_mean), 
            color = "#009E73", size = 1, linetype = 1)+
  geom_ribbon(aes(x = doy, ymin = vwc_25_mean - vwc_25_sd, ymax = vwc_25_mean + vwc_25_sd), 
              alpha = 0.3, fill = "#009E73") +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw()+
  ylab("VWC at 25 cm (cm3/cm3)")+
  #geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*0.1), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "VWC at 25 cm (cm3/cm3)", 
    limits = c(0, 0.6),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./0.1,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 6, 1)  # Set the secondary axis from 0 to 6 cm
    )
  )

### statistics

# vwc 5 cm

# metrics_vwc5_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_water_p34, Day > 90 & Day < 335)$vwc_5
modeled <- filter(combined_data_water_p34, Day > 90 & Day < 335)$vwc_5_mean
metrics_vwc5 <- calculate_metrics(observed, modeled)
metrics_vwc5

# metrics_vwc5_df <- rbind(metrics_vwc5_df, metrics_vwc5)
# metrics_vwc5_df


# vwc 25 cm

# metrics_vwc25_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_water_p34, Day > 90 & Day < 335)$vwc_25
modeled <- filter(combined_data_water_p34, Day > 90 & Day < 335)$vwc_25_mean
metrics_vwc25 <- calculate_metrics(observed, modeled)
metrics_vwc25

# metrics_vwc25_df <- rbind(metrics_vwc25_df, metrics_vwc25)
# metrics_vwc25_df

# # vwc 5 cm
# observed <- combined_data2$VWC_5
# modeled <- combined_data2$vwc_5/100
# calculate_metrics(observed, modeled)
# 
# observed <- filter(combined_data2, vwc_5!=0)$VWC_5
# modeled <- filter(combined_data2, vwc_5!=0)$vwc_5/100
# calculate_metrics(observed, modeled)
# 
# # vwc 25 cm
# observed <- combined_data2$VWC_25
# modeled <- combined_data2$vwc_25/100
# calculate_metrics(observed, modeled)
# 
# observed <- filter(combined_data2, vwc_25!=0)$VWC_25
# modeled <- filter(combined_data2, vwc_25!=0)$vwc_25/100
# calculate_metrics(observed, modeled)

###### SOIL NITROGEN ##############

# import modeled data for p1

mod_conv_soil_n <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
combined_names <- paste(mod_conv_soil_n[3, ], mod_conv_soil_n[4, ], sep = "_")
colnames(mod_conv_soil_n) <- combined_names

mod_conv_soil_n <- mod_conv_soil_n[-c(1:4),]
mod_conv_soil_n <- data.frame(lapply(mod_conv_soil_n, as.numeric))

mod_conv_soil_n$date_jul <- mod_conv_soil_n$Day_

mod_conv_soil_n <- mod_conv_soil_n %>%
  #filter(Year_ > 6) %>%
  mutate(year = case_when(
    Year_ == 1 ~ 2008,
    Year_ == 2 ~ 2009,
    Year_ == 3 ~ 2010,
    Year_ == 4 ~ 2011,
    Year_ == 5 ~ 2012,
    Year_ == 6 ~ 2013,
    Year_ == 7 ~ 2014,
    Year_ == 8 ~ 2015,
    Year_ == 9 ~ 2016,
    Year_ == 10 ~ 2017,
    Year_ == 11 ~ 2018,
    Year_ == 12 ~ 2019,
    Year_ == 13 ~ 2020,
    Year_ == 14 ~ 2021,
    Year_ == 15 ~ 2022,
    Year_ == 16 ~ 2023,
    Year_ == 17 ~ 2024
    
  ))

mod_conv_soil_n$date <- as.Date(paste(mod_conv_soil_n$year, mod_conv_soil_n$date_jul), format = "%Y %j")
mod_conv_soil_n$doy <- mod_conv_soil_n$date_jul

mod_conv_soil_n$nh4_0_15 <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
mod_conv_soil_n$no3_0_15 <- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2

mod_conv_soil_n <- mod_conv_soil_n %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

# plotting soil N for the entire period 2012-2023

mod_conv_soil_n %>%
  filter(year %in% 2018:2023) %>%
  ggplot(aes(x = doy, y = no3_0_15))+
  geom_errorbar(data = filter(obs_soil_n_mean_sd_p34, year %in% c(2018:2023)),
                aes(
                  x=doy,
                  ymin = no3_kg_n_ha_mean - no3_kg_n_ha_sd, 
                  ymax = no3_kg_n_ha_mean + no3_kg_n_ha_sd),
                position = position_dodge(width = 1),
                width = 0.3, inherit.aes = FALSE, colour = "#009E73"
  ) +
  geom_errorbar(data = filter(obs_soil_n_mean_sd_p34, year %in% c(2018:2023)),
                aes(
                  x=doy,
                  ymin = nh4_kg_n_ha_mean - nh4_kg_n_ha_sd, 
                  ymax = nh4_kg_n_ha_mean + nh4_kg_n_ha_sd),
                position = position_dodge(width = 1),
                width = 0.3, inherit.aes = FALSE, colour = "#D55E00"
  ) +
  geom_line(colour = "#009E73", alpha = 0.75, size = 0.75)+
  geom_point(data = filter(obs_soil_n_mean_sd_p34, year %in% c(2018:2023)), aes(x=doy, y=no3_kg_n_ha_mean), colour = "#009E73", alpha=0.5)+
  geom_point(data = filter(obs_soil_n_mean_sd_p34, year %in% c(2018:2023)), aes(x=doy, y=nh4_kg_n_ha_mean), colour = "#D55E00", alpha=0.5)+
  geom_line(data = filter(mod_conv_soil_n, year %in% 2018:2023), aes(x=doy, y=nh4_0_15), colour = "#D55E00", alpha=0.75, size = 0.75)+
  facet_wrap(year~.)+
  theme_bw()+
  #geom_vline(data = filter(fert_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  #geom_point(data = filter(mgmt_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  ylab("Soil N (kg N/ha)")

ggsave(filename = "soil_n_div.png",  width = 10, height = 6, dpi = 300)

# mod_conv_soil_n %>%
#   #filter(Year_ < 7) %>%
#   ggplot(aes(x = doy, y = no3_0_15))+
#   geom_line()+
#   geom_point(data = filter(obs_soil_n_p1_long, year>2011), aes(x=doy, y=no3_kg_n_ha), colour = "blue", alpha=0.5)+
#   facet_wrap(year~.)+
#   theme_bw()+
#   geom_vline(data = filter(fert_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)
# 
# mod_conv_soil_n %>%
#   #filter(Year_ < 7) %>%
#   ggplot(aes(x = doy, y = nh4_0_15))+
#   geom_line()+
#   geom_point(data = filter(obs_soil_n_p1_long, year>2011), aes(x=doy, y=nh4_kg_n_ha), colour = "blue", alpha=0.5)+
#   facet_wrap(year~.)+
#   theme_bw()+
#   geom_vline(data = filter(fert_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)

# plot other N losses as reported by DNDC

mod_conv_soil_n %>%
  filter(!is.na(crop_year)) %>% 
  group_by(crop_year) %>%
  summarise(
    n2o = sum(N2O.flux_),
    n2 = sum(N2.flux_),
    nh3 = sum(NH3.flux_),
    no3_l = sum(NO3.leach_),
    sum = n2o + n2 + nh3 + no3_l
  ) %>%
  pivot_longer(
    cols = c(n2o, n2,  nh3, no3_l,  sum),
    names_to = "losses",
    values_to = "value"
  ) %>%
  ggplot(aes(x = losses, y = value, fill = losses))+
  geom_col(position = position_dodge(), color = "black")+
  facet_wrap(crop_year~.)+
  theme_bw()+
  ylab("Annual N losses (kg N/ha)")+
  xlab("Pathways")+
  geom_text(aes(label = round(value, digits = 1)))

ggsave(filename = "n_lossess_div.png",  width = 10, height = 6, dpi = 300)

# # graphs
# 
# # nh4
# 
# merged_data <- merge(mod_conv_soil_n, obs_soil_n_p1, by = "date") %>% 
#   select(date, nh4_0_15, nh4_kg_n_ha, year.x) %>% 
#   pivot_longer(
#     cols = c(nh4_0_15, nh4_kg_n_ha), 
#     names_to = "measurement", 
#     values_to = "value"
#   ) %>% 
#   mutate(
#     obs_mod = ifelse(measurement == "nh4_0_15", "mod", "obs"),
#     date = as.factor(date)
#   )
# 
# ggplot(merged_data, aes(x = date, y = value, fill = obs_mod)) +
#   geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
#   scale_fill_manual(values = c("mod" = "#999999", "obs" = "#D55E00")) +  # Customize fill colors
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(year.x ~ ., scales = "free_x", ncol = 6) +  # Keep faceting if needed
#   theme(panel.spacing = unit(1, "lines"))+
#   ylab("Soil NH4 0-15 cm (kg N/ha)")
# 
# 
# # older line graph
# 
# ggplot(mod_conv_soil_n, aes(x = doy , y = nh4_0_15))+
#   geom_line()+
#   geom_point(data = obs_soil_n_p1,
#              aes(x = doy, y = nh4_kg_n_ha),
#              color = "black",
#              fill = "#D55E00",
#              shape = 21,
#              size = 2.5)+
#   facet_wrap(year ~ ., nrow = 3, ncol = 2) +
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5))+
#   scale_x_continuous(
#     breaks = seq(min(mod_conv_soil_n$doy), max(mod_conv_soil_n$doy), by = 20),
#   )+
#   geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4) 
# 
# 
# 
# # no3
# 
# merged_data <- merge(mod_conv_soil_n, obs_soil_n_p1, by = "date") %>% 
#   select(date, no3_0_15, no3_kg_n_ha, year.x) %>% 
#   pivot_longer(
#     cols = c(no3_0_15, no3_kg_n_ha), 
#     names_to = "measurement", 
#     values_to = "value"
#   ) %>% 
#   mutate(
#     obs_mod = ifelse(measurement == "no3_0_15", "mod", "obs"),
#     date = as.factor(date)
#   )
# 
# ggplot(merged_data, aes(x = date, y = value, fill = obs_mod)) +
#   geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
#   scale_fill_manual(values = c("mod" = "#999999", "obs" = "#009E73")) +  # Customize fill colors
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(year.x ~ ., scales = "free_x", ncol = 6) +  # Keep faceting if needed
#   theme(panel.spacing = unit(1, "lines"))+
#   ylab("Soil N03 0-15 cm (kg N/ha)")
# 
# # older line graph
# 
# ggplot(mod_conv_soil_n, aes(x = doy , y = no3_0_15))+
#   geom_line()+
#   geom_point(data = obs_soil_n_p1,
#              aes(x = doy, y = no3_kg_n_ha),
#              color = "black",
#              fill = "#009E73",
#              shape = 21,
#              size = 2.5)+
#   facet_wrap(year ~ ., nrow = 3, ncol = 2) +
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5))+
#   scale_x_continuous(
#     breaks = seq(min(mod_conv_soil_n$doy), max(mod_conv_soil_n$doy), by = 20)) +
#   geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4) 


### statistics

combined_data_n <- merge(mod_conv_soil_n, obs_soil_n_mean_sd_p34, by = c("year", "doy"))
combined_data_n <- combined_data_n %>% 
  filter(year>2017)

# nh4

# metrics_nh4_0_15_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- combined_data_n$nh4_kg_n_ha_mean
modeled <- combined_data_n$nh4_0_15

metrics_nh4_0_15<- calculate_metrics(observed, modeled)
metrics_nh4_0_15


# no3

# metrics_no3_0_15_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- combined_data_n$no3_kg_n_ha_mean
modeled <- combined_data_n$no3_0_15

metrics_no3_0_15<- calculate_metrics(observed, modeled)
metrics_no3_0_15
metrics_no3_0_15_df <- rbind(metrics_no3_0_15_df, metrics_no3_0_15)
metrics_no3_0_15_df

###### CO2 ############

# import modeled data for p3

mod_co2_p3 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilC_1.csv", header = FALSE)
colnames(mod_co2_p3) <- mod_co2_p3[2, ]
colnames(mod_co2_p3) <- make.names(colnames(mod_co2_p3), unique = TRUE)
mod_co2_p3 <- mod_co2_p3[-c(1,2),]
mod_co2_p3 <- data.frame(lapply(mod_co2_p3, as.numeric))

mod_co2_p3$date_jul <- mod_co2_p3$Day

mod_co2_p3$mod_resp <- mod_co2_p3$Eco.respiration*1000/10000 # transforming from kg C/ha (DNDC output) to g C/m2
mod_co2_p3$mod_gpp <- mod_co2_p3$Photosynthesis*(-1000/10000) # transforming from kg C/ha (DNDC output) to g C/m2
mod_co2_p3$mod_nee <- mod_co2_p3$X.NEE*1000/10000 # transforming from kg C/ha (DNDC output) to g C/m2
mod_co2_p3 <- mod_co2_p3 %>%
  filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024
  ))

mod_co2_p3$date <- as.Date(paste(mod_co2_p3$year, mod_co2_p3$date_jul), format = "%Y %j")
mod_co2_p3$doy <- mod_co2_p3$Day

# additional variables needed for graphs

mod_co2_p3 <- mod_co2_p3 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

mod_co2_p3 <- mod_co2_p3 %>%
  filter(!is.na(crop_year)) %>%
  mutate(
    doy_crop = case_when(
      doy >= 121 ~ doy - 120,          # DOY 121 → 1, 122 → 2, ..., 365 → 245
      doy < 121  ~ doy + (365 - 120)   # DOY 1 → 246, ..., 120 → 365
    )
  )

obs_co2_p3$year = obs_co2_p3$Year

#### statistics CO2 - calibration ####

merged_data_co2 <- mod_co2_p3 %>% 
  left_join(obs_co2_p3, by = c("year", "doy")) %>%
  mutate(crop_year = crop_year.x) %>% 
  filter(!is.na(crop_year))

# daily values

metrics_df_daily_corn_2cc <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

metrics_df_daily_soybean_ww <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

metrics_df_daily_ww_4cc <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)


# gpp 

# daytime

# all years
# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_dt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# by crop

# corn years
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn_2cc <- rbind(metrics_df_daily_corn_2cc, metrics_gpp)
metrics_df_daily_corn_2cc

# soybean years
predicted <- filter(merged_data_co2, crop_year %in% c("19/20"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("19/20"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_soybean_ww <- rbind(metrics_df_daily_soybean_ww, metrics_gpp)
metrics_df_daily_soybean_ww

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("20/21"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("20/21"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_ww_4cc <- rbind(metrics_df_daily_ww_4cc, metrics_gpp)
metrics_df_daily_ww_4cc

# nighttime

# all years
# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_nt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# corn years
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn_2cc <- rbind(metrics_df_daily_corn_2cc, metrics_gpp)
metrics_df_daily_corn_2cc

# soybean years
predicted <- filter(merged_data_co2, crop_year %in% c("19/20"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("19/20"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_soybean_ww <- rbind(metrics_df_daily_soybean_ww, metrics_gpp)
metrics_df_daily_soybean_ww

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("20/21"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("20/21"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily_ww_4cc <- rbind(metrics_df_daily_ww_4cc, metrics_gpp)
metrics_df_daily_ww_4cc

# respiration

# daytime

# all years
# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_dt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn_2cc <- rbind(metrics_df_daily_corn_2cc, metrics_resp)
metrics_df_daily_corn_2cc

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("19/20"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("19/20"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_soybean_ww <- rbind(metrics_df_daily_soybean_ww, metrics_resp)
metrics_df_daily_soybean_ww

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("20/21"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("20/21"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_ww_4cc <- rbind(metrics_df_daily_ww_4cc, metrics_resp)
metrics_df_daily_ww_4cc

# nightime

# all years
# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_nt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_corn_2cc <- rbind(metrics_df_daily_corn_2cc, metrics_resp)
metrics_df_daily_corn_2cc

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("19/20"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("19/20"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_soybean_ww <- rbind(metrics_df_daily_soybean_ww, metrics_resp)
metrics_df_daily_soybean_ww

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("20/21"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("20/21"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily_ww_4cc <- rbind(metrics_df_daily_ww_4cc, metrics_resp)
metrics_df_daily_ww_4cc

# nee

# all years

# predicted <- merged_data_co2$mod_nee
# observed <- merged_data_co2$nee_g_c_m2_day
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("18/19"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("18/19"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily_corn_2cc <- rbind(metrics_df_daily_corn_2cc, metrics_nee)
metrics_df_daily_corn_2cc

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("19/20"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("19/20"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily_soybean_ww <- rbind(metrics_df_daily_soybean_ww, metrics_nee)
metrics_df_daily_soybean_ww

# winter wheat
predicted <- filter(merged_data_co2, crop_year %in% c("20/21"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("20/21"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily_ww_4cc <- rbind(metrics_df_daily_ww_4cc, metrics_nee)
metrics_df_daily_ww_4cc

# statistics by week

weekly_co2 <- merged_data_co2 %>%
  mutate(week = week(date)) %>%
  group_by(week, crop_year) %>%
  summarise(
    weekly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    weekly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    weekly_mod_gpp = sum(mod_gpp),
    weekly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    weekly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    weekly_mod_reco = sum(mod_resp),
    weekly_obs_nee = sum(nee_g_c_m2_day),
    weekly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

# calibration

metrics_df_weekly <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# weekly daytime gpp

#overall
# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly nighttime gpp

#overall
# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly reco

# weekly daytime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# weekly nighttime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# nee

# all years

# predicted <- weekly_co2$weekly_mod_nee
# observed <- weekly_co2$weekly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("18/19"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("19/20"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("20/21"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# statistics by year

yearly_co2 <- merged_data_co2 %>%
  group_by(crop_year) %>%
  summarise(
    yearly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    yearly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    yearly_mod_gpp = sum(mod_gpp),
    yearly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    yearly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    yearly_mod_reco = sum(mod_resp),
    yearly_obs_nee = sum(nee_g_c_m2_day),
    yearly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

# metrics_df_yearly_corn_2cc <- data.frame(
#   n = numeric(0),
#   RMSE = numeric(0),
#   nRMSE = numeric(0),
#   d = numeric(0),
#   nARE = numeric(0)
# )
# 
# metrics_df_yearly_soybean_ww <- data.frame(
#   n = numeric(0),
#   RMSE = numeric(0),
#   nRMSE = numeric(0),
#   d = numeric(0),
#   nARE = numeric(0)
# )
# 
# metrics_df_yearly_ww_4cc <- data.frame(
#   n = numeric(0),
#   RMSE = numeric(0),
#   nRMSE = numeric(0),
#   d = numeric(0),
#   nARE = numeric(0)
# )

# daytime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn_2cc <- rbind(metrics_df_yearly_corn_2cc, metrics_gpp)
metrics_df_yearly_corn_2cc

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_soybean_ww <- rbind(metrics_df_yearly_soybean_ww, metrics_gpp)
metrics_df_yearly_soybean_ww

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_ww_4cc <- rbind(metrics_df_yearly_ww_4cc, metrics_gpp)
metrics_df_yearly_ww_4cc

# nightime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_nt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn_2cc <- rbind(metrics_df_yearly_corn_2cc, metrics_gpp)
metrics_df_yearly_corn_2cc

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_soybean_ww <- rbind(metrics_df_yearly_soybean_ww, metrics_gpp)
metrics_df_yearly_soybean_ww

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly_ww_4cc <- rbind(metrics_df_yearly_ww_4cc, metrics_gpp)
metrics_df_yearly_ww_4cc

# reco

# daytime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn_2cc <- rbind(metrics_df_yearly_corn_2cc, metrics_reco)
metrics_df_yearly_corn_2cc

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_soybean_ww <- rbind(metrics_df_yearly_soybean_ww, metrics_reco)
metrics_df_yearly_soybean_ww

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_ww_4cc <- rbind(metrics_df_yearly_ww_4cc, metrics_reco)
metrics_df_yearly_ww_4cc

# nightime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn_2cc <- rbind(metrics_df_yearly_corn_2cc, metrics_reco)
metrics_df_yearly_corn_2cc

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_soybean_ww <- rbind(metrics_df_yearly_soybean_ww, metrics_reco)
metrics_df_yearly_soybean_ww

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly_ww_4cc <- rbind(metrics_df_yearly_ww_4cc, metrics_reco)
metrics_df_yearly_ww_4cc

# nee

# overall

# predicted <- yearly_co2$yearly_mod_nee
# observed <- yearly_co2$yearly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("18/19"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly_corn_2cc <- rbind(metrics_df_yearly_corn_2cc, metrics_nee)
metrics_df_yearly_corn_2cc

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("19/20"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly_soybean_ww <- rbind(metrics_df_yearly_soybean_ww, metrics_nee)
metrics_df_yearly_soybean_ww

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("20/21"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly_ww_4cc <- rbind(metrics_df_yearly_ww_4cc, metrics_nee)
metrics_df_yearly_ww_4cc

#### statistics CO2 - validation ####

merged_data_co2 <- mod_co2_p3 %>% 
  left_join(obs_co2_p3, by = c("year", "doy")) %>%
  mutate(crop_year = crop_year.x) %>% 
  filter(!is.na(crop_year))

metrics_df_daily <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# gpp 

# daytime

# all years
# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_dt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# by crop

# corn years
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# soybean years
predicted <- filter(merged_data_co2, crop_year %in% c("22/23"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("22/23"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("23/24"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("23/24"))$gpp_mean_g_c_m2_day_dt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# nighttime

# all years
# predicted <- merged_data_co2$mod_gpp
# observed <- merged_data_co2$gpp_mean_g_c_m2_day_nt *-1
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
# metrics_df_daily

# corn years
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# soybean years
predicted <- filter(merged_data_co2, crop_year %in% c("22/23"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("22/23"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("23/24"))$mod_gpp
observed <- filter(merged_data_co2, crop_year %in% c("23/24"))$gpp_mean_g_c_m2_day_nt*-1
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_gpp)
metrics_df_daily

# respiration

# daytime

# all years
# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_dt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("22/23"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("22/23"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("23/24"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("23/24"))$reco_mean_g_c_m2_day_dt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# nightime

# all years
# predicted <- merged_data_co2$mod_resp
# observed <- merged_data_co2$reco_mean_g_c_m2_day_nt
# metrics_resp <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("22/23"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("22/23"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# winter wheat years
predicted <- filter(merged_data_co2, crop_year %in% c("23/24"))$mod_resp
observed <- filter(merged_data_co2, crop_year %in% c("23/24"))$reco_mean_g_c_m2_day_nt
metrics_resp <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_resp)
metrics_df_daily

# nee

# all years
# predicted <- merged_data_co2$mod_nee
# observed <- merged_data_co2$nee_g_c_m2_day
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
# metrics_df_daily

# corn
predicted <- filter(merged_data_co2, crop_year %in% c("21/22"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("21/22"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
metrics_df_daily

# soybeans
predicted <- filter(merged_data_co2, crop_year %in% c("22/23"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("22/23"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
metrics_df_daily

# winter wheat
predicted <- filter(merged_data_co2, crop_year %in% c("23/24"))$mod_nee
observed <- filter(merged_data_co2, crop_year %in% c("23/24"))$nee_g_c_m2_day
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_daily <- rbind(metrics_df_daily, metrics_nee)
metrics_df_daily

# statistics by week

# validation

metrics_df_weekly <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# weekly daytime gpp

#overall
# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly nighttime gpp

#overall
# predicted <- weekly_co2$weekly_mod_gpp
# observed <- weekly_co2$weekly_obs_gpp_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_mod_gpp
observed <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_gpp)
metrics_df_weekly

# weekly reco

# weekly daytime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# weekly nighttime reco

#overall
# predicted <- weekly_co2$weekly_mod_reco
# observed <- weekly_co2$weekly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_mod_reco
observed <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_reco)
metrics_df_weekly

# nee

# all years
# predicted <- weekly_co2$weekly_mod_nee
# observed <- weekly_co2$weekly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
# metrics_df_weekly

# corn years
predicted <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("21/22"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# soybean years
predicted <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("22/23"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# winter wheat years
predicted <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_mod_nee
observed <- filter(weekly_co2, crop_year %in% c("23/24"))$weekly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_weekly <- rbind(metrics_df_weekly, metrics_nee)
metrics_df_weekly

# statistics by year

yearly_co2 <- merged_data_co2 %>%
  group_by(crop_year) %>%
  summarise(
    yearly_obs_gpp_dt = -sum(gpp_mean_g_c_m2_day_dt),
    yearly_obs_gpp_nt = -sum(gpp_mean_g_c_m2_day_nt),
    yearly_mod_gpp = sum(mod_gpp),
    yearly_obs_reco_dt = sum(reco_mean_g_c_m2_day_dt),
    yearly_obs_reco_nt = sum(reco_mean_g_c_m2_day_nt),
    yearly_mod_reco = sum(mod_resp),
    yearly_obs_nee = sum(nee_g_c_m2_day),
    yearly_mod_nee = sum(mod_nee),
    .groups = "drop"
  )

metrics_df_yearly <- data.frame(
  n = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# daytime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_dt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_obs_gpp_dt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# nightime gpp

#overall
# predicted <- yearly_co2$yearly_mod_gpp
# observed <- yearly_co2$yearly_obs_gpp_nt
# metrics_gpp <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
# metrics_df_yearly

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_mod_gpp
observed <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_obs_gpp_nt
metrics_gpp <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_gpp)
metrics_df_yearly

# reco

# daytime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_dt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_obs_reco_dt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# nightime reco

#overall
# predicted <- yearly_co2$yearly_mod_reco
# observed <- yearly_co2$yearly_obs_reco_nt
# metrics_reco <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_mod_reco
observed <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_obs_reco_nt
metrics_reco <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_reco)
metrics_df_yearly

# nee

# overall
# predicted <- yearly_co2$yearly_mod_nee
# observed <- yearly_co2$yearly_obs_nee
# metrics_nee <- calculate_metrics(observed, predicted)
# metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
# metrics_df_yearly

# by crop

# corn years
predicted <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("21/22"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
metrics_df_yearly

# soybean years
predicted <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("22/23"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
metrics_df_yearly

# winter wheat years
predicted <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_mod_nee
observed <- filter(yearly_co2, crop_year %in% c("23/24"))$yearly_obs_nee
metrics_nee <- calculate_metrics(observed, predicted)
metrics_df_yearly <- rbind(metrics_df_yearly, metrics_nee)
metrics_df_yearly

#### graphs co2 ####

# daily graphs

# general items for all graphs

# Define the wrapped crop DOY ticks you want on the x-axis
custom_breaks <- c(1, 51, 101, 151, 201, 251, 301, 351)

# Corresponding labels in original DOY terms:
# 1  = 121 (start of crop year)
# 51 = 171
# 101 = 221
# 151 = 271
# 201 = 21  (wrapped back to start of year)
# 245 = 120 (end of crop year)

custom_labels <- c("121", "171", "221", "271", "321", "6", "56", "106")

custom_labels_facets <- c(
  "18/19" = "Year 1 (18-19 | Corn+2-CC)",
  "19/20" = "Year 2 (19-20 | Soybean+WW)",
  "20/21" = "Year 3 (20-21 | WW+4-CC)",
  "21/22" = "Year 4 (21-22 | Corn+2-CC)",
  "22/23" = "Year 5 (22-23 | Soybean+WW)",
  "23/24" = "Year 6 (23-24 | WW+4-CC)"
)

# Prepare labels data for management dates + DOY 365 line
mgmt_labels <- mgmt_dates_p3 %>%
  mutate(
    label = paste0("DOY = ", doy),
    y_pos = min(c(mod_co2_p3$mod_nee, obs_co2_p3$nee_g_c_m2_day), na.rm = TRUE) # slightly above max y
  )

# Add a row for the DOY 365 line label
doy365_label <- data.frame(
  doy_crop = 245,  # crop day corresponding to DOY 365 after wrapping
  label = "DOY = 365",
  y_pos = min(mgmt_labels$y_pos)
)

# respiration and GPP (displaying either NT or DT-partitioned GPP, in blue and orange, respectively)

ggplot() +
  
  geom_hline(yintercept = 0, color = "black")+
  
  
  # gpp
  geom_line(
    data = mod_co2_p3,
    aes(x = doy_crop, y = mod_gpp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # reco
  geom_line(
    data = mod_co2_p3,
    aes(x = doy_crop, y = mod_resp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#0072B2", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = doy_crop, ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt, ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt), fill = "#0072B2", alpha = 0.3) +
  
  # reco
  
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#D55E00", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt, ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt), fill = "#D55E00", alpha = 0.3) +
  
  
  geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_dt), color = "#D55E00", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = reco_mean_g_c_m2_day_dt  - reco_sd_ustar_g_c_dt, ymax = reco_mean_g_c_m2_day_dt  + reco_sd_ustar_g_c_dt), fill = "#D55E00", alpha = 0.3) +
  
  # gpp
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_nt), color = "#009E73", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = -gpp_mean_g_c_m2_day_nt  - gpp_sd_ustar_g_c_nt, ymax = -gpp_mean_g_c_m2_day_nt  + gpp_sd_ustar_g_c_nt), fill = "#009E73", alpha = 0.3) +
  
  geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_dt), color = "#009E73", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = -gpp_mean_g_c_m2_day_dt  - gpp_sd_ustar_g_c_dt, ymax = -gpp_mean_g_c_m2_day_dt  + gpp_sd_ustar_g_c_dt), fill = "#009E73", alpha = 0.3) +
  
  geom_vline(
    data = mgmt_dates_p3,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6
  ) +
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown"
  ) +
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "black"
  ) +
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  ylab(expression(CO[2]~fluxes~(g~C~m^{-2}~day^{-1})))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )+
  scale_y_continuous(limits = c(-35, 30), breaks = seq(-30, 30, by = 10))

ggsave(filename = "modeled_reco_gpp_div.png",  width = 8, height = 5, dpi = 300)

#ggsave(filename = "modeled_reco_gpp_div.png",  width = 4, height = 10, dpi = 300)


# nee

ggplot() +
  geom_hline(yintercept = 0, color = "black")+
  geom_line(
    data = mod_co2_p3,
    aes(x = doy_crop, y = mod_nee),
    size = 0.6, color = "black", alpha = 0.6
  ) +
  
  geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = doy_crop, ymin = nee_g_c_m2_day  - sd_comb_g_c, ymax = nee_g_c_m2_day  + sd_comb_g_c), fill = "#0072B2", alpha = 0.3) +
  
  geom_line(
    data = obs_co2_p3,
    aes(x = doy_crop, y = nee_g_c_m2_day),
    color = "#0072B2", size = 0.6, alpha = 0.6
  ) +
  geom_vline(
    data = mgmt_dates_p3,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6
  ) +
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = y_pos, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown"
  ) +
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = y_pos, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "black"
  ) +
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  ylab(expression(NEE~(g~C~m^{-2}~day^{-1})))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )+
  scale_y_continuous(limits = c(-20, 10), breaks = seq(-20, 10, by = 5))

ggsave(filename = "modeled_nee_div.png",  width = 8, height = 5, dpi = 300)

# annual graphs

# Load the data
obs_co2_p3_annual<- read_excel("obs_data/measured_co2/mean_sdAnnual_gC_all_div.xlsx")

# Process observed data (Nighttime, Daytime, NEE)
obs_long <- obs_co2_p3_annual %>%
  select(crop_year,
         mean_GPP_U50_f, mean_GPP_DT_U50,
         mean_Reco_U50_f, mean_Reco_DT_U50,
         mean_NEE_U50_f) %>%
  mutate(
    mean_GPP_U50_f = mean_GPP_U50_f * -1,
    mean_GPP_DT_U50 = mean_GPP_DT_U50 * -1
  ) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "var_method",
    values_to = "value"
  ) %>%
  mutate(
    variable = case_when(
      grepl("GPP", var_method) ~ "GPP",
      grepl("Reco", var_method) ~ "Reco",
      grepl("NEE", var_method) ~ "NEE"
    ),
    method = case_when(
      grepl("DT", var_method) ~ "Daytime",
      grepl("NEE", var_method) ~ "Nighttime",  # treat NEE as Nighttime
      TRUE ~ "Nighttime"
    )
  ) %>%
  select(crop_year, variable, method, value)

# Process SD data
sd_long <- obs_co2_p3_annual %>%
  select(crop_year,
         sd_GPP_Ustar_NT, sd_GPP_uStar_DT,
         sd_Reco_Ustar_NT, sd_Reco_Ustar_DT,
         sdComb) %>%
  pivot_longer(
    cols = -crop_year,
    names_to = "var_method",
    values_to = "sd"
  ) %>%
  mutate(
    variable = case_when(
      grepl("GPP", var_method) ~ "GPP",
      grepl("Reco", var_method) ~ "Reco",
      grepl("sdComb", var_method) ~ "NEE"
    ),
    method = case_when(
      grepl("DT", var_method) ~ "Daytime",
      grepl("sdComb", var_method) ~ "Nighttime",
      TRUE ~ "Nighttime"
    )
  ) %>%
  select(crop_year, variable, method, sd)

# Merge obs + SD and define aesthetics
obs_plot_df <- obs_long %>%
  left_join(sd_long, by = c("crop_year", "variable", "method")) %>%
  mutate(
    source = method,
    fill_color = case_when(
      variable == "GPP" & method == "Nighttime" ~ "#0072B2",
      variable == "GPP" & method == "Daytime"   ~ "#E69F00",
      variable == "Reco" & method == "Nighttime" ~ "#0072B2",
      variable == "Reco" & method == "Daytime"   ~ "#E69F00",
      variable == "NEE" ~ "#CC79A7",  # Special color for observed NEE
      TRUE ~ "#999999"  # Default or DNDC fallback
    ),
    pattern = case_when(
      variable == "Reco" ~ "stripe",
      variable == "NEE" ~ "circle",
      TRUE ~ "none"
    ),
    label_color = if_else(variable == "GPP", "white", "black"),
    label_vjust = case_when(
      variable == "GPP" ~ 0.5,
      variable == "Reco" ~ 0.4,
      variable == "NEE" ~ 0.5
    )
  )

# Process DNDC data
dndc_plot_df <- mod_co2_p3 %>%
  filter(!(Year == 2018 & doy < 121), !(Year == 2024 & doy > 120)) %>%
  group_by(crop_year) %>%
  summarise(
    NEE = sum(mod_nee, na.rm = TRUE),
    GPP = sum(mod_gpp, na.rm = TRUE),
    Reco = sum(mod_resp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(NEE, GPP, Reco), names_to = "variable", values_to = "value") %>%
  mutate(
    method = "DNDC",
    sd = NA,
    source = "DNDC",
    fill_color = "#999999",
    pattern = case_when(
      variable == "Reco" ~ "stripe",
      variable == "NEE" ~ "circle",
      TRUE ~ "none"
    ),
    label_color = if_else(variable == "GPP", "white", "black"),
    label_vjust = case_when(
      variable == "GPP" ~ 0.5,
      variable == "Reco" ~ 0.4,
      variable == "NEE" ~ 0.5
    )
  )

# Combine all
plot_df <- bind_rows(obs_plot_df, dndc_plot_df) %>%
  mutate(
    ymin = value - sd,
    ymax = value + sd,
    variable = factor(variable, levels = c("GPP", "Reco", "NEE")),
    method = factor(method, levels = c("Nighttime", "Daytime", "DNDC", "NEE")),
    var_method = factor(paste(variable, source, sep = "_"), levels = c(
      "GPP_Nighttime", "GPP_Daytime", "GPP_DNDC",
      "Reco_Nighttime", "Reco_Daytime", "Reco_DNDC",
      "NEE_Nighttime", "NEE_DNDC"  # Fix this line
    ))
  )

# --- Final Plot ---

plot_df %>% 
  filter(grepl("Daytime|DNDC", var_method) | var_method == "NEE_Nighttime") %>%
  ggplot(aes(x = var_method, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.9),
    colour = "black",
    pattern_fill = "white",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.1
  ) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge2(width = 0.9),
    width = 0.9,
    color = "black",
    na.rm = TRUE
  ) +
  geom_text(
    aes(label = round(value, 0), vjust = label_vjust, colour = label_color, hjust = -0.5),
    position = position_dodge2(width = 0.9),
    angle = 90,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_pattern_identity() +
  scale_color_identity() +
  scale_y_continuous(limits = c(-1800, 2000), breaks = seq(-1800, 2000, by = 200)) +
  facet_wrap(~ crop_year, nrow = 1, labeller = labeller(crop_year = custom_labels_facets)) +
  labs(
    #title = "Annual aggregated fluxes - Conventional rotation (p3+P2)",
    #subtitle = "Error bars = SD (NEE = random + u*-filtering uncertainty, GPP & Reco = u*-filtering uncertainty)",
    y = expression(CO[2] ~ "fluxes (g C m"^{-2}~"year"^{-1}~")")
  ) +
  scale_x_discrete(labels = c(
    "GPP_Nighttime" = "GPP NT",
    "GPP_Daytime" = "GPP DT",
    "GPP_DNDC" = "GPP DNDC",
    "Reco_Nighttime" = "Reco NT",
    "Reco_Daytime" = "Reco DT",
    "Reco_DNDC" = "Reco DNDC",
    "NEE_Nighttime" = "NEE",
    "NEE_DNDC" = "NEE DNDC"
  ))+
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    panel.grid = element_blank()  # ← this removes all gridlines
  )

ggsave(filename = "annual_div.png",  width = 11.5, height = 5, dpi = 300)

###### N2O #########

# import modeled data for p3

mod_n2o_p3 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
colnames(mod_n2o_p3) <- mod_n2o_p3[3, ]
colnames(mod_n2o_p3) <- make.names(colnames(mod_n2o_p3), unique = TRUE)
mod_n2o_p3 <- mod_n2o_p3[-c(1,2, 3, 4),]
mod_n2o_p3 <- data.frame(lapply(mod_n2o_p3, as.numeric))

mod_n2o_p3$date_jul <- mod_n2o_p3$Day

mod_n2o_p3$n2o_flux_g_n_ha_d <- mod_n2o_p3$N2O.flux*1000

mod_n2o_p3 <- mod_n2o_p3 %>%
  #filter(Year > 10) %>%
  mutate(year = case_when(
    Year == 1 ~ 2008,
    Year == 2 ~ 2009,
    Year == 3 ~ 2010,
    Year == 4 ~ 2011,
    Year == 5 ~ 2012,
    Year == 6 ~ 2013,
    Year == 7 ~ 2014,
    Year == 8 ~ 2015,
    Year == 9 ~ 2016,
    Year == 10 ~ 2017,
    
    Year == 11 ~ 2018,
    Year == 12 ~ 2019,
    Year == 13 ~ 2020,
    Year == 14 ~ 2021,
    Year == 15 ~ 2022,
    Year == 16 ~ 2023,
    Year == 17 ~ 2024,
  ))

mod_n2o_p3$date <- as.Date(paste(mod_n2o_p3$year, mod_n2o_p3$date_jul), format = "%Y %j")
mod_n2o_p3$doy <- mod_n2o_p3$Day
mod_n2o_p3 <- mod_n2o_p3 %>%
  mutate(
    crop_year = ifelse(year == 2018 & doy > 120 | year == 2019 & doy < 121, "18/19", 
                       ifelse(year == 2019 & doy > 120 | year == 2020 & doy < 121, "19/20",
                              ifelse(year == 2020 & doy > 120 | year == 2021 & doy < 121, "20/21",
                                     ifelse(year == 2021 & doy > 120 | year == 2022 & doy < 121, "21/22",
                                            ifelse(year == 2022 & doy > 120 | year == 2023 & doy < 121, "22/23",
                                                   ifelse(year == 2023 & doy > 120 | year == 2024 & doy < 121, "23/24", NA))))))
    
  )

# loading other years of n2o data to compare

# n2o_new_plot1_2000_2014 <- read_excel("obs_data/measured_n2o/other_or_older/e-26_longtermN2O_March_2025_Nir.xlsx", sheet = "e26_2000-2014_N2O_plot1")
# n2o_new_plot3_2015_2017 <- read_excel("obs_data/measured_n2o/other_or_older/e-26_longtermN2O_March_2025_Nir.xlsx", sheet = "e26_2015-2023_N2O_plot3")
# 
# n2o_new_plot1_2000_2014$doy = n2o_new_plot1_2000_2014$DOY
# n2o_new_plot1_2000_2014$year = n2o_new_plot1_2000_2014$Year
# n2o_new_plot3_2015_2017$doy = yday(n2o_new_plot3_2015_2017$Date) 
# n2o_new_plot3_2015_2017$year = n2o_new_plot3_2015_2017$Year

# graphs 

mod_n2o_p3 %>%
  filter(!is.na(crop_year)) %>% 
  ggplot(aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_d )) +
  geom_line(size = 1, color = "darkgray") +
  geom_line(data = filter(obs_n2o_p3p4_mean_sd, year %in% c(2018:2023)), aes(x = doy, y = n2o_flux_g_n_ha_day_mean ), color = "#0072B2", size = 1, alpha = 0.7) +
  geom_ribbon(data = filter(obs_n2o_p3p4_mean_sd, year %in% c(2018:2023)),aes(x = doy, ymin = n2o_flux_g_n_ha_day_mean - n2o_flux_g_n_ha_day_sd, ymax = n2o_flux_g_n_ha_day_mean + n2o_flux_g_n_ha_day_sd), 
              alpha = 0.3, fill = "#0072B2", inherit.aes = FALSE) +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw()+
  #geom_bar(data = filter(climate, year %in% c(2018:2023)) , aes(x = doy, y = prec*15), # 15 is an scaling factor
  #         stat = "identity", fill = "red", alpha = 1, position = "identity", width = 1) +
  theme(
    axis.text.x = element_text(angle = 90))+
 # geom_vline(data = filter(fert_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy), color = "darkred", linetype = "dashed", size = 1)+
  #geom_point(data = filter(mgmt_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
# geom_point(data = combined_data_water, aes(x = doy, y = X5cm * 100), size = 1, color = "blue", alpha = 0.5)+
# geom_point(data = combined_data_water, aes(x = doy, y = wfps_5), size = 1, color = "#D55E00", alpha = 0.5)+
  ylab("N2O emissions (g N/ha/day)")
  
#ggsave(filename = "modeled_n2o_div.png",  width = 10, height = 7, dpi = 300)

# statistics 
merged_data <- merge(mod_n2o_p3, obs_n2o_p3p4_mean_sd, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs")) %>% 
  filter(year>2017)

predicted <- merged_data$n2o_flux_g_n_ha_d
observed <- merged_data$n2o_flux_g_n_ha_day_mean
metrics_n2o <- calculate_metrics(observed, predicted)
metrics_n2o

# metrics_n2o_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

metrics_n2o_df <- rbind(metrics_n2o_df, metrics_n2o)
metrics_n2o_df

# graph aggregated emissions

merged_data <- merge(mod_n2o_p3, obs_n2o_p3p4_mean_sd, by = c("year", "doy"))%>%
  mutate(crop_year=crop_year.y) %>% 
  filter(!is.na(crop_year))

# Filter rows with no NA in either variable
filtered_data <- merged_data %>%
  filter(!is.na(n2o_flux_g_n_ha_d) & !is.na(n2o_flux_g_n_ha_day_mean)) %>% 
  select(year,crop_year, doy, n2o_flux_g_n_ha_d, n2o_flux_g_n_ha_day_mean)

# Summarize by year and type (mod/obs)
summary_data <- filtered_data %>%
  filter(!is.na(crop_year)) %>% 
  #mutate(year = as.integer(year)) %>%
  group_by(crop_year) %>%
  summarise(
    mod_sum = sum(n2o_flux_g_n_ha_d),
    obs_sum = sum(n2o_flux_g_n_ha_day_mean),
    n = n(),  # number of valid observations
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mod_sum, obs_sum),
               names_to = "source",
               values_to = "sum") %>%
  mutate(source = ifelse(source == "mod_sum", "Modeled", "Observed"))

# Plot
summary_data %>% 
  #filter(!year == 2024) %>% 
ggplot(aes(x = factor(crop_year), y = sum, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_text(aes(label = round(sum, 0)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  geom_text(data = summary_data %>%  distinct(crop_year, n),
            aes(x = factor(crop_year), y = 4500,
                label = paste("n =", n)),
            inherit.aes = FALSE, size = 3.5) +
  scale_fill_manual(values = c("Observed" = "#0072B2", "Modeled" = "#999999")) +
  labs(x = "Year", y = "Total N2O emissions (g N/ha)", fill = "Source",
       title = "Annual aggregated N2O emissions for days with observed data",
       subtitle = "n = number of daily observations considered") +
  theme_bw()

ggsave(filename = "modeled_n2o_div_annual.png",  width = 8, height = 7, dpi = 300)


predicted <- summary_data %>% 
  filter(source=="Modeled") %>% 
  select(sum)
observed <- summary_data %>% 
  filter(source=="Observed")%>% 
  select(sum)

metrics_n2o_cum <- calculate_metrics(observed, predicted)
metrics_n2o_cum

###### NECB ######

# estimate C removed, and uncertainty propagation

# This script calculates the propagated uncertainty in the estimation of C_removed = yield × C_content
# using the standard error propagation formula for the product of two variables.

# Relative uncertainty: σ_Cremoved / Cremoved = sqrt( (σ_yield / yield)^2 + (σ_C / C)^2 )

obs_yield_mean_sd_with_uncertainty <- obs_yield_mean_sd %>%
  # Calculate the mean C_removed using yield × C_concentration
  mutate(
    crop_c_removed_derived_mean = crop_yield_obs_kg_ha_mean * grain_c_removed_obs_frac_mean
  ) %>%
  # Propagate uncertainty: C = A × B → σ_C = C × sqrt( (σ_A/A)^2 + (σ_B/B)^2 )
  mutate(
    crop_c_removed_derived_sd = crop_c_removed_derived_mean *
      sqrt(
        (crop_yield_obs_kg_ha_sd / crop_yield_obs_kg_ha_mean)^2 +
          (grain_c_removed_obs_frac_sd  / grain_c_removed_obs_frac_mean)^2
      )
  ) %>% 
  mutate(
    crop_c_removed_derived_mean_gc_m2 = crop_c_removed_derived_mean * 0.1,
    crop_c_removed_derived_sd_gc_m2 = crop_c_removed_derived_sd * 0.1
  ) %>% 
  filter(year>2017 & group == "P3_P4" & crop_type %in% c("corn", "soybean", "winter wheat")) %>%
  select(year, crop_type, group, crop_c_removed_derived_mean_gc_m2, crop_c_removed_derived_sd_gc_m2) %>% 
  mutate(
    crop_year = case_when(
      year == 2018 ~ "18/19",
      year == 2019 ~ "19/20",
      year == 2020 ~ "20/21",
      year == 2021 ~ "21/22",
      year == 2022 ~ "22/23",
      year == 2023 ~ "23/24"
    )
  )

obs_yield_mean_sd_with_uncertainty

# load annual NEE and uncertainty

mean_sd_nee_annual_gc_m2_year <- read_excel("obs_data/measured_co2/mean_sdAnnual_gC_all_div.xlsx") %>% 
  select(crop_year, mean_NEE_U50_f, sdComb) %>% 
  mutate(mean_nee_u50_f_gc_m2 = mean_NEE_U50_f,
         sdcomb_nee_gc_m2 = sdComb) %>% 
  select(crop_year, mean_nee_u50_f_gc_m2, sdcomb_nee_gc_m2)
mean_sd_nee_annual_gc_m2_year

# total 

necb_obs <- obs_yield_mean_sd_with_uncertainty %>% 
  left_join(mean_sd_nee_annual_gc_m2_year, by = "crop_year") %>% 
  #left_join(obs_straw_p34_mean_sd, by = "crop_year") %>%
  #mutate(
  #  straw_obs_gc_m2_mean = ifelse(is.na(straw_obs_gc_m2_mean), 0, straw_obs_gc_m2_mean),
  #  straw_obs_gc_m2_sd = ifelse(is.na(straw_obs_gc_m2_sd), 0, straw_obs_gc_m2_sd)
  #) %>% 
  mutate(
    necb_mean_g_c_m2_obs = crop_c_removed_derived_mean_gc_m2 + mean_nee_u50_f_gc_m2  , # without straw
    necb_sd_g_c_m2_obs = sqrt(crop_c_removed_derived_sd_gc_m2^2 + sdcomb_nee_gc_m2 ^2 ) # without straw
    #necb_mean_g_c_m2_obs = crop_c_removed_derived_mean_gc_m2 + mean_nee_u50_f_gc_m2 + straw_obs_gc_m2_mean, # with straw
    #necb_sd_g_c_m2_obs = sqrt(crop_c_removed_derived_sd_gc_m2^2 + sdcomb_nee_gc_m2^2 + straw_obs_gc_m2_sd^2) # with straw
  ) %>% 
  select(crop_year, crop_type,necb_mean_g_c_m2_obs, necb_sd_g_c_m2_obs)
necb_obs

# This block calculates NECB and its uncertainty by summing:
# 1) Carbon removed in grain (grain yield × C content)
# 2) Carbon removed in straw
# 3) Annual Net Ecosystem Exchange (NEE)

# Uncertainty is propagated assuming the three components are independent:
# σ_NECB = sqrt(σ_grainC^2 + σ_NEE^2 + σ_strawC^2)

# This assumption may underestimate true uncertainty, since:
# - Grain and straw yields are likely positively correlated
# - Grain yield and NEE may also be correlated in good/bad growing seasons

# For now, we use the standard formula assuming independence, and note this limitation in reporting.

# cumulative amounts across the 6 years

# I think what Shannon did for previous papers was propagate using variance addition

# nee

cumulative_NEE_mean <- sum(mean_sd_nee_annual_gc_m2_year$mean_nee_u50_f_gc_m2)
cumulative_NEE_sd <- sqrt(sum(mean_sd_nee_annual_gc_m2_year$sdcomb_nee_gc_m2^2))
cumulative_NEE_mean
cumulative_NEE_sd

# crop c

cumulative_crop_c_mean <- sum(obs_yield_mean_sd_with_uncertainty$crop_c_removed_derived_mean_gc_m2)
cumulative_crop_c_sd <- sqrt(sum(obs_yield_mean_sd_with_uncertainty$crop_c_removed_derived_sd_gc_m2^2))
cumulative_crop_c_mean
cumulative_crop_c_sd

# straw

cumulative_straw_c_mean <- sum(obs_straw_p34_mean_sd$straw_obs_gc_m2_mean)
cumulative_straw_c_sd <- sqrt(sum(obs_straw_p34_mean_sd$straw_obs_gc_m2_sd^2))
cumulative_straw_c_mean
cumulative_straw_c_sd

# necb_obs

cumulative_crop_c_mean <- sum(necb_obs$necb_mean_g_c_m2_obs)
cumulative_crop_c_sd <- sqrt(sum(necb_obs$necb_sd_g_c_m2_obs^2))
cumulative_crop_c_mean
cumulative_crop_c_sd

# modeled values

# c removed with harvest

c_removed_grain_mod <- mod_yield_p3 %>%
  filter(year > 2017 & year < 2024) %>% 
  select(year, grain_c_kg_ha_mod, crop_type) %>% 
  mutate(
    grain_c_removed_gc_m2_mod = grain_c_kg_ha_mod*0.1,
    crop_year = case_when(
      year == 2018 ~ "18/19",
      year == 2019 ~ "19/20",
      year == 2020 ~ "20/21",
      year == 2021 ~ "21/22",
      year == 2022 ~ "22/23",
      year == 2023 ~ "23/24"
    )
  ) %>% 
  filter(crop_type %in% c("corn", "soybean", "winter wheat")) %>% 
  filter(!grain_c_kg_ha_mod == 0)
c_removed_grain_mod

# nee

annual_nee_mod <- mod_co2_p3 %>%
  filter(!is.na(crop_year)) %>% 
  group_by(crop_year) %>%
  summarise(
    method = "DNDC",
    nee_gc_m2_year_mod = sum(mod_nee, na.rm = TRUE),
    # GPP = sum(mod_gpp, na.rm = TRUE),
    # Reco = sum(mod_resp, na.rm = TRUE),
    .groups = "drop"
  ) 

necb_mod <- annual_nee_mod %>% 
  left_join(c_removed_grain_mod, by = "crop_year") %>% 
  mutate(
    necb_mean_g_c_m2_mod = nee_gc_m2_year_mod + grain_c_removed_gc_m2_mod ,
  ) %>% 
  select(crop_year, crop_type, necb_mean_g_c_m2_mod)
necb_mod


# # comparing NECB modeled and observed
# 
# # Reshape to long format
# necb_long <- necb_obs %>% 
#   left_join(necb_mod, by = "crop_year") %>%
#   select(crop_year, 
#          necb_mean_g_c_m2_obs, necb_sd_g_c_m2_obs, 
#          necb_mean_g_c_m2_mod) %>%
#   rename(
#     obs_mean = necb_mean_g_c_m2_obs,
#     obs_sd = necb_sd_g_c_m2_obs,
#     mod_mean = necb_mean_g_c_m2_mod
#   ) %>%
#   pivot_longer(cols = c(obs_mean, mod_mean), 
#                names_to = "source", 
#                values_to = "necb_mean") %>%
#   mutate(
#     necb_sd = ifelse(source == "obs_mean", obs_sd, NA_real_),
#     source = recode(source, "obs_mean" = "Obs", "mod_mean" = "Mod")
#   )
# 
# # Plot
# ggplot(necb_long, aes(x = source, y = necb_mean, fill = source)) +
#   geom_col(position = position_dodge(), width = 0.6, color = "black") +
#   geom_errorbar(
#     data = filter(necb_long, !is.na(necb_sd)),
#     aes(ymin = necb_mean - necb_sd, ymax = necb_mean + necb_sd),
#     width = 0.2,
#     position = position_dodge(0.6)
#   ) +
#   facet_wrap(~ crop_year, nrow=1) +
#   labs(
#     x = "Source",
#     y = expression(NECB~(g~C~m^{-2})),
#     fill = "Data Source"
#   ) +
#   geom_text(
#     aes(label = round(necb_mean, 0)),
#     vjust = -0.5,
#     position = position_dodge(0.6),
#     size = 3
#   ) +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# ggsave(filename = "necb_div.png", width = 5, height = 6, dpi = 300)

# Combine observed components
obs_components <- obs_yield_mean_sd_with_uncertainty %>%
  rename(grain_c_mean = crop_c_removed_derived_mean_gc_m2,
         grain_c_sd = crop_c_removed_derived_sd_gc_m2) %>%
  left_join(mean_sd_nee_annual_gc_m2_year, by = "crop_year") %>%
  rename(nee_mean = mean_nee_u50_f_gc_m2,
         nee_sd = sdcomb_nee_gc_m2) %>%
  left_join(necb_obs, by = c("crop_year", "crop_type")) %>%
  rename(necb_mean = necb_mean_g_c_m2_obs,
         necb_sd = necb_sd_g_c_m2_obs) %>%
  select(crop_year, crop_type, grain_c_mean, grain_c_sd,
         nee_mean, nee_sd, necb_mean, necb_sd) %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "component", values_to = "value") %>%
  mutate(
    sd = case_when(
      component == "grain_c_mean" ~ grain_c_sd,
      component == "nee_mean" ~ nee_sd,
      component == "necb_mean" ~ necb_sd,
      TRUE ~ NA_real_
    ),
    component = recode(component,
                       "grain_c_mean" = "Grain C",
                       "nee_mean" = "NEE",
                       "necb_mean" = "NECB"
    ),
    source = "Obs"
  )

# Combine observed components
obs_components <- obs_yield_mean_sd_with_uncertainty %>%
  rename(grain_c_mean = crop_c_removed_derived_mean_gc_m2,
         grain_c_sd = crop_c_removed_derived_sd_gc_m2) %>%
  left_join(mean_sd_nee_annual_gc_m2_year, by = "crop_year") %>%
  rename(nee_mean = mean_nee_u50_f_gc_m2,
         nee_sd = sdcomb_nee_gc_m2) %>%
  left_join(necb_obs, by = c("crop_year", "crop_type")) %>%
  rename(necb_mean = necb_mean_g_c_m2_obs,
         necb_sd = necb_sd_g_c_m2_obs) %>%
  select(crop_year, crop_type, grain_c_mean, grain_c_sd,
         nee_mean, nee_sd, necb_mean, necb_sd) %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "component", values_to = "value") %>%
  mutate(
    sd = case_when(
      component == "grain_c_mean" ~ grain_c_sd,
      component == "nee_mean" ~ nee_sd,
      component == "necb_mean" ~ necb_sd,
      TRUE ~ NA_real_
    ),
    component = recode(component,
                       "grain_c_mean" = "Grain C",
                       "nee_mean" = "NEE",
                       "necb_mean" = "NECB"
    ),
    source = "Obs"
  )

# Combine modeled components
mod_components <- c_removed_grain_mod %>%
  rename(grain_c = grain_c_removed_gc_m2_mod) %>%
  left_join(annual_nee_mod, by = "crop_year") %>%
  rename(nee = nee_gc_m2_year_mod) %>%
  left_join(necb_mod, by = c("crop_year", "crop_type")) %>%
  rename(necb = necb_mean_g_c_m2_mod) %>%
  select(crop_year, crop_type, grain_c, nee, necb) %>%
  pivot_longer(cols = c(grain_c, nee, necb), names_to = "component", values_to = "value") %>%
  mutate(
    sd = NA_real_,
    component = recode(component,
                       "grain_c" = "Grain C",
                       "nee" = "NEE",
                       "necb" = "NECB"
    ),
    source = "Mod"
  )

# Combine all and assign styles
plot_df <- bind_rows(obs_components, mod_components) %>%
  mutate(
    component = factor(component, levels = c("Grain C", "NEE", "NECB")),
    source = factor(source, levels = c("Obs", "Mod")),
    var_source = paste(component, source, sep = "_"),
    
    # Color and pattern settings
    fill_color = case_when(
      source == "Obs" ~ "#1b9e77",
      source == "Mod"  ~ "gray",
    ),
    pattern = case_when(
      component == "Grain C" ~ "none",
      component == "NEE"             ~ "stripe",
      component == "NECB"            ~ "circle"
    ),
    label_color = "black",
    ymin = value - sd,
    ymax = value + sd
  )

plot_df <- plot_df %>%
  mutate(
    var_source = factor(
      var_source,
      levels = c("Grain C_Obs", "Grain C_Mod",
                 "NEE_Obs", "NEE_Mod",
                 "NECB_Obs", "NECB_Mod")
    )
  )

ggplot(plot_df, aes(x = var_source, y = value, fill = fill_color, pattern = pattern)) +
  geom_col_pattern(
    position = position_dodge2(width = 0.9),
    colour = "black",
    pattern_fill = "white",
    pattern_colour = NA,
    pattern_angle = 45,
    pattern_density = 0.4,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.6,
    width = 0.6
  ) +
  geom_errorbar(
    data = filter(plot_df, !is.na(sd)),
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge2(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  geom_text(
    aes(label = round(value, 0), color = label_color),
    position = position_dodge2(width = 0.9),
    vjust = -1.5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_pattern_identity() +
  scale_color_identity() +
  facet_wrap(~ crop_year, nrow = 1) +
  labs(
    y = expression(NECB~components~(g~C~m^{-2}~year^{-1})),
    x = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, vjust =0.5)
  )

ggsave(filename = "necb_div.png", width = 10, height = 5, dpi = 300)

# statistics

metrics_df_annual_necb <- data.frame(
  n = numeric(0),
  Measured_Avg  = numeric(0),
  Modeled_Avg  = numeric(0),
  RMSE = numeric(0),
  nRMSE = numeric(0),
  d = numeric(0),
  nARE = numeric(0)
)

# necb 

# all years
predicted <- necb_mod$necb_mean_g_c_m2_mod
observed <- necb_obs$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

# corn years
predicted <- filter(necb_mod, crop_year %in% c("18/19", "21/22"))$necb_mean_g_c_m2_mod
observed <- filter(necb_obs, crop_year %in% c("18/19", "21/22"))$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

# soybean years
predicted <- filter(necb_mod, crop_year %in% c("19/20", "22/23"))$necb_mean_g_c_m2_mod
observed <- filter(necb_obs, crop_year %in% c("19/20", "22/23"))$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

# winter wheat years
predicted <- filter(necb_mod, crop_year %in% c("21/22", "23/24"))$necb_mean_g_c_m2_mod
observed <- filter(necb_obs, crop_year %in% c("21/22", "23/24"))$necb_mean_g_c_m2_obs
metrics <- calculate_metrics(observed, predicted)
metrics

#########################

###########################################
########## climate graph ##################

# exctracting the air temperature from the ReddyProc inputs, as those should allow me to calculate a more accurate average than just using min and max temperature in the DNDC file

library(REddyProc)

#2018
EddyData_2018_p3 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p3_2018")
EddyData_2018_p3 <- EddyData_2018_p3 %>%
  filter(DoY < 194) 
EddyData_2018_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2018") 
EddyData_2018_p1 <- EddyData_2018_p1 %>%
  filter(DoY > 193)
EddyData_2019_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2019")
EddyData_2020_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2020")
EddyData_2021_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2021")
EddyData_2022_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2022")
EddyData_2023_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2023")
EddyData_2024_p1 <- fLoadTXTIntoDataframe("combined_ec_fg_full_data_allyear_p1_2024")

# merging all years in a single dataset

EddyData_p1 <- rbind(EddyData_2018_p3, EddyData_2018_p1, EddyData_2019_p1, EddyData_2020_p1, EddyData_2021_p1, EddyData_2022_p1, EddyData_2023_p1,EddyData_2024_p1) 

daily_temp <- EddyData_p1 %>% 
  select(Year, DoY, Tair) %>% 
  group_by(Year, DoY) %>% 
  summarise(
    daily_temp = mean(Tair, na.rm = TRUE)
  )

# retriving the rest of the data from DNDC files 

col_names <- c("julian_day", "obs_max_temp_c", "obs_min_temp_c", "obs_precip_cm", "obs_wind2m_ms", "obs_rad_mjm2day", "obs_rh_pct")

climate_2018 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2018.txt", skip = 1, header = FALSE, col.names = col_names)  
climate_2019 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2019.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2020 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2020.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2021 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2021.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2022 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2022.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2023 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2023.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2024 <- read.table("C:/DNDC/sites/elora_c/data/climate/elora2024.txt", skip = 1, header = FALSE, col.names = col_names)
climate_2018$Year = 2018
climate_2019$Year = 2019
climate_2020$Year = 2020
climate_2021$Year = 2021
climate_2022$Year = 2022
climate_2023$Year = 2023
climate_2024$Year = 2024

climate_data <- rbind(climate_2018, climate_2019, climate_2020, climate_2021, climate_2022, climate_2023, climate_2024) 

# plotting

# Define the wrapped crop DOY ticks you want on the x-axis
custom_breaks <- c(1, 51, 101, 151, 201, 251, 301, 351)

custom_labels <- c("121", "171", "221", "271", "321", "6", "56", "106")

custom_labels_facets <- c(
  "18/19" = "Year 1 (18-19)",
  "19/20" = "Year 2 (19-20)",
  "20/21" = "Year 3 (20-21)",
  "21/22" = "Year 4 (21-22)",
  "22/23" = "Year 5 (22-23)",
  "23/24" = "Year 6 (23-24)"
)

# Add a row for the DOY 365 line label
doy365_label <- data.frame(
  doy_crop = 245,  # crop day corresponding to DOY 365 after wrapping
  label = "DOY = 365",
  y_pos = min(mgmt_labels$y_pos)
)

combined <- daily_temp %>%
  mutate(julian_day = DoY) %>%
  left_join(climate_data %>% 
              mutate(julian_day = julian_day), 
            by = c("Year", "julian_day"))

# there seems to be some missing values for temperature, I will quickly interpolate, as it is just a few

temp_fixed <- combined$daily_temp  
# interpolate only for the NA positions
temp_fixed[is.na(temp_fixed)] <- na.approx(
  temp_fixed, 
  x = combined$julian_day, 
  na.rm = FALSE
)[is.na(temp_fixed)]
# put it back
combined$daily_temp <- temp_fixed



combined <- combined %>%
  mutate(
    crop_year = ifelse(Year == 2018 & DoY > 120 | Year == 2019 & DoY < 121, "18/19",
                       ifelse(Year == 2019 & DoY > 120 | Year == 2020 & DoY < 121, "19/20",
                              ifelse(Year == 2020 & DoY > 120 | Year == 2021 & DoY < 121, "20/21",
                                     ifelse(Year == 2021 & DoY > 120 | Year == 2022 & DoY < 121, "21/22",
                                            ifelse(Year == 2022 & DoY > 120 | Year == 2023 & DoY < 121, "22/23",
                                                   ifelse(Year == 2023 & DoY > 120 | Year == 2024 & DoY < 121, "23/24", NA))))))
    
  )

combined <- combined %>%
  filter(!is.na(crop_year)) %>%
  mutate(
    doy_crop = case_when(
      DoY >= 121 ~ DoY - 120,          # DOY 121 → 1, 122 → 2, ..., 365 → 245
      DoY < 121  ~ DoY + (365 - 120)   # DOY 1 → 246, ..., 120 → 365
    )
  )

ggplot(combined, aes(x = doy_crop)) +
  # Daily temperature (primary axis)
  geom_line(aes(y = daily_temp), color = "#D55E00", size = 0.5, alpha=0.8) +
  # Precipitation (rescaled to plot against same primary axis, but displayed on secondary axis)
  geom_col(aes(y = obs_precip_cm*10 / 4), fill = "#0072B2",  size=1) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~.*4, name = "Precipitation (mm)")
  ) +
  labs(x = "Day of Year") +
  theme_bw()+
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    #axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )

ggsave(filename = "climate.png", width = 10, height = 5, dpi = 300)


# respiration and GPP (displaying either NT or DT-partitioned GPP, in blue and orange, respectively)

ggplot() +
  
  geom_hline(yintercept = 0, color = "black")+
  
  
  # gpp
  geom_line(
    data = mod_co2_p3,
    aes(x = doy_crop, y = mod_gpp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # reco
  geom_line(
    data = mod_co2_p3,
    aes(x = doy_crop, y = mod_resp),
    size = 0.7, color = "black", alpha = 0.6
  ) +
  
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#0072B2", linetype = 1, size = 0.7, alpha = 0.5)+
  # geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = doy_crop, ymin = reco_mean_g_c_m2_day_nt  - reco_sd_ustar_g_c_nt, ymax = reco_mean_g_c_m2_day_nt  + reco_sd_ustar_g_c_nt), fill = "#0072B2", alpha = 0.3) +
  
  # reco
  
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_nt), color = "#D55E00", linetype = "dotted", size = 0.7, alpha = 0.5)+
  
  geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = reco_mean_g_c_m2_day_dt), color = "#D55E00", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = reco_mean_g_c_m2_day_dt  - reco_sd_ustar_g_c_dt, ymax = reco_mean_g_c_m2_day_dt  + reco_sd_ustar_g_c_dt), fill = "#D55E00", alpha = 0.3) +
  
  # gpp
  # geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_nt), color = "#009E73", linetype = "dotted", size = 0.7, alpha = 0.5)+
  
  geom_line(data =  filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), y = -gpp_mean_g_c_m2_day_dt), color = "#009E73", linetype = 1, size = 0.7, alpha = 0.5)+
  geom_ribbon(data = filter(obs_co2_p3, !is.na(crop_year)), aes(x = as.numeric(doy_crop), ymin = -gpp_mean_g_c_m2_day_dt  - gpp_sd_ustar_g_c_dt, ymax = -gpp_mean_g_c_m2_day_dt  + gpp_sd_ustar_g_c_dt), fill = "#009E73", alpha = 0.3) +
  
  geom_vline(
    data = mgmt_dates_p3,
    aes(xintercept = doy_crop),
    color = "brown", linetype = "dashed", size = 0.6
  ) +
  geom_vline(xintercept = 245, linetype = "dotted", color = "gray50", size = 0.6) + # Crop day for DOY 365
  # Add text labels for vertical lines
  geom_text(
    data = mgmt_labels,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "brown"
  ) +
  geom_text(
    data = doy365_label,
    aes(x = doy_crop, y = -30, label = label),
    angle = 90,
    vjust = 1.4,  # Adjust vertical justification (above the line)
    hjust = 0,     # Align text so it’s right over the line
    size = 3.0,
    color = "black"
  ) +
  facet_wrap(~crop_year, nrow = 2, labeller = labeller(crop_year = custom_labels_facets)) +
  ylab(expression(CO[2]~fluxes~(g~C~m^{-2}~day^{-1})))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.title.x = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = custom_breaks,
    labels = custom_labels,
    limits = c(1, 365),
    expand = c(0, 0)
  )+
  scale_y_continuous(limits = c(-35, 20), breaks = seq(-35, 20, by = 5))

ggsave(filename = "modeled_reco_gpp_div.png",  width = 11, height = 6, dpi = 300)

###########################################









############################################################################################
################# OLDER CODE - Do not look below this point ###############################
###########################################################################################

############################################################
#### final code for scenarios  - ALL ROTATION ######

# Define the base folder where Case1 to Case10 are located

base_folder <- "C:/DNDC/Result/Record/Batch"  # Path to Folder B

# Initialize empty data frames for results
results_table_co2 <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_gpp <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_nee <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_n2o <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_yield <- data.frame(Scenario = character(0),
                                  PBIAS = numeric(0),
                                  NSE = numeric(0),
                                  RMSE = numeric(0),
                                  RRMSE_percent = numeric(0),
                                  Index_of_Agreement_d = numeric(0),
                                  R2 = numeric(0),
                                  stringsAsFactors = FALSE)

results_table_soil_water <- data.frame(Scenario = character(0),
                                       PBIAS = numeric(0),
                                       NSE = numeric(0),
                                       RMSE = numeric(0),
                                       RRMSE_percent = numeric(0),
                                       Index_of_Agreement_d = numeric(0),
                                       R2 = numeric(0),
                                       stringsAsFactors = FALSE)

# Loop through scenarios 1 to 15 (this needs changing based on the number of scenarios - change the number after "1:" in the first line)
for (i in 1:2) {
  # Construct the folder name based on the specified structure
  folder_name <- paste0("Case", i, "-p1_conv_new")
  folder_path <- file.path(base_folder, folder_name)
  
  # Construct the file path for "Day_SoilC_1.csv"
  file_path_co2 <- file.path(folder_path, "Day_SoilC_1.csv")
  
  # Check if the CO2 file exists
  if (file.exists(file_path_co2)) {
    # Read the CO2 CSV file
    mod_co2_p1 <- read.csv(file_path_co2, header = FALSE)
    
    # Process the CO2 data
    colnames(mod_co2_p1) <- mod_co2_p1[2, ]
    colnames(mod_co2_p1) <- make.names(colnames(mod_co2_p1), unique = TRUE)
    mod_co2_p1 <- mod_co2_p1[-c(1, 2), ]
    mod_co2_p1 <- data.frame(lapply(mod_co2_p1, as.numeric))
    
    mod_co2_p1$date_jul <- mod_co2_p1$Day
    mod_co2_p1$mod_resp <- mod_co2_p1$Eco.respiration * 1000 / 10000
    mod_co2_p1$mod_gpp <- mod_co2_p1$Photosynthesis * (-1000 / 10000)
    mod_co2_p1$mod_nee <- mod_co2_p1$X.NEE * 1000 / 10000
    
    mod_co2_p1 <- mod_co2_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
    
    # Assume obs_co2_p1 is defined elsewhere; merge data for CO2
    merged_data <- merge(mod_co2_p1, obs_co2_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Calculate metrics for mod_resp
    predicted_resp <- merged_data$mod_resp
    observed_resp <- merged_data$obs_resp
    metrics_resp <- calculate_metrics(observed_resp, predicted_resp)
    
    # Add the results for mod_resp to the results table
    scenario_name <- paste("Scenario", i)
    results_table_co2 <- rbind(results_table_co2, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_resp)))
    
    # Calculate metrics for mod_gpp
    predicted_gpp <- merged_data$mod_gpp
    observed_gpp <- merged_data$obs_gpp  # Ensure this is the correct observed column name
    metrics_gpp <- calculate_metrics(observed_gpp, predicted_gpp)
    
    # Add results for mod_gpp
    results_table_gpp <- rbind(results_table_gpp, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_gpp)))
    
    # Calculate metrics for mod_nee
    predicted_nee <- merged_data$mod_nee
    observed_nee <- merged_data$obs_nee  # Ensure this is the correct observed column name
    metrics_nee <- calculate_metrics(observed_nee, predicted_nee)
    
    # Add results for mod_nee
    results_table_nee <- rbind(results_table_nee, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_nee)))
  } else {
    message(paste("CO2 file not found:", file_path_co2))
  }
  
  # Now handle the N2O data
  file_path_n2o <- file.path(folder_path, "Day_SoilN_1.csv")
  
  # Check if the N2O file exists
  if (file.exists(file_path_n2o)) {
    # Read the N2O CSV file
    mod_n2o_p1 <- read.csv(file_path_n2o, header = FALSE)
    
    # Process the N2O data
    colnames(mod_n2o_p1) <- mod_n2o_p1[3, ]
    colnames(mod_n2o_p1) <- make.names(colnames(mod_n2o_p1), unique = TRUE)
    mod_n2o_p1 <- mod_n2o_p1[-c(1, 2, 3, 4), ]
    mod_n2o_p1 <- data.frame(lapply(mod_n2o_p1, as.numeric))
    
    mod_n2o_p1$date_jul <- mod_n2o_p1$Day
    mod_n2o_p1$n2o_flux_g_n_ha_d <- mod_n2o_p1$N2O.flux * 1000
    
    mod_n2o_p1 <- mod_n2o_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_n2o_p1$date <- as.Date(paste(mod_n2o_p1$year, mod_n2o_p1$date_jul), format = "%Y %j")
    
    # Assume obs_n2o_p1 is defined elsewhere; merge data for N2O
    merged_n2o_data <- merge(mod_n2o_p1, obs_n2o_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Calculate metrics for N2O
    predicted_n2o <- merged_n2o_data$n2o_flux_g_n_ha_d
    observed_n2o <- merged_n2o_data$N2O_.g_N.ha.d.  # Ensure this is the correct observed column name
    metrics_n2o <- calculate_metrics(observed_n2o, predicted_n2o)
    
    # Add the results for N2O to the results table
    results_table_n2o <- rbind(results_table_n2o, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_n2o)))
    
  } else {
    message(paste("N2O file not found:", file_path_n2o))
  }
  
  # Handle the yield data
  file_paths_yield <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(file_paths_yield) > 0) {
    results <- do.call(rbind, lapply(file_paths_yield, extract_info))
    mod_yield_p1 <- results %>%
      filter(year > 6) %>%
      mutate(year = case_when(
        year == 7 ~ 2018,
        year == 8 ~ 2019,
        year == 9 ~ 2020,
        year == 10 ~ 2021,
        year == 11 ~ 2022,
        year == 12 ~ 2023
      ))
    
    mod_yield_p1$type <- "mod"
    mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
    
    # Creating a column for yield
    mod_yield_p1$crop_yield_kg_ha <- ifelse(mod_yield_p1$crop_name %in% c("Corn", "Soybean", "Winter Wheat"), mod_yield_p1$grain_c_kg_ha / 0.40, mod_yield_p1$grain_c_kg_ha / 0.40)
    
    modeled_yield <- mod_yield_p1$crop_yield_kg_ha
    observed_yield <- obs_yield_p1$crop_yield_obs_kg_ha  # Ensure this is the correct observed column name
    metrics_yield <- calculate_metrics(observed_yield, modeled_yield)
    
    # Add the results for yield to the results table
    results_table_yield <- rbind(results_table_yield, 
                                 cbind(Scenario = scenario_name, as.data.frame(metrics_yield)))
  } else {
    message("Yield files not found.")
  }
  
  # Handle the soil water content data
  file_path_soil_water <- file.path(folder_path, "Day_SoilClimate_1.csv")
  
  if (file.exists(file_path_soil_water)) {
    mod_soil_water_p1 <- read.csv(file_path_soil_water, header = FALSE)
    mod_soil_water_p1 <- mod_soil_water_p1[, c(1:5, 28:49)]
    colnames(mod_soil_water_p1) <- mod_soil_water_p1[3,]
    mod_soil_water_p1 <- mod_soil_water_p1[-c(1:3),]
    mod_soil_water_p1 <- data.frame(lapply(mod_soil_water_p1, as.numeric))
    
    mod_soil_water_p1$date_jul <- mod_soil_water_p1$Day
    mod_soil_water_p1$wfps_25_mod <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2
    
    mod_soil_water_p1$vwc_5 <- mod_soil_water_p1$X5cm * (1.2 / 2.65) * 100
    mod_soil_water_p1$vwc_25 <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2 * (1.3 / 2.65) * 100
    
    mod_soil_water_p1 <- mod_soil_water_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_soil_water_p1$date <- as.Date(paste(mod_soil_water_p1$year, mod_soil_water_p1$date_jul), format = "%Y %j")
    
    # Merge with observed data
    combined_data <- merge(obs_soil_wfps_p1, mod_soil_water_p1, by = "date")
    
    # Calculate metrics for soil water content at 5 cm
    observed_wfps_5 <- combined_data$wfps_5
    modeled_wfps_5 <- combined_data$X5cm * 100
    metrics_wfps_5 <- calculate_metrics(observed_wfps_5, modeled_wfps_5)
    
    # Add results for soil water content at 5 cm
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_5", as.data.frame(metrics_wfps_5)))
    
    # Calculate metrics for soil water content at 25 cm
    observed_wfps_25 <- combined_data$wfps_25
    modeled_wfps_25 <- combined_data$wfps_25_mod * 100
    metrics_wfps_25 <- calculate_metrics(observed_wfps_25, modeled_wfps_25)
    
    # Add results for soil water content at 25 cm
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_25", as.data.frame(metrics_wfps_25)));
    
  } else {
    message("Soil water content file not found.")
  }
}

# Print the results tables
print(results_table_yield)
print(results_table_gpp)
print(results_table_co2)
print(results_table_nee)
print(results_table_n2o)
print(results_table_soil_water)


# Select and transpose the table
install.packages("writexl")
library(writexl)

table_new <- results_table_n2o %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new.xlsx")

table_new <- results_table_co2 %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new2.xlsx")

table_new <- results_table_gpp %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new3.xlsx")

table_new <- results_table_yield %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new4.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_5") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new5.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_25") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new6.xlsx")


#############################################################

###################################################################################
#### final code for scenarios in specific years - CONVENTIONAL ROTATION::CORN #####

# Initialize empty data frames for results

base_folder <- "C:/DNDC/Result/Record/Batch"  # Path to Folder B

results_table_co2 <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_gpp <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_nee <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_n2o <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_yield <- data.frame(Scenario = character(0),
                                  PBIAS = numeric(0),
                                  NSE = numeric(0),
                                  RMSE = numeric(0),
                                  RRMSE_percent = numeric(0),
                                  Index_of_Agreement_d = numeric(0),
                                  R2 = numeric(0),
                                  stringsAsFactors = FALSE)

results_table_soil_water <- data.frame(Scenario = character(0),
                                       PBIAS = numeric(0),
                                       NSE = numeric(0),
                                       RMSE = numeric(0),
                                       RRMSE_percent = numeric(0),
                                       Index_of_Agreement_d = numeric(0),
                                       R2 = numeric(0),
                                       stringsAsFactors = FALSE)

results_table_soil_n <- data.frame(Scenario = character(0),
                                   PBIAS = numeric(0),
                                   NSE = numeric(0),
                                   RMSE = numeric(0),
                                   RRMSE_percent = numeric(0),
                                   Index_of_Agreement_d = numeric(0),
                                   R2 = numeric(0),
                                   stringsAsFactors = FALSE)


results_grain_c <- data.frame(Scenario = character(0),
                              PBIAS = numeric(0),
                              NSE = numeric(0),
                              RMSE = numeric(0),
                              RRMSE_percent = numeric(0),
                              Index_of_Agreement_d = numeric(0),
                              R2 = numeric(0),
                              stringsAsFactors = FALSE)



# Define the years of interest - CORN

years_of_interest <- c(2018, 2021)

for (i in 1:7) {
  folder_name <- paste0("Case", i, "-p1_conv_new")
  folder_path <- file.path(base_folder, folder_name)
  
  file_path_co2 <- file.path(folder_path, "Day_SoilC_1.csv")
  
  if (file.exists(file_path_co2)) {
    mod_co2_p1 <- read.csv(file_path_co2, header = FALSE)
    colnames(mod_co2_p1) <- mod_co2_p1[2, ]
    colnames(mod_co2_p1) <- make.names(colnames(mod_co2_p1), unique = TRUE)
    mod_co2_p1 <- mod_co2_p1[-c(1, 2), ]
    mod_co2_p1 <- data.frame(lapply(mod_co2_p1, as.numeric))
    
    mod_co2_p1$date_jul <- mod_co2_p1$Day
    mod_co2_p1$mod_resp <- mod_co2_p1$Eco.respiration * 1000 / 10000
    mod_co2_p1$mod_gpp <- mod_co2_p1$Photosynthesis * (-1000 / 10000)
    mod_co2_p1$mod_nee <- mod_co2_p1$X.NEE * 1000 / 10000
    
    mod_co2_p1 <- mod_co2_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
    
    merged_data <- merge(mod_co2_p1, obs_co2_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Filter merged data for the years of interest
    merged_data <- merged_data[merged_data$year_mod %in% years_of_interest, ]
    
    # Calculate metrics for mod_resp
    predicted_resp <- merged_data$mod_resp
    observed_resp <- merged_data$obs_resp
    metrics_resp <- calculate_metrics(observed_resp, predicted_resp)
    
    scenario_name <- paste("Scenario", i)
    results_table_co2 <- rbind(results_table_co2, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_resp)))
    
    # Calculate metrics for mod_gpp
    predicted_gpp <- merged_data$mod_gpp
    observed_gpp <- merged_data$obs_gpp
    metrics_gpp <- calculate_metrics(observed_gpp, predicted_gpp)
    
    results_table_gpp <- rbind(results_table_gpp, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_gpp)))
    
    # Calculate metrics for mod_nee
    predicted_nee <- merged_data$mod_nee
    observed_nee <- merged_data$obs_nee
    metrics_nee <- calculate_metrics(observed_nee, predicted_nee)
    
    results_table_nee <- rbind(results_table_nee, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_nee)))
  } else {
    message(paste("CO2 file not found:", file_path_co2))
  }
  
  # Handle N2O data
  file_path_n2o <- file.path(folder_path, "Day_SoilN_1.csv")
  
  if (file.exists(file_path_n2o)) {
    mod_n2o_p1 <- read.csv(file_path_n2o, header = FALSE)
    colnames(mod_n2o_p1) <- mod_n2o_p1[3, ]
    colnames(mod_n2o_p1) <- make.names(colnames(mod_n2o_p1), unique = TRUE)
    mod_n2o_p1 <- mod_n2o_p1[-c(1:4), ]
    mod_n2o_p1 <- data.frame(lapply(mod_n2o_p1, as.numeric))
    
    mod_n2o_p1$date_jul <- mod_n2o_p1$Day
    mod_n2o_p1$n2o_flux_g_n_ha_d <- mod_n2o_p1$N2O.flux * 1000
    
    mod_n2o_p1 <- mod_n2o_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_n2o_p1$date <- as.Date(paste(mod_n2o_p1$year, mod_n2o_p1$date_jul), format = "%Y %j")
    
    merged_n2o_data <- merge(mod_n2o_p1, obs_n2o_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Filter merged data for the years of interest
    merged_n2o_data <- merged_n2o_data[merged_n2o_data$year_obs %in% years_of_interest, ]
    
    predicted_n2o <- merged_n2o_data$n2o_flux_g_n_ha_d
    observed_n2o <- merged_n2o_data$N2O_.g_N.ha.d.
    metrics_n2o <- calculate_metrics(observed_n2o, predicted_n2o)
    
    results_table_n2o <- rbind(results_table_n2o, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_n2o)))
  } else {
    message(paste("N2O file not found:", file_path_n2o))
  }
  
  # Handle yield data
  file_paths_yield <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(file_paths_yield) > 0) {
    results <- do.call(rbind, lapply(file_paths_yield, extract_info))
    mod_yield_p1 <- results %>%
      filter(year > 6) %>%
      mutate(year = case_when(
        year == 7 ~ 2018,
        year == 8 ~ 2019,
        year == 9 ~ 2020,
        year == 10 ~ 2021,
        year == 11 ~ 2022,
        year == 12 ~ 2023
      ))
    
    mod_yield_p1 <- mod_yield_p1[mod_yield_p1$year %in% years_of_interest, ]
    
    mod_yield_p1$type <- "mod"
    mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
    
    mod_yield_p1$crop_yield_kg_ha <- ifelse(mod_yield_p1$crop_name %in% c("Corn", "Soybean", "Winter Wheat"), mod_yield_p1$grain_c_kg_ha / 0.40, mod_yield_p1$grain_c_kg_ha / 0.40)
    
    modeled_yield <- mod_yield_p1$crop_yield_kg_ha
    observed_yield <- obs_yield_p1[obs_yield_p1$year %in% years_of_interest, ]$crop_yield_obs_kg_ha
    metrics_yield <- calculate_metrics(observed_yield, modeled_yield)
    
    results_table_yield <- rbind(results_table_yield, 
                                 cbind(Scenario = scenario_name, as.data.frame(metrics_yield)))
  } else {
    message("Yield files not found.")
  }
  
  # Handle crop grain C data
  file_paths_yield <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(file_paths_yield) > 0) {
    results <- do.call(rbind, lapply(file_paths_yield, extract_info))
    mod_yield_p1 <- results %>%
      filter(year > 6) %>%
      mutate(year = case_when(
        year == 7 ~ 2018,
        year == 8 ~ 2019,
        year == 9 ~ 2020,
        year == 10 ~ 2021,
        year == 11 ~ 2022,
        year == 12 ~ 2023
      ))
    
    mod_yield_p1 <- mod_yield_p1[mod_yield_p1$year %in% years_of_interest, ]
    
    mod_yield_p1$type <- "mod"
    mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
    
    modeled_grain_c <- mod_yield_p1$grain_c_kg_ha
    observed_grain_c <- obs_yield_p1[obs_yield_p1$year %in% years_of_interest, ]$grain_c_kg_ha
    metrics_grain_c <- calculate_metrics(observed_grain_c, modeled_grain_c)
    
    results_table_grain_c <- rbind(results_table_yield, 
                                   cbind(Scenario = scenario_name, as.data.frame(metrics_grain_c)))
  } else {
    message("Grain C files not found.")
  }
  
  
  # Handle soil water content data
  file_path_soil_water <- file.path(folder_path, "Day_SoilClimate_1.csv")
  
  if (file.exists(file_path_soil_water)) {
    mod_soil_water_p1 <- read.csv(file_path_soil_water, header = FALSE)
    mod_soil_water_p1 <- mod_soil_water_p1[, c(1:5, 28:49)]
    colnames(mod_soil_water_p1) <- mod_soil_water_p1[3,]
    mod_soil_water_p1 <- mod_soil_water_p1[-c(1:3),]
    mod_soil_water_p1 <- data.frame(lapply(mod_soil_water_p1, as.numeric))
    
    mod_soil_water_p1$date_jul <- mod_soil_water_p1$Day
    mod_soil_water_p1$wfps_25_mod <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2
    
    mod_soil_water_p1$vwc_5 <- mod_soil_water_p1$X5cm * (1.2 / 2.65) * 100
    mod_soil_water_p1$vwc_25 <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2 * (1.3 / 2.65) * 100
    
    mod_soil_water_p1 <- mod_soil_water_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_soil_water_p1$date <- as.Date(paste(mod_soil_water_p1$year, mod_soil_water_p1$date_jul), format = "%Y %j")
    
    combined_data <- merge(obs_soil_wfps_p1, mod_soil_water_p1, by = "date")
    
    # Filter combined data for the years of interest
    combined_data <- combined_data[combined_data$year.y %in% years_of_interest, ]
    
    # Calculate metrics for soil water content at 5 cm
    observed_wfps_5 <- combined_data$wfps_5
    modeled_wfps_5 <- combined_data$X5cm * 100
    metrics_wfps_5 <- calculate_metrics(observed_wfps_5, modeled_wfps_5)
    
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_5", as.data.frame(metrics_wfps_5)))
    
    # Calculate metrics for soil water content at 25 cm
    observed_wfps_25 <- combined_data$wfps_25
    modeled_wfps_25 <- combined_data$wfps_25_mod * 100
    metrics_wfps_25 <- calculate_metrics(observed_wfps_25, modeled_wfps_25)
    
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_25", as.data.frame(metrics_wfps_25)));
    
  } else {
    message("Soil water content file not found.")
  }
  
  # handle soil N
  file_path_soil_n <- file.path(folder_path, "Day_SoilN_1.csv")
  
  if (file.exists(file_path_soil_n)) {
    
    mod_conv_soil_n <- read.csv(file_path_soil_n, header = FALSE)
    combined_names <- paste(mod_conv_soil_n[3, ], mod_conv_soil_n[4, ], sep = "_")
    colnames(mod_conv_soil_n) <- combined_names
    
    mod_conv_soil_n <- mod_conv_soil_n[-c(1:4),]
    mod_conv_soil_n <- data.frame(lapply(mod_conv_soil_n, as.numeric))
    
    mod_conv_soil_n$date_jul <- mod_conv_soil_n$Day_
    
    mod_conv_soil_n <- mod_conv_soil_n %>%
      filter(Year_ > 6) %>%
      mutate(year = case_when(
        Year_ == 7 ~ 2018,
        Year_ == 8 ~ 2019,
        Year_ == 9 ~ 2020,
        Year_ == 10 ~ 2021,
        Year_ == 11 ~ 2022,
        Year_ == 12 ~ 2023
      ))
    
    mod_conv_soil_n$date <- as.Date(paste(mod_conv_soil_n$year, mod_conv_soil_n$date_jul), format = "%Y %j")
    
    mod_conv_soil_n$nh4_0_15 <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
    mod_conv_soil_n$no3_0_15 <- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2
    
    
    # Filter combined data for the years of interest
    combined_data <- merge(obs_soil_n_p1, mod_conv_soil_n, by = "date")
    combined_data <- combined_data[combined_data$year.y %in% years_of_interest, ]
    
    # nh4
    observed_nh4 <- combined_data$nh4_kg_n_ha
    modeled_nh4 <- combined_data$nh4_0_15
    metrics_nh4 <- calculate_metrics(observed_nh4, modeled_nh4)
    
    results_table_soil_n <- rbind(results_table_soil_n, 
                                  cbind(Scenario = scenario_name, Variable = "nh4", as.data.frame(metrics_nh4)))
    
    # no3
    observed_no3 <- combined_data$no3_kg_n_ha
    modeled_no3 <- combined_data$no3_0_15
    metrics_no3 <- calculate_metrics(observed, modeled)
    
    results_table_soil_n <- rbind(results_table_soil_n, 
                                  cbind(Scenario = scenario_name, Variable = "no3", as.data.frame(metrics_no3)))
    
  } else {
    message("Soil n content file not found.")
  }
  
}


# Print the results tables
print(results_table_yield)
print(results_table_gpp)
print(results_table_co2)
print(results_table_nee)
print(results_table_n2o)
print(results_table_soil_water)
print(results_table_soil_n)


table_new <- results_table_n2o %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new.xlsx")

table_new <- results_table_co2 %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new2.xlsx")

table_new <- results_table_gpp %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new3.xlsx")

table_new <- results_table_yield %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new4.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_5") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new5.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_25") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new6.xlsx")


###################################################################################

#################################################################################
#### final code for scenarios in specific years - CONVENTIONAL ROTATION::SOYBEANS #####

# Initialize empty data frames for results
results_table_co2 <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_gpp <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_nee <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_n2o <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_yield <- data.frame(Scenario = character(0),
                                  PBIAS = numeric(0),
                                  NSE = numeric(0),
                                  RMSE = numeric(0),
                                  RRMSE_percent = numeric(0),
                                  Index_of_Agreement_d = numeric(0),
                                  R2 = numeric(0),
                                  stringsAsFactors = FALSE)

results_table_soil_water <- data.frame(Scenario = character(0),
                                       PBIAS = numeric(0),
                                       NSE = numeric(0),
                                       RMSE = numeric(0),
                                       RRMSE_percent = numeric(0),
                                       Index_of_Agreement_d = numeric(0),
                                       R2 = numeric(0),
                                       stringsAsFactors = FALSE)

# Define the years of interest - CORN

years_of_interest <- c(2019, 2020)

for (i in 1:25) {
  folder_name <- paste0("Case", i, "-p1_conv_new")
  folder_path <- file.path(base_folder, folder_name)
  
  file_path_co2 <- file.path(folder_path, "Day_SoilC_1.csv")
  
  if (file.exists(file_path_co2)) {
    mod_co2_p1 <- read.csv(file_path_co2, header = FALSE)
    colnames(mod_co2_p1) <- mod_co2_p1[2, ]
    colnames(mod_co2_p1) <- make.names(colnames(mod_co2_p1), unique = TRUE)
    mod_co2_p1 <- mod_co2_p1[-c(1, 2), ]
    mod_co2_p1 <- data.frame(lapply(mod_co2_p1, as.numeric))
    
    mod_co2_p1$date_jul <- mod_co2_p1$Day
    mod_co2_p1$mod_resp <- mod_co2_p1$Eco.respiration * 1000 / 10000
    mod_co2_p1$mod_gpp <- mod_co2_p1$Photosynthesis * (-1000 / 10000)
    mod_co2_p1$mod_nee <- mod_co2_p1$X.NEE * 1000 / 10000
    
    mod_co2_p1 <- mod_co2_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
    
    merged_data <- merge(mod_co2_p1, obs_co2_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Filter merged data for the years of interest
    merged_data <- merged_data[merged_data$year_mod %in% years_of_interest, ]
    
    # Calculate metrics for mod_resp
    predicted_resp <- merged_data$mod_resp
    observed_resp <- merged_data$obs_resp
    metrics_resp <- calculate_metrics(observed_resp, predicted_resp)
    
    scenario_name <- paste("Scenario", i)
    results_table_co2 <- rbind(results_table_co2, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_resp)))
    
    # Calculate metrics for mod_gpp
    predicted_gpp <- merged_data$mod_gpp
    observed_gpp <- merged_data$obs_gpp
    metrics_gpp <- calculate_metrics(observed_gpp, predicted_gpp)
    
    results_table_gpp <- rbind(results_table_gpp, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_gpp)))
    
    # Calculate metrics for mod_nee
    predicted_nee <- merged_data$mod_nee
    observed_nee <- merged_data$obs_nee
    metrics_nee <- calculate_metrics(observed_nee, predicted_nee)
    
    results_table_nee <- rbind(results_table_nee, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_nee)))
  } else {
    message(paste("CO2 file not found:", file_path_co2))
  }
  
  # Handle N2O data
  file_path_n2o <- file.path(folder_path, "Day_SoilN_1.csv")
  
  if (file.exists(file_path_n2o)) {
    mod_n2o_p1 <- read.csv(file_path_n2o, header = FALSE)
    colnames(mod_n2o_p1) <- mod_n2o_p1[3, ]
    colnames(mod_n2o_p1) <- make.names(colnames(mod_n2o_p1), unique = TRUE)
    mod_n2o_p1 <- mod_n2o_p1[-c(1:4), ]
    mod_n2o_p1 <- data.frame(lapply(mod_n2o_p1, as.numeric))
    
    mod_n2o_p1$date_jul <- mod_n2o_p1$Day
    mod_n2o_p1$n2o_flux_g_n_ha_d <- mod_n2o_p1$N2O.flux * 1000
    
    mod_n2o_p1 <- mod_n2o_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_n2o_p1$date <- as.Date(paste(mod_n2o_p1$year, mod_n2o_p1$date_jul), format = "%Y %j")
    
    merged_n2o_data <- merge(mod_n2o_p1, obs_n2o_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Filter merged data for the years of interest
    merged_n2o_data <- merged_n2o_data[merged_n2o_data$year_obs %in% years_of_interest, ]
    
    predicted_n2o <- merged_n2o_data$n2o_flux_g_n_ha_d
    observed_n2o <- merged_n2o_data$N2O_.g_N.ha.d.
    metrics_n2o <- calculate_metrics(observed_n2o, predicted_n2o)
    
    results_table_n2o <- rbind(results_table_n2o, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_n2o)))
  } else {
    message(paste("N2O file not found:", file_path_n2o))
  }
  
  # Handle yield data
  file_paths_yield <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(file_paths_yield) > 0) {
    results <- do.call(rbind, lapply(file_paths_yield, extract_info))
    mod_yield_p1 <- results %>%
      filter(year > 6) %>%
      mutate(year = case_when(
        year == 7 ~ 2018,
        year == 8 ~ 2019,
        year == 9 ~ 2020,
        year == 10 ~ 2021,
        year == 11 ~ 2022,
        year == 12 ~ 2023
      ))
    
    mod_yield_p1 <- mod_yield_p1[mod_yield_p1$year %in% years_of_interest, ]
    
    mod_yield_p1$type <- "mod"
    mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
    
    mod_yield_p1$crop_yield_kg_ha <- ifelse(mod_yield_p1$crop_name %in% c("Corn", "Soybean", "Winter Wheat"), mod_yield_p1$grain_c_kg_ha / 0.40, mod_yield_p1$grain_c_kg_ha / 0.40)
    
    modeled_yield <- mod_yield_p1$crop_yield_kg_ha
    observed_yield <- obs_yield_p1[obs_yield_p1$year %in% years_of_interest, ]$crop_yield_obs_kg_ha
    metrics_yield <- calculate_metrics(observed_yield, modeled_yield)
    
    results_table_yield <- rbind(results_table_yield, 
                                 cbind(Scenario = scenario_name, as.data.frame(metrics_yield)))
  } else {
    message("Yield files not found.")
  }
  
  # Handle soil water content data
  file_path_soil_water <- file.path(folder_path, "Day_SoilClimate_1.csv")
  
  if (file.exists(file_path_soil_water)) {
    mod_soil_water_p1 <- read.csv(file_path_soil_water, header = FALSE)
    mod_soil_water_p1 <- mod_soil_water_p1[, c(1:5, 28:49)]
    colnames(mod_soil_water_p1) <- mod_soil_water_p1[3,]
    mod_soil_water_p1 <- mod_soil_water_p1[-c(1:3),]
    mod_soil_water_p1 <- data.frame(lapply(mod_soil_water_p1, as.numeric))
    
    mod_soil_water_p1$date_jul <- mod_soil_water_p1$Day
    mod_soil_water_p1$wfps_25_mod <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2
    
    mod_soil_water_p1$vwc_5 <- mod_soil_water_p1$X5cm * (1.2 / 2.65) * 100
    mod_soil_water_p1$vwc_25 <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2 * (1.3 / 2.65) * 100
    
    mod_soil_water_p1 <- mod_soil_water_p1 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022,
        Year == 12 ~ 2023
      ))
    
    mod_soil_water_p1$date <- as.Date(paste(mod_soil_water_p1$year, mod_soil_water_p1$date_jul), format = "%Y %j")
    
    combined_data <- merge(obs_soil_wfps_p1, mod_soil_water_p1, by = "date")
    
    # Filter combined data for the years of interest
    combined_data <- combined_data[combined_data$year.y %in% years_of_interest, ]
    
    # Calculate metrics for soil water content at 5 cm
    observed_wfps_5 <- combined_data$wfps_5
    modeled_wfps_5 <- combined_data$X5cm * 100
    metrics_wfps_5 <- calculate_metrics(observed_wfps_5, modeled_wfps_5)
    
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_5", as.data.frame(metrics_wfps_5)))
    
    # Calculate metrics for soil water content at 25 cm
    observed_wfps_25 <- combined_data$wfps_25
    modeled_wfps_25 <- combined_data$wfps_25_mod * 100
    metrics_wfps_25 <- calculate_metrics(observed_wfps_25, modeled_wfps_25)
    
    results_table_soil_water <- rbind(results_table_soil_water, 
                                      cbind(Scenario = scenario_name, Variable = "wfps_25", as.data.frame(metrics_wfps_25)));
    
  } else {
    message("Soil water content file not found.")
  }
}

# Print the results tables
print(results_table_yield)
print(results_table_gpp)
print(results_table_co2)
print(results_table_nee)
print(results_table_n2o)
print(results_table_soil_water)

table_new <- results_table_n2o %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new.xlsx")

table_new <- results_table_co2 %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new2.xlsx")

table_new <- results_table_gpp %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new3.xlsx")

table_new <- results_table_yield %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new4.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_5") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new5.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_25") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new6.xlsx")


#################################################################################

####################################
#### final code for p3 ######

# Define the base folder where Case1 to Case10 are located

base_folder <- "C:/DNDC/Result/Record/Batch"  # Path to Folder B

# Initialize empty data frames for results
results_table_co2 <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_gpp <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_nee <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_n2o <- data.frame(Scenario = character(0),
                                PBIAS = numeric(0),
                                NSE = numeric(0),
                                RMSE = numeric(0),
                                RRMSE_percent = numeric(0),
                                Index_of_Agreement_d = numeric(0),
                                R2 = numeric(0),
                                stringsAsFactors = FALSE)

results_table_yield <- data.frame(Scenario = character(0),
                                  PBIAS = numeric(0),
                                  NSE = numeric(0),
                                  RMSE = numeric(0),
                                  RRMSE_percent = numeric(0),
                                  Index_of_Agreement_d = numeric(0),
                                  R2 = numeric(0),
                                  stringsAsFactors = FALSE)

results_table_soil_water <- data.frame(Scenario = character(0),
                                       PBIAS = numeric(0),
                                       NSE = numeric(0),
                                       RMSE = numeric(0),
                                       RRMSE_percent = numeric(0),
                                       Index_of_Agreement_d = numeric(0),
                                       R2 = numeric(0),
                                       stringsAsFactors = FALSE)

# Loop through scenarios 1 to 15
for (i in 1:6) {
  # Construct the folder name based on the specified structure
  folder_name <- paste0("Case", i, "-p3_new")
  folder_path <- file.path(base_folder, folder_name)
  
  # Construct the file path for "Day_SoilC_1.csv"
  file_path_co2 <- file.path(folder_path, "Day_SoilC_1.csv")
  
  # Check if the CO2 file exists
  if (file.exists(file_path_co2)) {
    # Read the CO2 CSV file
    mod_co2_p3 <- read.csv(file_path_co2, header = FALSE)
    
    # Process the CO2 data
    colnames(mod_co2_p3) <- mod_co2_p3[2, ]
    colnames(mod_co2_p3) <- make.names(colnames(mod_co2_p3), unique = TRUE)
    mod_co2_p3 <- mod_co2_p3[-c(1, 2), ]
    mod_co2_p3 <- data.frame(lapply(mod_co2_p3, as.numeric))
    
    mod_co2_p3$date_jul <- mod_co2_p3$Day
    mod_co2_p3$mod_resp <- mod_co2_p3$Eco.respiration * 1000 / 10000
    mod_co2_p3$mod_gpp <- mod_co2_p3$Photosynthesis * (-1000 / 10000)
    mod_co2_p3$mod_nee <- mod_co2_p3$X.NEE * 1000 / 10000
    
    mod_co2_p3 <- mod_co2_p3 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022
      ))
    
    mod_co2_p3$date <- as.Date(paste(mod_co2_p3$year, mod_co2_p3$date_jul), format = "%Y %j")
    
    # Assume obs_co2_p3 is defined elsewhere; merge data for CO2
    merged_data <- merge(mod_co2_p3, obs_co2_p3, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Calculate metrics for mod_resp
    predicted_resp <- merged_data$mod_resp
    observed_resp <- merged_data$obs_resp
    metrics_resp <- calculate_metrics(observed_resp, predicted_resp)
    
    # Add the results for mod_resp to the results table
    scenario_name <- paste("Scenario", i)
    results_table_co2 <- rbind(results_table_co2, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_resp)))
    
    # Calculate metrics for mod_gpp
    predicted_gpp <- merged_data$mod_gpp
    observed_gpp <- merged_data$obs_gpp  # Ensure this is the correct observed column name
    metrics_gpp <- calculate_metrics(observed_gpp, predicted_gpp)
    
    # Add results for mod_gpp
    results_table_gpp <- rbind(results_table_gpp, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_gpp)))
    
    # Calculate metrics for mod_nee
    predicted_nee <- merged_data$mod_nee
    observed_nee <- merged_data$obs_nee  # Ensure this is the correct observed column name
    metrics_nee <- calculate_metrics(observed_nee, predicted_nee)
    
    # Add results for mod_nee
    results_table_nee <- rbind(results_table_nee, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_nee)))
  } else {
    message(paste("CO2 file not found:", file_path_co2))
  }
  
  # Now handle the N2O data
  file_path_n2o <- file.path(folder_path, "Day_SoilN_1.csv")
  
  # Check if the N2O file exists
  if (file.exists(file_path_n2o)) {
    # Read the N2O CSV file
    mod_n2o_p3 <- read.csv(file_path_n2o, header = FALSE)
    
    # Process the N2O data
    colnames(mod_n2o_p3) <- mod_n2o_p3[3, ]
    colnames(mod_n2o_p3) <- make.names(colnames(mod_n2o_p3), unique = TRUE)
    mod_n2o_p3 <- mod_n2o_p3[-c(1, 2, 3, 4), ]
    mod_n2o_p3 <- data.frame(lapply(mod_n2o_p3, as.numeric))
    
    mod_n2o_p3$date_jul <- mod_n2o_p3$Day
    mod_n2o_p3$n2o_flux_g_n_ha_d <- mod_n2o_p3$N2O.flux * 1000
    
    mod_n2o_p3 <- mod_n2o_p3 %>%
      filter(Year > 6) %>%
      mutate(year = case_when(
        Year == 7 ~ 2018,
        Year == 8 ~ 2019,
        Year == 9 ~ 2020,
        Year == 10 ~ 2021,
        Year == 11 ~ 2022
      ))
    
    mod_n2o_p3$date <- as.Date(paste(mod_n2o_p3$year, mod_n2o_p3$date_jul), format = "%Y %j")
    
    # Assume obs_n2o_p1 is defined elsewhere; merge data for N2O
    merged_n2o_data <- merge(mod_n2o_p3, obs_n2o_p3, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))
    
    # Calculate metrics for N2O
    predicted_n2o <- merged_n2o_data$n2o_flux_g_n_ha_d
    observed_n2o <- merged_n2o_data$N2O_.g_N.ha.d.  # Ensure this is the correct observed column name
    metrics_n2o <- calculate_metrics(observed_n2o, predicted_n2o)
    
    # Add the results for N2O to the results table
    results_table_n2o <- rbind(results_table_n2o, 
                               cbind(Scenario = scenario_name, as.data.frame(metrics_n2o)))
    
  } else {
    message(paste("N2O file not found:", file_path_n2o))
  }
  
}

# Print the results tables
print(results_table_co2)
print(results_table_gpp)
print(results_table_nee)
print(results_table_n2o)
print(results_table_yield)
print(results_table_soil_water)


# Select and transpose the table
install.packages("writexl")
library(writexl)

table_new <- results_table_n2o %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new.xlsx")

table_new <- results_table_co2 %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new2.xlsx")

table_new <- results_table_gpp %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new3.xlsx")

table_new <- results_table_yield %>% 
  select(Scenario, RRMSE_percent) %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new4.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_5") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new5.xlsx")

table_new <- results_table_soil_water %>% 
  select(Scenario, RRMSE_percent, Variable) %>%
  filter(Variable == "wfps_25") %>% 
  t()  # Transpose the data
table_new_df <- as.data.frame(table_new)
write_xlsx(table_new_df, "table_new6.xlsx")


####################################3

####################################

#####################################
######## new code for Christine #####

# import modeled data

mod_c <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilC_1.csv", header = FALSE)
colnames(mod_c) <- mod_c[2, ]
colnames(mod_c) <- make.names(colnames(mod_c), unique = TRUE)
mod_c <- mod_c[-c(1,2),]
mod_c <- data.frame(lapply(mod_c, as.numeric))

mod_c$date_jul <- mod_c$Day

mod_c$mod_nee <- mod_c$X.NEE # X.NEE tracks daily NEE; a positive value indicates net carbon loss and a negative value a net carbon gain.
mod_c$mod_soc <- mod_c$dSOC # dSOC track daily change in SOC; a negative value indicates a net decrease in SOC, and a positive value a net increase in SOC.

mod_c <- mod_c %>%
  filter(Year > 10) %>%
  mutate(year = case_when(
    Year == 11 ~ 2000,
    Year == 12 ~ 2001,
    Year == 13 ~ 2002,
    Year == 14 ~ 2003,
    Year == 15 ~ 2004,
    Year == 16 ~ 2005,
    Year == 17 ~ 2006,
    Year == 18 ~ 2007,
    Year == 19 ~ 2008,
    Year == 20 ~ 2009,
    Year == 21 ~ 2010,
    Year == 22 ~ 2011,
    Year == 23 ~ 2012,
    Year == 24 ~ 2013,
    Year == 25 ~ 2014,
    Year == 26 ~ 2015,
    Year == 27 ~ 2016,
    Year == 28 ~ 2017,
    Year == 29 ~ 2018,
    Year == 30 ~ 2019,
    Year == 31 ~ 2020,
    Year == 32 ~ 2021,
    Year == 33 ~ 2022,
    Year == 34 ~ 2023
  ))

mod_c$date <- as.Date(paste(mod_c$year, mod_c$date_jul), format = "%Y %j")


# graph net ecosystem exchange

mod_c %>% 
  group_by(year) %>%
  summarize(
    mod_nee = sum(mod_nee)
  ) %>% 
  ggplot(aes(x = factor(year), y = mod_nee))+
  geom_col(color = "black", fill = "#D55E00")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+  # Adjusted for better visibility
  ylab("Annual NEE (kg C/ha/year)")+
  xlab("Year")+
  scale_x_discrete(breaks = unique(mod_c$year))+
  geom_text(aes(label = round(mod_nee, digits = 0), y=3000), vjust = 0.5, angle = 90)  # Corrected to use 'label' aesthetic

mod_c %>% 
  summarize(
    mod_nee = sum(mod_nee)
  ) %>% 
  ggplot(aes(x = "", y = mod_nee)) +  # Use an empty string for x
  geom_col(color = "black", fill = "#D55E00")+
  theme_bw()+
  ylab("NEE (kg C/ha/year)")+
  geom_text(aes(label = round(mod_nee, digits = 0), vjust = 0.5))  # Corrected to use 'label' aesthetic


# graph soil organic carbon

mod_c %>% 
  group_by(year) %>%
  summarize(
    mod_soc = sum(mod_soc)
  ) %>% 
  ggplot(aes(x = factor(year), y = mod_soc))+
  geom_col(color = "black", fill = "#009E73")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+  # Adjusted for better visibility
  ylab("Annual SOC change (kg C/ha/year)")+
  xlab("Year")+
  scale_x_discrete(breaks = unique(mod_c$year))+
  geom_text(aes(label = round(mod_soc, digits = 0), y=5000), vjust = 0.5, angle = 90)

mod_c %>% 
  summarize(
    mod_soc = sum(mod_soc)
  ) %>% 
  ggplot(aes(x = "", y = mod_soc)) +  # Use an empty string for x
  geom_col(color = "black", fill = "#009E73")+
  theme_bw()+
  ylab("SOC change (kg C/ha/year)")+
  geom_text(aes(label = round(mod_soc, digits = 0), vjust = 0.5))  # Corrected to use 'label' aesthetic

#####################################

#####################################
######## draft code #################

# code to compare different sources of CO2 data.

#co2_jevans_folder

new_co2 <- read.csv("data/measured_co2/co2_jevans_folder.csv", header = FALSE)
colnames(new_co2) <- new_co2[2, ]
new_co2 <- new_co2[-c(1:3),]
new_co2$nee <- as.numeric(new_co2$NEE)
new_co2$gpp <- as.numeric(new_co2$GPP)
new_co2$resp <- as.numeric(new_co2$Re)
new_co2$year <- as.numeric(new_co2$year)
new_co2$source <- "co2_jevans_other"

data1 <- new_co2 %>%
  filter(doy >120) %>% 
  group_by(year, plot) %>%
  summarise(
    nee = sum(nee),
    gpp = sum(gpp),
    resp = sum(resp)
  ) %>%
  pivot_longer(
    cols = c(nee, gpp, resp),
    names_to = "variable",
    values_to = "value",
  )

data1$source <- "co2_jevans_other"


#co2_jevans_thesis

new_co2_2 <- read.csv("data/measured_co2/co2_jevans_thesis.csv", header = FALSE)
colnames(new_co2_2) <- new_co2_2[2, ]
new_co2_2 <- new_co2_2[-c(1:3),]
new_co2_2$nee <- as.numeric(new_co2_2$NEE_gf)
new_co2_2$gpp <- as.numeric(new_co2_2$Gpp_gf)
new_co2_2$resp <- as.numeric(new_co2_2$Re_gf)
new_co2_2$year <- as.numeric(new_co2_2$year)
new_co2_2$source <- "co2_jevans_thesis"

data2 <- new_co2_2 %>%
  filter(doy >120) %>% 
  group_by(year, plot) %>%
  summarise(
    nee = sum(nee),
    gpp = sum(gpp),
    resp = sum(resp)
  ) %>%
  pivot_longer(
    cols = c(nee, gpp, resp),
    names_to = "variable",
    values_to = "value",
  )

data2$source <- "co2_jevans_thesis"


# initial

obs_co2_p1$plot <- "1"
obs_co2_p3$plot <- "3"
obs_co2_p1 <- obs_co2_p1 %>%
  mutate(doy = yday(date))
obs_co2_p3 <- obs_co2_p3 %>%
  mutate(doy = yday(date))

merged_co2 <- rbind(obs_co2_p1, obs_co2_p3)

new_co2_3 <- merged_co2 %>% 
  mutate(
    resp = obs_resp,
    gpp = obs_gpp,
    nee = obs_gpp,
    source = "initial"
  )

data3 <-merged_co2 %>%
  filter(doy >120) %>% 
  group_by(year, plot) %>%
  summarise(
    nee = sum(obs_nee),
    gpp = sum(obs_gpp),
    re = sum(obs_resp)
  ) %>%
  pivot_longer(
    cols = c(nee, gpp, re),
    names_to = "variable",
    values_to = "value",
  ) 
data3$source <- "initial"
str(data3)
# shannons folder

corn_nn_compiled <- read_excel("Z:/E26/database/Compiled/Corn EC data/corn_nn_compiled_ajo_5_11_24.xlsx")

corn_nn_compiled$nee <- as.numeric(corn_nn_compiled$nee)
corn_nn_compiled$gpp <- as.numeric(corn_nn_compiled$gpp)
corn_nn_compiled$resp <- as.numeric(corn_nn_compiled$resp)
corn_nn_compiled$year <- as.numeric(corn_nn_compiled$year)
corn_nn_compiled$plot <- as.character(corn_nn_compiled$plot)

corn_nn_compiled$source = "corn_nn_shannon"

data4 <- corn_nn_compiled %>%
  filter(doy >120) %>% 
  group_by(year, plot) %>%
  summarise(
    nee = sum(nee),
    gpp = sum(gpp),
    re = sum(resp)
  ) %>%
  pivot_longer(
    cols = c(nee, gpp, re),
    names_to = "variable",
    values_to = "value",
  )

data4$source <- "corn_nn_shannon"

# merging all

merged_df <- rbind(data1, data2, data3, data4)

merged_plot <- merged_df %>% 
  filter(plot == "1") %>% 
  filter(year %in% c(2018:2021)) %>% 
  mutate(variable = factor(variable, levels = c("gpp", "re", "nee"))) %>%  # Set the desired order
  ggplot(aes(x = source, y = value, fill = variable)) +
  geom_col(colour = "black") +
  scale_fill_manual(values = c("gpp" = "#009E73", "re" = "#D55E00", "nee" = "#F0E442")) +  # Set custom colors
  facet_grid(vars(variable), vars(year)) +
  theme_bw() +
  geom_text(aes(label = round(value, digits = 0)), vjust = 0.5) +  # Adjust text position if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  ylab("Annual cumulative C flux (g/m2/year)")

getwd()

ggsave(filename = "merged_plot.png", plot = merged_plot, width = 9, height = 7, dpi = 300)

c("#F0E442", "#D55E00","#CC79A7","#E69F00", "#0072B2", "#009E73","#56B4E9" , "#000000" ,"#999999")


# daily fluxes

# Select only the columns of interest for each dataset
new_co2_subset <- new_co2[, c("year", "doy", "plot", "gpp", "resp", "source")]
new_co2_2_subset <- new_co2_2[, c("year", "doy", "plot", "gpp", "resp", "source")]
new_co2_3_subset <- new_co2_3[, c("year", "doy", "plot", "gpp", "resp", "source")]
corn_nn_compiled_subset <- corn_nn_compiled[, c("year", "doy", "plot", "gpp", "resp", "source")]
# Combine all datasets using rbind
merged_df2 <- rbind(new_co2_subset, new_co2_2_subset, new_co2_3_subset, corn_nn_compiled_subset)
# Check the resulting dataframe
head(merged_df2)


merged_plot2 <- merged_df2 %>%
  filter(plot == "1") %>%   # Filter for plot "1"
  filter(year %in% c(2018:2021)) %>%  # Filter for years 2018 to 2021
  mutate(
    year = factor(year),  # Ensure 'year' is a factor for proper faceting
    doy = as.numeric(doy)  # Convert 'doy' to numeric for proper ordering on x-axis
  ) %>%
  ggplot(aes(x = doy, y = gpp, color = source, group = source)) +  # Group by source for lines
  geom_line(size = 0.9) +  # Plot lines for each source
  facet_wrap(~year, nrow = 4) +  # Facet by year (2018, 2019, 2020, 2021)
  theme_bw() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 90),  # Rotate x-axis labels if needed
    legend.title = element_blank()  # Optional: Remove legend title for clarity
  ) +
  labs(x = "Day of Year (DOY)", y = "GPP (kg C/ha/day)")

ggsave(filename = "merged_plot2.png", plot = merged_plot2, width = 7, height = 9, dpi = 300)


merged_plot3 <- merged_df2 %>%
  filter(plot == "1") %>%   # Filter for plot "1"
  filter(year %in% c(2018:2021)) %>%  # Filter for years 2018 to 2021
  mutate(
    year = factor(year),  # Ensure 'year' is a factor for proper faceting
    doy = as.numeric(doy)  # Convert 'doy' to numeric for proper ordering on x-axis
  ) %>%
  ggplot(aes(x = doy, y = resp, color = source, group = source)) +  # Group by source for lines
  geom_line(size = 0.9) +  # Plot lines for each source
  facet_wrap(~year, nrow = 4) +  # Facet by year (2018, 2019, 2020, 2021)
  theme_bw() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 90),  # Rotate x-axis labels if needed
    legend.title = element_blank()  # Optional: Remove legend title for clarity
  ) +
  labs(x = "Day of Year (DOY)", y = "Resp (kg C/ha/day)")

ggsave(filename = "merged_plot3.png", plot = merged_plot3, width = 7, height = 9, dpi = 300)


###########

# import modeled data for p1

mod_conv_co2 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilC_1.csv", header = FALSE)
colnames(mod_conv_co2) <- mod_conv_co2[2, ]
colnames(mod_conv_co2) <- make.names(colnames(mod_conv_co2), unique = TRUE)
mod_conv_co21 <- mod_conv_co2[-c(1,2),]
mod_conv_co21 <- data.frame(lapply(mod_conv_co21, as.numeric))

mod_conv_co21$date_jul <- mod_conv_co21$Day

mod_conv_co21$mod_resp <- mod_conv_co21$Eco.respiration*1000/10000
mod_conv_co21$mod_gpp <- mod_conv_co21$Photosynthesis*(-1000/10000)
mod_conv_co21$mod_nee <- mod_conv_co21$X.NEE*1000/10000
mod_conv_co21 <- mod_conv_co21 %>%
  filter(Year > 22) %>%
  mutate(year = case_when(
    Year == 23 ~ 2012,
    Year == 24 ~ 2013,
    Year == 25 ~ 2014,
    Year == 26 ~ 2015,
    Year == 27 ~ 2016,
    Year == 28 ~ 2017,
    Year == 29 ~ 2018,
    Year == 30 ~ 2019,
    Year == 31 ~ 2020,
    Year == 32 ~ 2021,
    Year == 33 ~ 2022,
    Year == 34 ~ 2023
  ))

mod_conv_co21$date <- as.Date(paste(mod_conv_co21$year, mod_conv_co21$date_jul), format = "%Y %j")
head(mod_conv_co21)
### graphs 

# gpp
mod_conv_co21$doy <- yday(mod_conv_co21$date)

ggplot(mod_conv_co21, aes(x = doy, y = mod_gpp))+
  geom_line(size = 1, color = "gray")+
  geom_line(data = corn_nn_compiled, aes(x = doy, y = gpp), color = "#009E73", linetype = 1, size = 1)+
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  facet_wrap(year~., nrow = 6, scales = "free_x")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("GPP (kg C/ha/day)")

head(corn_nn_compiled)

###################

#co2 compiled

co2_compiled <- read_excel("data/measured_co2/co2_fluxes_compiled_ajo_12_11_24.xlsx")

co2_compiled %>% 
  ggplot(aes(x = doy, y = obs_nee))+
  geom_line(size = 1, color = "gray")+
  #geom_line(data = corn_nn_compiled, aes(x = doy, y = gpp), color = "#009E73", linetype = 1, size = 1)+
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  #facet_wrap(year~., nrow = 6, scales = "free_x")+
  facet_grid(vars(year), vars(plot)) +
  
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("GPP (kg C/ha/day)")

######################################