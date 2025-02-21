######################################################
########## E26 C fluxes data analysis ################
######################################################

# author:
# date: 

##################################
####### GENERAL ##################

# set wd
getwd()
setwd("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis")

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


# suggested colors for graphs
c("#F0E442", "#D55E00","#CC79A7","#E69F00", "#0072B2", "#009E73","#56B4E9" , "#000000" ,"#999999")

# general functions

# Manual function to calculate NSE
nse <- function(observed, modeled) {
  # Compute NSE
  mean_observed <- mean(observed)
  numerator <- sum((modeled - observed)^2)
  denominator <- sum((observed - mean_observed)^2)
  nse_value <- 1 - (numerator / denominator)
  return(nse_value)
}

# Manual function to calculate Index of Agreement (d)
index_of_agreement <- function(observed, modeled) {
  # Compute Index of Agreement (d)
  mean_observed <- mean(observed)
  numerator <- sum((modeled - observed)^2)
  denominator <- sum((abs(modeled - mean_observed) + abs(observed - mean_observed))^2)
  d_value <- 1 - (numerator / denominator)
  return(d_value)
}

# Function to calculate metrics
calculate_metrics <- function(observed, modeled) {
  
  # Remove NA values
  valid_idx <- !is.na(observed) & !is.na(modeled)
  observed_valid <- observed[valid_idx]
  modeled_valid <- modeled[valid_idx]
  
  # Calculate metrics
  pbias_value <- pbias(observed_valid, modeled_valid)
  nse_value <- nse(observed_valid, modeled_valid)
  rmse_value <- rmse(observed_valid, modeled_valid)
  rrms_value <- (rmse_value / mean(observed_valid)) * 100
  d_value <- index_of_agreement(observed_valid, modeled_valid)
  r2_value <- cor(observed_valid, modeled_valid)^2
  
  # Create a dataset with the results
  metrics_results <- data.frame(
    PBIAS = round(pbias_value, digits = 2),
    NSE = round(nse_value, digits = 2),
    RMSE = round(rmse_value, digits = 2),
    RRMSE_percent = round(rrms_value, digits = 2),
    Index_of_Agreement_d = round(d_value, digits = 2),
    R2 = round(r2_value, digits = 2)
  )
  
  return(metrics_results)
}

# Function to read .txt files for annual yield results (year value in line 7 for conv)

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

###################################

###################################
######## LOADING OBSERVED DATA ####

### import observed data crop yield

obs_yield <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/3_undergrad_christine/Christine Cudmore/1_winter_2025/NECB.xlsx", sheet = "obs_yield")

obs_yield$type <- "obs"
obs_yield$grain_t_ha <- as.numeric(obs_yield$grain_t_ha)
obs_yield$crop_yield_obs_kg_ha <- obs_yield$grain_t_ha*1000

obs_yield_p1 <- filter(obs_yield, plot_new == "1")

# import observed data co2
obs_co2 <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/3_undergrad_christine/Christine Cudmore/1_winter_2025/NECB.xlsx", sheet = "NEE (DNDC) (2)")
#colnames(obs_co2) <- obs_co2[2, ]
#obs_co2 <- obs_co2[-c(1:3),]
obs_co2$obs_nee <- as.numeric(obs_co2$obs_nee)
obs_co2$obs_gpp <- as.numeric(obs_co2$obs_gpp)
obs_co2$obs_resp <- as.numeric(obs_co2$obs_resp)
obs_co2$year <- as.numeric(obs_co2$year)

obs_co2_p1 <- filter(obs_co2, plot == "1")

# graph observed co2 data
head(obs_co2_p1)
obs_co2_p1 %>% 
  ggplot(aes(x = doy, y = obs_nee)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year~., nrow = 4) +
  ylab("NEE")

# obs_co2_p1<- read_excel("data/measured_co2/P1_nondiverse_ObsCO2_2018-2021.xlsx", col_names = TRUE)
# obs_co2_p1$obs_gpp <- obs_co2_p1$obs_gpp*(-1)
# obs_co2_p1$date <- as.Date(obs_co2_p1$date, format = "%Y/%m/%d")
# obs_co2_p1$date_jul <- yday(obs_co2_p1$date)
# obs_co2_p1$year <- year(obs_co2_p1$date)
# 
# obs_co2_p3 <- read_excel("data/measured_co2/P3_Diverse_ObsCO2_2018-2021.xlsx", col_names = TRUE)
# obs_co2_p3$obs_gpp <- obs_co2_p3$obs_gpp*(-1)
# obs_co2_p3$date <- as.Date(obs_co2_p3$date, format = "%Y/%m/%d")
# obs_co2_p3$date_jul <- yday(obs_co2_p3$date)
# obs_co2_p3$year <- year(obs_co2_p3$date)



### import observed data soil temperature

obs_soil_temp <- read_excel("data/measured_soil_temp/e26_temp.xlsx")
obs_soil_temp$date <- as.Date(obs_soil_temp$Date, format = "%m/%d/%Y")
obs_soil_temp$year <- year(obs_soil_temp$date)
obs_soil_temp$month <- month(obs_soil_temp$date)

obs_soil_temp_p1 <- filter(obs_soil_temp, Plot == "P1")
obs_soil_temp_p2 <- filter(obs_soil_temp, Plot == "P2")
obs_soil_temp_p3 <- filter(obs_soil_temp, Plot == "P3")
obs_soil_temp_p4 <- filter(obs_soil_temp, Plot == "P4")

### import observed data soil water
obs_soil_wfps <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/3_undergrad_christine/Christine Cudmore/1_winter_2025/NECB.xlsx", sheet = "obs_vcw")
obs_soil_wfps$doy <- as.numeric(obs_soil_wfps$doy)

obs_soil_wfps_p1 <- filter(obs_soil_wfps, new_plot == 1)
obs_soil_wfps_p1$date <- as.Date(paste(obs_soil_wfps_p1$year, obs_soil_wfps_p1$doy), format = "%Y %j")

obs_soil_wfps_p1 %>% 
  ggplot(aes(x = doy, y = wfps_25)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year~.)
  #facet_grid(cols = vars(year), rows = vars(plot_new),scales = "free_x")

# obs_soil_vwc <- read_excel("data/measured_vwc/e26_vwc.xlsx")
# obs_soil_vwc$date <- as.Date(obs_soil_vwc$date, format = "%m/%d/%Y")
# obs_soil_vwc$year <- year(obs_soil_vwc$date)
# obs_soil_vwc$month <- month(obs_soil_vwc$date)
# 
# obs_soil_vwc_p1 <- filter(obs_soil_vwc, plot == "P1")
# obs_soil_vwc_p2 <- filter(obs_soil_vwc, plot == "P2")
# obs_soil_vwc_p3 <- filter(obs_soil_vwc, plot == "P3")
# obs_soil_vwc_p4 <- filter(obs_soil_vwc, plot == "P4")

# import observed data soil nitrogen

obs_soil_n <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/3_undergrad_christine/Christine Cudmore/1_winter_2025/NECB.xlsx", sheet = "soil_N")
#obs_soil_n$date <- parse_date_time(obs_soil_n$date, orders = c("dmy", "d/m/y", "d/m/Y"))
#obs_soil_n$date <- as.Date(obs_soil_n$date)
#obs_soil_n$doy <- obs_soil_n$date_jul

obs_soil_n_p1 <- filter(obs_soil_n, plot_new == 1)
obs_soil_n_p1$doy <- as.numeric(obs_soil_n_p1$doy)
obs_soil_n_p1$nh4_kg_n_ha_0_15 <- as.numeric(obs_soil_n_p1$nh4_kg_n_ha_0_15)
obs_soil_n_p1$no3_kg_n_ha_0_15 <- as.numeric(obs_soil_n_p1$no3_kg_n_ha_0_15)
obs_soil_n_p1$nh4_kg_n_ha_15_30 <- as.numeric(obs_soil_n_p1$nh4_kg_n_ha_15_30)
obs_soil_n_p1$no3_kg_n_ha_15_30 <- as.numeric(obs_soil_n_p1$no3_kg_n_ha_15_30)

# graph to check observed data for soil N

obs_soil_n_p1 %>% 
  ggplot(aes(x = doy, y = no3_kg_n_ha_0_15)) +
  #ggplot(aes(x = doy, y = no3_kg_n_ha_15_30)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year~.)+
  #facet_grid(cols = vars(year), rows = vars(plot_new),scales = "free_x") +
  ylab("Soil no3 0-15 cm (kg N/ha)")

obs_soil_n_p1 %>% 
  ggplot(aes(x = doy, y = nh4_kg_n_ha_0_15)) +
  #ggplot(aes(x = doy, y = nh4_kg_n_ha_15_30)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year~.)+
  #facet_grid(cols = vars(year), rows = vars(plot_new),scales = "free_x") +
  ylab("Soil nh4 0-15 cm (kg N/ha)")


# import observed data n2o

obs_n2o_p1 <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/1_modeling/1_e26_r_analysis/obs_data/1_e26_long_term_data.xlsx", sheet = "n2o_obs_non_gap_filled")

# obs_n2o$date <- as.Date(obs_n2o$date, format = "%m/%d/%Y")
# obs_n2o$date_jul <- yday(obs_n2o$date)
# obs_n2o$year <- year(obs_n2o$date)

# obs_n2o_p1 <- filter(obs_n2o, obs_n2o$Plot == "P1")
# obs_n2o_p2 <- filter(obs_n2o, obs_n2o$Plot == "P2")
# obs_n2o_p3 <- filter(obs_n2o, obs_n2o$Plot == "P3")
# obs_n2o_p4 <- filter(obs_n2o, obs_n2o$Plot == "P4")

# loading management dates p1

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
colnames(mgmt_dates_p1) <- c("doy", "year")

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
colnames(mgmt_dates_p3) <- c("doy", "year")

# mgmt_dates_p1 <- as.data.frame( do.call( rbind, list(
#   c(1,2018),
#   c(140,2018), # plating
#   c(303,2018), # harvest
#   c(365,2018), # end of year
#   c(1,2019),
#   c(150,2019), # plating
#   c(273,2019), # harvest
#   c(365,2019), # end of year
#   c(1,2020),
#   c(143,2020), # plating
#   c(267,2020), # harvest
#   c(365,2020), # end of year
#   c(1,2021),
#   c(134,2021), # plating
#   c(307,2021), # harvest
#   c(365,2021), # end of year
#   c(1,2022),
#   c(138,2022), # plating
#   c(276,2022), # harvest
#   c(365,2022), # end of year
#   c(1,2023),
#   c(136,2023), # plating
#   c(277,2023), # harvest
#   c(365,2023) # end of year
# )))
# colnames(mgmt_dates_p1) <- c("doy", "year")


fert_dates_p1 <- as.data.frame( do.call( rbind, list(
  c(186,2000), # uan injected
  c(157,2000), # starter
  c(113,2002), #urea broadcasted
  c(183,2003), # uan injected
  c(135,2003), # starter
  c(131,2005), #urea bradocasted
  c(131,2005), #starter
  c(127,2007), #urea bradocasted
  c(127,2007), # starter
  c(126,2008), #urea bradocasted
  c(127,2008), #starter
  c(174,2009), # uan injected
  c(132,2009), # starter
  c(104,2011), # urea broadcasted
  c(292,2011), # manure broadcasted + incorporated
  c(293,2012), # manure broadcasted + incorporated
  c(318,2013), # manure broadcasted + incorporated
  c(178,2014),# urea broadcasted
  c(332,2014), # manure broadcasted + incorporated
  c(132,2015), # urea + NUI broadcasted + incorporated
  c(133,2015), # starter
  c(132,2016), # urea + NUI broadcasted + incorporated
  c(133,2016), # starter
  c(138,2017), # urea + NUI broadcasted + incorporated
  c(139,2017), # starter
  c(137,2018), # urea broadcasted + incorporated
  c(138,2018), # starter
  c(150,2019), # P blend
  c(168,2021) # UAN injected
)))
colnames(fert_dates_p1) <- c("doy", "year")


fert_dates_p1_uan <- as.data.frame( do.call( rbind, list(
  c(186,2000), # uan injected
  c(183,2003), # uan injected
  c(174,2009), # uan injected
  c(168,2021) # UAN injected
)))
colnames(fert_dates_p1_uan) <- c("doy", "year")

fert_dates_p1_urea <- as.data.frame( do.call( rbind, list(
  c(113,2002), #urea broadcasted
  c(131,2005), #urea bradocasted
  c(127,2007), #urea bradocasted
  c(126,2008), #urea bradocasted
  c(104,2011), # urea broadcasted
  c(178,2014),# urea broadcasted
  c(132,2015), # urea + NUI broadcasted + incorporated
  c(132,2016), # urea + NUI broadcasted + incorporated
  c(138,2017), # urea + NUI broadcasted + incorporated
  c(137,2018) # urea broadcasted + incorporated
)))
colnames(fert_dates_p1_urea) <- c("doy", "year")

fert_dates_p1_man <- as.data.frame( do.call( rbind, list(
  c(292,2011), # manure broadcasted + incorporated
  c(293,2012), # manure broadcasted + incorporated
  c(318,2013), # manure broadcasted + incorporated
  c(332,2014) # manure broadcasted + incorporated
)))
colnames(fert_dates_p1_man) <- c("doy", "year")


fert_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(130,2012),
  c(131,2012),
  c(128,2013),
  c(129,2013),
  c(149,2014),
  c(178,2014),
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

# loading weather data.

file_directory <- "data/climate/"
file_list <- list.files(path = file_directory, pattern = "*.txt", full.names = TRUE)
col_names <- c("doy", "temp_min", "temp_max", "prec", "wind", "radi", "hum")

# Read each file, add the year column, and remove columns with only white spaces or "unknown"
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

# precipitation

climate %>% 
  group_by(year) %>% 
  summarise(
    prec = sum(prec)
  ) %>% 
  ggplot(aes(x = year, y = prec))+
  geom_col()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90)
  )


climate %>% 
  ggplot(aes(x = doy, y = prec))+
  geom_point()+
  theme_bw()+
  facet_wrap(year~.)


#################################

###################################
####### CONVENTIONAL ROTATION #####
###################################

###################################
####### Main variables ############
###################################


###### CROP YIELD, CROP C #######

### import modeled data for p3

mod_yield <- read.csv("C:/DNDC/Result/Record/Site/Multi_year_summary.csv", header = FALSE)
mod_yield <- mod_yield[, c(1:21)]
colnames(mod_yield) <- mod_yield[2,]
mod_yield <- mod_yield[-c(1:3),]
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
    RootC1 = as.numeric(RootC1)
  )

crop_types <- mod_yield %>%
  select(Year, Crop1) %>%
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
    cols = c(GrainC1, LeafC1, StemC1, RootC1),
    names_to = c(".value", "crop_type"),
    names_pattern = "(.*)(C[1-1])"
  ) %>%
  mutate(crop_order = as.numeric(gsub("C", "", crop_type))) %>%
  select(Year, crop_order, grain_c = Grain, leaf_c = Leaf, stem_c = Stem, root_c = Root) %>%
  arrange(Year, crop_order)

# Combine with crop types
mod_yield_p1 <- left_join(long_df, crop_types, by = c("Year", "crop_order")) %>%
  replace_na(list(grain_c = 0, leaf_c = 0, stem_c = 0, root_c = 0)) %>%
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
  )) %>% 
  mutate(crop_type = case_when(
    crop_type == 90 ~ "corn",
    crop_type == 2 ~ "winter wheat",
    crop_type == 3 ~ "soybean",
    crop_type == 15  ~ "rye",
    crop_type == 47  ~ "cover crop",
    crop_type == 10  ~ "clover"
  ))
mod_yield_p1$type <- "mod"
mod_yield_p1 <- mod_yield_p1[order(mod_yield_p1$year), ]
mod_yield_p1$crop_yield_mod_kg_ha <- ifelse(mod_yield_p1$crop_type == "corn", mod_yield_p1$grain_c /0.42, ifelse(mod_yield_p1$crop_type == "soybean", mod_yield_p1$grain_c /0.41, ifelse(mod_yield_p1$crop_type == "winter wheat", mod_yield_p1$grain_c /0.40, ifelse(mod_yield_p1$crop_type %in% c("cover crop", "rye", "clover"), c(mod_yield_p1$grain_c + mod_yield_p1$leaf_c + mod_yield_p1$stem_c)/0.41,mod_yield_p1$grain_c /0.41))))

# graph

combined_data <- left_join(mod_yield_p1, obs_yield_p1, by = c("year", "crop_type"))

combined_data %>% 
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha, crop_yield_mod_kg_ha), 
    names_to = "yield_type", 
    values_to = "yield_value"
  ) %>%
  mutate(yield_category = ifelse(grepl("obs", yield_type), "obs", "mod")) %>%
  select(-yield_type) %>%
  #filter(crop_type == "winter wheat") %>% 
  #filter(!crop == 3) %>% 
  ggplot(aes(x = yield_category, y = yield_value, fill = yield_category)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  facet_wrap(year~., , nrow = 1)+
  scale_fill_manual(values = c("obs" = "#009E73", "mod" = "#999999")) +
  #theme_minimal() +
  labs(x = "Data Type", y = "crop yield (kg/ha)")+
  geom_text(aes(label = round(yield_value, 0)), 
            position = position_dodge(width = 0.9), 
            hjust = 1,
            angle = 90)+
  theme_bw()


## statistics

# yield 

metrics_yield_df <- data.frame(
  PBIAS = numeric(0),
  NSE = numeric(0),
  RMSE = numeric(0),
  RRMSE_percent = numeric(0),
  Index_of_Agreement_d = numeric(0),
  R2 = numeric(0)
)

combined_data <- merge(mod_yield_p1, obs_yield_p1, by = c("year", "crop_type"))

modeled <- combined_data$crop_yield_mod_kg_ha
observed <- combined_data$crop_yield_obs_kg_ha

metrics_yield <- calculate_metrics(observed, modeled)
metrics_yield_df <- rbind(metrics_yield_df, metrics_yield)
metrics_yield_df

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

mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
mod_co2_p1$doy <- mod_co2_p1$Day

mod_co2_p1 %>% 
  filter(Year == 23) %>% 
  summarise(
    resp = sum(mod_resp)
  )

### graphs 

# gpp

mod_co2_p1 %>% 
  filter(year>2011) %>% 
  ggplot(aes(x = as.numeric(doy), y = mod_gpp))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(data = obs_co2_p1, aes(x = as.numeric(doy), y = obs_gpp), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("GPP (g C/m2/day)")+
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed") 

# respiration

mod_co2_p1 %>% 
  filter(year>2011) %>% 
  ggplot(aes(x = as.numeric(doy), y = mod_resp)) +
  geom_line(size = 1, color = "darkgray") +
  geom_line(data = obs_co2_p1, aes(x = as.numeric(doy), y = obs_resp), color = "#D55E00", linetype = 1, size = 1, alpha = 0.5) +
  facet_wrap(year ~ ., nrow = 5) +
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  facet_wrap(year ~ ., nrow = 6, scales = "free_x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("Respiration (g C/m2/day)")+
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed")

# nee
mod_co2_p1 %>% 
  filter(year>2011) %>% 
  ggplot(aes(x = as.numeric(doy), y = mod_nee))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(data = obs_co2_p1, aes(x = as.numeric(doy), y = obs_nee), color = "#0072B2", linetype = 1, size = 1, alpha = 0.5)+
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  facet_wrap(year~., nrow = 6, scales = "free_x")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed")


### statistics
merged_data <- merge(mod_co2_p1, obs_co2_p1, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs"))

# gpp

merged_filter <- merged_data %>%
  filter(doy>121 & doy < 335)

predicted <- merged_filter$mod_gpp
observed <- merged_filter$obs_gpp

metrics_gpp <- calculate_metrics(observed, predicted)

# corn
# predicted <- filter(merged_filter, year_mod %in% c(2018,2021))$mod_gpp
# observed <- filter(merged_filter, year_mod %in% c(2018,2021))$obs_gpp
# metrics_gpp <- calculate_metrics(observed, predicted)

# soybeans
# predicted <- filter(merged_filter, year_mod %in% c(2019,2020))$mod_gpp
# observed <- filter(merged_filter, year_mod %in% c(2019,2020))$obs_gpp
# metrics_gpp <- calculate_metrics(observed, predicted)

metrics_gpp_df <- data.frame(
  PBIAS = numeric(0),
  NSE = numeric(0),
  RMSE = numeric(0),
  RRMSE_percent = numeric(0),
  Index_of_Agreement_d = numeric(0),
  R2 = numeric(0)
)

metrics_gpp_df <- rbind(metrics_gpp_df, metrics_gpp)
metrics_gpp_df


# respiration

# metrics_resp_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

predicted_resp <- merged_data$mod_resp
observed_resp <- merged_data$obs_resp
metrics_resp <- calculate_metrics(observed_resp, predicted_resp)
# 
# predicted <- filter(merged_data, year_mod %in% c(2018,2021))$mod_resp
# observed <- filter(merged_data, year_mod %in% c(2018,2021))$obs_resp
# calculate_metrics(observed, predicted)

# predicted <- filter(merged_data, year_mod %in% c(2019,2020))$mod_resp
# observed <- filter(merged_data, year_mod %in% c(2019,2020))$obs_resp
# calculate_metrics(observed, predicted)

# predicted <- filter(merged_filter, year_mod %in% c(2018,2021))$mod_resp
# observed <- filter(merged_filter, year_mod %in% c(2018,2021))$obs_resp
# metrics_resp <- calculate_metrics(observed, predicted)

# predicted <- filter(merged_filter, year_mod %in% c(2019,2020))$mod_resp
# observed <- filter(merged_filter, year_mod %in% c(2019,2020))$obs_resp
# metrics_resp <- calculate_metrics(observed, predicted)

metrics_resp_df <- rbind(metrics_resp_df, metrics_resp)
metrics_resp_df


# nee

# metrics_nee_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

predicted_nee <- merged_filter$mod_nee
observed_nee <- merged_filter$obs_nee
metrics_nee<-calculate_metrics(predicted_nee, observed_nee)

metrics_nee_df <- rbind(metrics_nee_df, metrics_nee)
metrics_nee_df

### analysis of different time scales

# gpp by week

# merged_data %>% 
#   mutate(
#     week = week(date)
#   ) %>% 
#   group_by(week, year_obs) %>% 
#   summarise(
#     weekly_obs_gpp = mean(obs_gpp),
#     weekly_mod_gpp = mean(mod_gpp),
#     weekly_obs_resp = mean(obs_resp),
#     weekly_mod_resp = mean(mod_resp),
#     weekly_obs_nee = mean(obs_nee),
#     weekly_mod_nee = mean(mod_nee)
#   ) %>% 
#   ggplot(aes(x = week, y = weekly_mod_gpp ))+
#   geom_line(size = 0.75, color = "gray")+
#   geom_line(aes(x = week, y = weekly_obs_gpp ), color = "#009E73", size = 0.75)+
#   facet_wrap(year_obs~., nrow = 5)+
#   theme_bw()

# gpp by month

# merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
#   mutate(
#     month = month(date)
#   ) %>% 
#   group_by(month, year.x) %>% 
#   summarise(
#     monthly_obs_gpp = mean(obs_gpp),
#     monthly_mod_gpp = mean(mod_gpp),
#   ) %>% 
#   pivot_longer(
#     cols = c(monthly_obs_gpp, monthly_mod_gpp), 
#     names_to = "measurement", 
#     values_to = "value"
#   ) %>% 
#   mutate(
#     obs_mod = ifelse(measurement == "monthly_mod_gpp", "mod", "obs"),
#     
#   ) %>% 
#   ggplot(aes(x = as.factor(month), y = value, fill = obs_mod)) +
#   geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
#   scale_fill_manual(values = c("mod" = "#999999", "obs" = "#009E73")) +  # Customize fill colors
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
#   #theme(panel.spacing = unit(1, "lines"))+
#   ylab("Monthly av GPP (g C/m2/day)")+
#   xlab("Month")

# gpp by year

merge(mod_co2_p1, obs_co2_p1, by = c("doy", "year")) %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year) %>% 
  summarise(
    # annual_obs_gpp = mean(obs_gpp),
    # annual_mod_gpp = mean(mod_gpp),
    annual_obs_gpp = sum(obs_gpp),
    annual_mod_gpp = sum(mod_gpp),
  ) %>% 
  pivot_longer(
    cols = c(annual_obs_gpp, annual_mod_gpp), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "annual_mod_gpp", "mod", "obs"),
    
  ) %>% 
  ggplot(aes(x = as.factor(year), y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#009E73")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
  #theme(panel.spacing = unit(1, "lines"))+
  #ylab("annual av GPP (kg C/ha/day)")+
  ylab("annual GPP (g C/m2)")+
  xlab("Year")+
  geom_text(aes(label = round(value, digits = 0)), position= position_dodge(width=0.9), vjust = 1 )

# resp by week

# merged_data %>% 
#   mutate(
#     week = week(date)
#   ) %>% 
#   group_by(week, year_obs) %>% 
#   summarise(
#     weekly_obs_resp = mean(obs_resp),
#     weekly_mod_resp = mean(mod_resp),
#   ) %>% 
#   ggplot(aes(x = week, y = weekly_mod_resp ))+
#   geom_line(size = 0.75, color = "gray")+
#   geom_line(aes(x = week, y = weekly_obs_resp ), color = "#D55E00", size = 0.75)+
#   facet_wrap(year_obs~., nrow = 5)+
#   theme_bw()

# resp by month

# merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
#   mutate(
#     month = month(date)
#   ) %>% 
#   group_by(month, year.x) %>% 
#   summarise(
#     monthly_obs_resp = mean(obs_resp),
#     monthly_mod_resp = mean(mod_resp),
#   ) %>% 
#   pivot_longer(
#     cols = c(monthly_obs_resp, monthly_mod_resp), 
#     names_to = "measurement", 
#     values_to = "value"
#   ) %>% 
#   mutate(
#     obs_mod = ifelse(measurement == "monthly_mod_resp", "mod", "obs"),
#     
#   ) %>% 
#   ggplot(aes(x = as.factor(month), y = value, fill = obs_mod)) +
#   geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
#   scale_fill_manual(values = c("mod" = "#999999", "obs" = "#D55E00")) +  # Customize fill colors
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
#   #theme(panel.spacing = unit(1, "lines"))+
#   ylab("Monthly av resp (g C/m2/day)")+
#   xlab("Month")

# resp by year

merge(mod_co2_p1, obs_co2_p1,  by = c("doy", "year")) %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year) %>% 
  summarise(
    # annual_obs_resp = mean(obs_resp),
    # annual_mod_resp = mean(mod_resp),
    annual_obs_resp = sum(obs_resp),
    annual_mod_resp = sum(mod_resp),
  ) %>% 
  pivot_longer(
    cols = c(annual_obs_resp, annual_mod_resp), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "annual_mod_resp", "mod", "obs"),
    
  ) %>% 
  ggplot(aes(x = as.factor(year), y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#D55E00")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
  #theme(panel.spacing = unit(1, "lines"))+
  #ylab("annual av resp (kg C/ha/day)")+
  ylab("annual resp (g C/m2/day)")+
  xlab("Year")+
  geom_text(aes(label = round(value, digits = 0)), position= position_dodge(width=0.9), vjust = 1 )

# nee
sum(obs_co2_p1$obs_nee),
# nee by year

merge(mod_co2_p1, obs_co2_p1, by = c("doy", "year")) %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year) %>% 
  summarise(
    # annual_obs_nee = mean(obs_nee),
    # annual_mod_nee = mean(mod_nee),
    annual_obs_nee = sum(obs_nee),
    annual_mod_nee = sum(mod_nee),
  ) %>% 
  pivot_longer(
    cols = c(annual_obs_nee, annual_mod_nee), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "annual_mod_nee", "mod", "obs"),
    
  ) %>% 
  ggplot(aes(x = as.factor(year), y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#0072B2")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
  #theme(panel.spacing = unit(1, "lines"))+
  #ylab("annual av nee (kg C/ha/day)")+
  ylab("annual nee (g C/m2)")+
  xlab("Year")+
  geom_text(aes(label = round(value, digits = 0)), position= position_dodge(width=0.9), vjust = 1 )





###################################
####### other variables ############
###################################

###### SOIL TEMPERATURE ###########

# import modeled data

mod_conv_soil_temp <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_conv_soil_temp <- mod_conv_soil_temp[, c(1:27)]
colnames(mod_conv_soil_temp) <- mod_conv_soil_temp[3,]
mod_conv_soil_temp <- mod_conv_soil_temp[-c(1:3),]
mod_conv_soil_temp <- data.frame(lapply(mod_conv_soil_temp, as.numeric))
mod_conv_soil_temp$date_jul <- mod_conv_soil_temp$Day

mod_conv_soil_temp <- mod_conv_soil_temp %>%
  filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 7 ~ 2018,
    Year == 8 ~ 2019,
    Year == 9 ~ 2020,
    Year == 10 ~ 2021,
    Year == 11 ~ 2022,
    Year == 12 ~ 2023
  ))

mod_conv_soil_temp$date <- as.Date(paste(mod_conv_soil_temp$year, mod_conv_soil_temp$date_jul), format = "%Y %j")

mod_conv_soil_temp$temp_25 <- (mod_conv_soil_temp$X20cm + mod_conv_soil_temp$X30cm) / 2
mod_conv_soil_temp$temp_55 <- (mod_conv_soil_temp$X50cm + mod_conv_soil_temp$X60cm) / 2
mod_conv_soil_temp$temp_85 <- (mod_conv_soil_temp$X80cm + mod_conv_soil_temp$X90cm) / 2


# graphs for plot 1
combined_data <- merge(obs_soil_temp_p1, mod_conv_soil_temp, by = "date")

# t5

combined_data %>% 
  #filter(year.x == "2018") %>%
  ggplot(aes(x = date, y = X5cm))+
  geom_line(size = 1, color = "gray")+
  geom_line(aes(x = date, y = T_5_c), 
            color = "#D55E00", size = 1, linetype = 3)+
  theme_bw()+
  ylab("Temperature at 5 cm (C)")

# t25

combined_data %>% 
  #filter(year.x == "2018") %>%
  ggplot(aes(x = date, y = temp_25))+
  geom_line(size = 1, color = "gray")+
  geom_line(aes(x = date, y = T_25_c), 
            color = "#009E73", size = 1, linetype = 3)+
  theme_bw()+
  ylab("Temperature at 25 cm (C)")


# t55

combined_data %>% 
  #filter(year.x == "2018") %>%
  ggplot(aes(x = date, y = temp_55))+
  geom_line(size = 1, color = "gray")+
  geom_line(aes(x = date, y = T_55_c), 
            color = "#009E73", size = 1, linetype = 3)+
  theme_bw()+
  ylab("Temperature at 25 cm (C)")


# t85

combined_data %>% 
  #filter(year.x == "2018") %>%
  ggplot(aes(x = date, y = temp_85))+
  geom_line(size = 1, color = "gray")+
  geom_line(aes(x = date, y = T_85_c), 
            color = "#009E73", size = 1, linetype = 3)+
  theme_bw()+
  ylab("Temperature at 25 cm (C)")


### statistics

# temperature 5 cm
observed <- combined_data$t5
modeled <- combined_data$X5cm
calculate_metrics(observed, modeled)

# temperature 25 cm
observed <- combined_data$t25
modeled <- combined_data$temp_25
calculate_metrics(observed, modeled)


###### SOIL WATER (WFPS) ###########

# import modeled data for p1

mod_soil_water_p1 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_soil_water_p1 <- mod_soil_water_p1[, c(1:5, 28:49)]
colnames(mod_soil_water_p1) <- mod_soil_water_p1[3,]
mod_soil_water_p1 <- mod_soil_water_p1[-c(1:3),]
mod_soil_water_p1 <- data.frame(lapply(mod_soil_water_p1, as.numeric))
mod_soil_water_p1$date_jul <- mod_soil_water_p1$Day
mod_soil_water_p1$doy <- mod_soil_water_p1$date_jul
mod_soil_water_p1$wfps_25_mod <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2

mod_soil_water_p1$vwc_5 <- mod_soil_water_p1$X5cm * (1.26/2.65) * 100
mod_soil_water_p1$vwc_25 <- c((mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm)/2 * (1.30/2.65) * 100)
mod_soil_water_p1$vwc_55 <- c((mod_soil_water_p1$X50cm + mod_soil_water_p1$X60cm)/2 * (1.35/2.65) * 100)
mod_soil_water_p1$vwc_85 <- c((mod_soil_water_p1$X80cm + mod_soil_water_p1$X90cm)/2 * (1.40/2.65) * 100)
mod_soil_water_p1$doy <- mod_soil_water_p1$date_jul

mod_soil_water_p1 <- mod_soil_water_p1 %>%
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

mod_soil_water_p1$date <- as.Date(paste(mod_soil_water_p1$year, mod_soil_water_p1$date_jul), format = "%Y %j")


# graphs
soybean_years <- c(2001, 2004, 2006, 2010, 2019, 2020, 2022, 2023)
corn_years <- c(2000,2003,2005,2007,2008,2009,2012,2013,2014,2015,2016,2017,2018,2021)

#combined_data <- merge(obs_soil_wfps_p1, mod_soil_water_p1, by = c("year", "doy"))
combined_data_soil_water_p1 <- left_join(mod_soil_water_p1, obs_soil_wfps_p1, by = "date")
combined_data_soil_water_p1$doy <- combined_data_soil_water_p1$doy.y

combined_data_soil_water_p1$year <- combined_data_soil_water_p1$year.x
# combined_data2 <- merge(obs_soil_vwc_p1, mod_soil_water_p1, by = "date")
# head(mod_soil_water_p1)

# wfps_5

climate_filtered <- climate %>%
  filter(doy > 90 & doy < 335) %>%  # Filter for the range of days of the year (March 31st to Dec 1st)
  filter(year >= 1999 & year <= 2024)

combined_data_soil_water_p1 %>%
  #filter(year %in% corn_years) %>% 
  filter(doy.x > 90 & doy.x < 335) %>%  # Filter for the desired doy range
  ggplot(aes(x = doy.x, y = X5cm * 100)) +
  geom_point(size = 1, color = "gray") +
  geom_point(aes(x = doy.x, y = wfps_5), 
             color = "#D55E00", size = 1, linetype = 3) +
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw() +
  ylab("WFPS at 5 cm (%)") +
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2000:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 17 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  # geom_bar(data = filter(climate_filtered, year %in% corn_years), aes(x = doy, y = prec*17), # 17 is an scaling factor
  #          stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "WFPS at 5 cm (%)", 
    limits = c(0, 100),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./17,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 6, 1)  # Set the secondary axis from 0 to 6 cm
    )
  )

# wfps_25

combined_data %>% 
  #filter(year %in% corn_years) %>% 
  filter(doy.x > 90 & doy.x < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = doy.x, y = wfps_25_mod*100))+
  geom_point(size = 1, color = "gray")+
  geom_point(aes(x = doy.x, y = wfps_25), 
             color = "#009E73", size = 1, linetype = 3)+
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw()+
  ylab("WFPS at 25 cm (%)")+
  geom_vline(data = mgmt_dates_p1, aes(xintercept = doy), color = "red", linetype = "dashed") +
  #geom_vline(data = filter(mgmt_dates_p1, year %in% corn_years), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  # geom_bar(data = filter(climate_filtered, year %in% corn_years), aes(x = doy, y = prec*17), # 15 is an scaling factor
  #          stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "WFPS at 25 cm (%)", 
    limits = c(0, 100),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./17,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 6, 1)  # Set the secondary axis from 0 to 6 cm
    )
  )


# vwc_5

combined_data2 %>% 
  filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = Day, y = vwc_5))+
  geom_point()+
  geom_point(aes(x = Day, y = VWC_5_m3_m3 *100), 
             color = "#D55E00")+
  facet_wrap(year.x ~ ., nrow = 5) +
  theme_bw()

combined_data2 %>% 
  filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = Day, y = vwc_25))+
  geom_point()+
  geom_point(aes(x = Day, y = VWC_25_m3_m3*100), 
             color = "#009E73")+
  facet_wrap(year.x ~ ., nrow = 5) +
  theme_bw()

combined_data2 %>% 
  filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = Day, y = vwc_55))+
  geom_point()+
  geom_point(aes(x = Day, y = VWC_55_m3_m3*100), 
             color = "#009E73")+
  facet_wrap(year.x ~ ., nrow = 5) +
  theme_bw()

combined_data2 %>% 
  filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = Day, y = vwc_85))+
  geom_point()+
  geom_point(aes(x = Day, y = VWC_85_m3_m3*100), 
             color = "#009E73")+
  facet_wrap(year.x ~ ., nrow = 5) +
  theme_bw()

### statistics

# wfps 5 cm
observed <- combined_data$wfps_5
modeled <- combined_data$X5cm*100
calculate_metrics(observed, modeled)



metrics_wfps5_df <- data.frame(
  PBIAS = numeric(0),
  NSE = numeric(0),
  RMSE = numeric(0),
  RRMSE_percent = numeric(0),
  Index_of_Agreement_d = numeric(0),
  R2 = numeric(0)
)

observed_wfps5 <- filter(combined_data, Day > 90 & Day < 335)$wfps_5
modeled_wfps5  <- filter(combined_data, Day > 90 & Day < 335)$X5cm*100
metrics_wfps5 <-calculate_metrics(predicted_wfps5 , observed_wfps5 )

metrics_wfps5 _df <- rbind(metrics_wfps5 _df, metrics_wfps5 )
metrics_wfps5 _df


# wfps 25 cm
observed <- combined_data$wfps_25
modeled <- combined_data$wfps_25_mod*100
calculate_metrics(observed, modeled)

observed <- filter(combined_data, Day > 90 & Day < 335)$wfps_25
modeled <- filter(combined_data, Day > 90 & Day < 335)$wfps_25_mod*100
calculate_metrics(observed, modeled)

# vwc 5 cm
observed <- combined_data2$VWC_5
modeled <- combined_data2$vwc_5/100
calculate_metrics(observed, modeled)

observed <- filter(combined_data2, vwc_5!=0)$VWC_5
modeled <- filter(combined_data2, vwc_5!=0)$vwc_5/100
calculate_metrics(observed, modeled)

# vwc 25 cm
observed <- combined_data2$VWC_25
modeled <- combined_data2$vwc_25/100
calculate_metrics(observed, modeled)

observed <- filter(combined_data2, vwc_25!=0)$VWC_25
modeled <- filter(combined_data2, vwc_25!=0)$vwc_25/100
calculate_metrics(observed, modeled)


###### SOIL NITROGEN ##############

# import modeled data for p1

mod_conv_soil_n <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
combined_names <- paste(mod_conv_soil_n[3, ], mod_conv_soil_n[4, ], sep = "_")
colnames(mod_conv_soil_n) <- combined_names

mod_conv_soil_n <- mod_conv_soil_n[-c(1:4),]
mod_conv_soil_n <- data.frame(lapply(mod_conv_soil_n, as.numeric))

mod_conv_soil_n$date_jul <- mod_conv_soil_n$Day_

mod_conv_soil_n <- mod_conv_soil_n %>%
  filter(Year_ > 10) %>%
  mutate(year = case_when(
    Year_ == 11 ~ 2000,
    Year_ == 12 ~ 2001,
    Year_ == 13 ~ 2002,
    Year_ == 14 ~ 2003,
    Year_ == 15 ~ 2004,
    Year_ == 16 ~ 2005,
    Year_ == 17 ~ 2006,
    Year_ == 18 ~ 2007,
    Year_ == 19 ~ 2008,
    Year_ == 20 ~ 2009,
    Year_ == 21 ~ 2010,
    Year_ == 22 ~ 2011,
    Year_ == 23 ~ 2012,
    Year_ == 24 ~ 2013,
    Year_ == 25 ~ 2014,
    Year_ == 26 ~ 2015,
    Year_ == 27 ~ 2016,
    Year_ == 28 ~ 2017,
    Year_ == 29 ~ 2018,
    Year_ == 30 ~ 2019,
    Year_ == 31 ~ 2020,
    Year_ == 32 ~ 2021,
    Year_ == 33 ~ 2022,
    Year_ == 34 ~ 2023
  ))



mod_conv_soil_n$date <- as.Date(paste(mod_conv_soil_n$year, mod_conv_soil_n$date_jul), format = "%Y %j")
mod_conv_soil_n$doy <- mod_conv_soil_n$date_jul

# mod_conv_soil_n$nh4_0_15 <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
# mod_conv_soil_n$no3_0_15 <- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2

mod_conv_soil_n$no3_0_15  <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
mod_conv_soil_n$nh4_0_15<- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2

mod_conv_soil_n$no3_15_30  <- mod_conv_soil_n$NH4._10.20cm/2 + mod_conv_soil_n$NH4._20.30cm
mod_conv_soil_n$nh4_15_30<- mod_conv_soil_n$X.NO3._10.20cm/2 + mod_conv_soil_n$X.NO3._20.30cm

# plotting soil N for the entire period 2012-2023

mod_conv_soil_n %>%
  #filter(Year_ < 7) %>%
  ggplot(aes(x = doy, y = no3_0_15))+
  geom_line(colour = "#009E73", alphha = 0.75, size = 0.75)+
  geom_point(data = obs_soil_n_p1, aes(x=doy, y=no3_kg_n_ha_0_15), colour = "#009E73", alpha=0.5)+
  geom_point(data = obs_soil_n_p1, aes(x=doy, y=nh4_kg_n_ha_0_15), colour = "#D55E00", alpha=0.5)+
  geom_line(data = mod_conv_soil_n, aes(x=doy, y=nh4_0_15), colour = "#D55E00", alpha=0.75, size = 0.75)+
  
  facet_wrap(year~.)+
  theme_bw()+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2000:2023)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_uan, year %in% c(2000:2023)), aes(xintercept = doy), color = "#56B4E9", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_urea, year %in% c(2000:2023)), aes(xintercept = doy), color = "#999999", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_man, year %in% c(2000:2023)), aes(xintercept = doy), color = "#F0E442", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2000:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  ylab("Soil N (kg N/ha)")

mod_conv_soil_n %>%
  #filter(Year_ < 7) %>%
  ggplot(aes(x = doy, y = no3_15_30))+
  geom_line(colour = "#009E73", alphha = 0.75, size = 0.75)+
  geom_point(data = obs_soil_n_p1, aes(x=doy, y=no3_kg_n_ha_15_30), colour = "#009E73", alpha=0.5)+
  geom_point(data = obs_soil_n_p1, aes(x=doy, y=nh4_kg_n_ha_15_30), colour = "#D55E00", alpha=0.5)+
  geom_line(data = mod_conv_soil_n, aes(x=doy, y=nh4_15_30), colour = "#D55E00", alpha=0.75, size = 0.75)+
  
  facet_wrap(year~.)+
  theme_bw()+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2000:2023)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_uan, year %in% c(2000:2023)), aes(xintercept = doy), color = "#56B4E9", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_urea, year %in% c(2000:2023)), aes(xintercept = doy), color = "#999999", linetype = "dashed", size = 1)+
  geom_vline(data = filter(fert_dates_p1_man, year %in% c(2000:2023)), aes(xintercept = doy), color = "#F0E442", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2000:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  ylab("Soil N (kg N/ha)")


# plot other N losses as reported by dndc

mod_conv_soil_n %>% 
  group_by(year) %>% 
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
  facet_wrap(year~.)+
  theme_bw()+
  ylab("Annual N losses (kg N/ha)")+
  xlab("Pathways")+
  geom_text(aes(label = round(value, digits = 1)))


# graphs

# nh4

merged_data <- merge(mod_conv_soil_n, obs_soil_n_p1, by = "date") %>% 
  select(date, nh4_0_15, nh4_kg_n_ha_0_15, year.x) %>% 
  pivot_longer(
    cols = c(nh4_0_15, nh4_kg_n_ha), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "nh4_0_15", "mod", "obs"),
    date = as.factor(date)
  )

ggplot(merged_data, aes(x = date, y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#D55E00")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year.x ~ ., scales = "free_x", ncol = 6) +  # Keep faceting if needed
  theme(panel.spacing = unit(1, "lines"))+
  ylab("Soil NH4 0-15 cm (kg N/ha)")


# older line graph

ggplot(mod_conv_soil_n, aes(x = doy , y = nh4_0_15))+
  geom_line()+
  geom_point(data = obs_soil_n_p1,
             aes(x = doy, y = nh4_kg_n_ha),
             color = "black",
             fill = "#D55E00",
             shape = 21,
             size = 2.5)+
  facet_wrap(year ~ ., nrow = 3, ncol = 2) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_continuous(
    breaks = seq(min(mod_conv_soil_n$doy), max(mod_conv_soil_n$doy), by = 20),
  )+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4) 



# no3

merged_data <- merge(mod_conv_soil_n, obs_soil_n_p1, by = "date") %>% 
  select(date, no3_0_15, no3_kg_n_ha, year.x) %>% 
  pivot_longer(
    cols = c(no3_0_15, no3_kg_n_ha), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "no3_0_15", "mod", "obs"),
    date = as.factor(date)
  )

ggplot(merged_data, aes(x = date, y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#009E73")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year.x ~ ., scales = "free_x", ncol = 6) +  # Keep faceting if needed
  theme(panel.spacing = unit(1, "lines"))+
  ylab("Soil N03 0-15 cm (kg N/ha)")

# older line graph

ggplot(mod_conv_soil_n, aes(x = doy , y = no3_0_15))+
  geom_line()+
  geom_point(data = obs_soil_n_p1,
             aes(x = doy, y = no3_kg_n_ha),
             color = "black",
             fill = "#009E73",
             shape = 21,
             size = 2.5)+
  facet_wrap(year ~ ., nrow = 3, ncol = 2) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_continuous(
    breaks = seq(min(mod_conv_soil_n$doy), max(mod_conv_soil_n$doy), by = 20)) +
  geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4) 


### statistics

# nh4
combined_data <- merge(obs_soil_n_p1, mod_conv_soil_n, by = "date")
observed <- combined_data$nh4_kg_n_ha
modeled <- combined_data$nh4_0_15
calculate_metrics(observed, modeled)

# no3
combined_data <- merge(obs_soil_n_p1, mod_conv_soil_n, by = "date")
observed <- combined_data$no3_kg_n_ha
modeled <- combined_data$no3_0_15
calculate_metrics(observed, modeled)


###### N2O ######

# import modeled data for p1

mod_n2o_p1 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilN_1.csv", header = FALSE)
colnames(mod_n2o_p1) <- mod_n2o_p1[3, ]
colnames(mod_n2o_p1) <- make.names(colnames(mod_n2o_p1), unique = TRUE)
mod_n2o_p1 <- mod_n2o_p1[-c(1,2, 3, 4),]
mod_n2o_p1 <- data.frame(lapply(mod_n2o_p1, as.numeric))

mod_n2o_p1$doy <- mod_n2o_p1$Day
mod_n2o_p1$n2o_flux_g_n_ha_d <- mod_n2o_p1$N2O.flux*1000

mod_n2o_p1 <- mod_n2o_p1 %>%
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


# graphs 
obs_n2o_p1$n2o_n_g_ha_day <- as.numeric(obs_n2o_p1$n2o_n_g_ha_day)

ggplot(data = filter(mod_n2o_p1, year %in% c(2015:2023)), aes(x = doy, y = n2o_flux_g_n_ha_d)) +
  geom_line(size = 1, color = "darkgray") +
  geom_line(data = filter(obs_n2o_p1, year %in% c(2015:2023)), aes(x = doy, y = n2o_n_g_ha_day), color = "#0072B2", size = 1, alpha = 0.6) +
  facet_wrap(year ~ .) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("N2O emissions (g N/ha/day)")+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2015:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
  # geom_point(data = filter(combined_data_soil_water_p1, year > 2015, doy > 90 & doy < 335), aes(x = doy, y = X5cm * 100*16), size = 1, color = "gray") +
  # geom_point(data = filter(combined_data_soil_water_p1, year > 2015, doy > 90 & doy < 335), aes(x = doy, y =wfps_5*16), size = 1, color = "#D55E00", alpha=0.5)+
  # geom_line(data = filter(mod_conv_soil_n, year %in% c(2018:2023)), aes(x = doy, y = no3_0_15*20), color = "black")+
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2015:2023)), aes(xintercept = doy, y = 500), color = "blue", size = 4, shape = 4)+
  scale_y_continuous(
    sec.axis = sec_axis(
      trans = ~ ./16,  # No scaling needed, precipitation is already in cm
      name = "WPFPS (%)",
      breaks = seq(0, 100, 25)  # Set the secondary axis from 0 to 6 cm
    )
  )

# statistics 
merged_data <- merge(mod_n2o_p1, obs_n2o_p1, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs"))


metrics_n2o_df <- data.frame(
  PBIAS = numeric(0),
  NSE = numeric(0),
  RMSE = numeric(0),
  RRMSE_percent = numeric(0),
  Index_of_Agreement_d = numeric(0),
  R2 = numeric(0)
)

predicted_n2o <- merged_data$n2o_flux_g_n_ha_d
observed_n2o <- merged_data$n2o_n_g_ha_day
metrics_n2o <-calculate_metrics(predicted_n2o , observed_n2o )

metrics_n2o_df <- rbind(metrics_n2o_df, metrics_n2o )
metrics_n2o_df



metrics <- calculate_metrics(observed, predicted)

mod_n2o_p1

# aggregated sums:

# Step 1: Merge datasets by "year" and "doy"
merged_n2o <- merge(mod_n2o_p1, obs_n2o_p1, by = c("year", "doy"), all = TRUE, 
                     suffixes = c("_mod", "_obs"))

# Step 2: Aggregate daily fluxes to annual sums
annual_sums <- merged_n2o %>%
  group_by(year) %>%
  summarise(
    n2o_mod = sum(n2o_flux_g_n_ha_d, na.rm = TRUE),
    n2o_obs = sum(n2o_n_g_ha_day, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(n2o_mod, n2o_obs), names_to = "source", values_to = "total_n2o")

# Step 3: Create the bar chart
ggplot(annual_sums, aes(x = source, y = total_n2o, fill = source)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  facet_wrap(~year, nrow = 2) +
  labs(x = "Source", y = "Annual N₂O Emissions (g N ha⁻¹ yr⁻¹)", fill = "Data Source") +
  theme_bw()+
  theme(
    axis.text.x = element_blank()
  )+
  scale_fill_manual(values = c("n2o_obs" = "#009E73", "n2o_mod" = "#999999"))+
  geom_text(aes(label = round(total_n2o, 0)), 
            position = position_dodge(width = 0.9), 
            angle = 90, hjust = 0.5)


###################################