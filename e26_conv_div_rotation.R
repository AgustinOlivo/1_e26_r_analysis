#########################################################################
########## R script - Conventional vs Diverse Rotation Project  ###########
#########################################################################

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

#########################

##################################
######## LOADING OBSERVED DATA ####

### import observed data crop yield

obs_yield <- read_excel("obs_data/measured_crop/obs_yield_e26_2025_02_06.xlsx", sheet = "obs_yield_main")
obs_yield$type <- "obs"
obs_yield$grain_t_ha <- as.numeric(obs_yield$grain_t_ha)
obs_yield$crop_yield_obs_kg_ha <- obs_yield$grain_t_ha*1000

# separating data by plot

obs_yield_p1 <- filter(obs_yield, plot == "1")
obs_yield_p2 <- filter(obs_yield, plot == "2")
obs_yield_p3 <- filter(obs_yield, plot == "3")
obs_yield_p4 <- filter(obs_yield, plot == "4")

### import observed data soil temperature

obs_soil_temp <- read_excel("obs_data/measured_soil_temp/e26_temp.xlsx")
obs_soil_temp$date <- as.Date(obs_soil_temp$Date, format = "%m/%d/%Y")
obs_soil_temp$year <- year(obs_soil_temp$date)
obs_soil_temp$month <- month(obs_soil_temp$date)

# separating data by plot

obs_soil_temp_p1 <- filter(obs_soil_temp, Plot == "P1")
obs_soil_temp_p2 <- filter(obs_soil_temp, Plot == "P2")
obs_soil_temp_p3 <- filter(obs_soil_temp, Plot == "P3")
obs_soil_temp_p4 <- filter(obs_soil_temp, Plot == "P4")

### import observed data soil water (water-filled pore space and volumetric water content)
obs_soil_wfps <- read.csv("obs_data/measured_wfps/wfps_5_25.csv", header = TRUE)
obs_soil_wfps$date <- as.Date(obs_soil_wfps$date, format = "%m/%d/%Y")
obs_soil_wfps$year <- year(obs_soil_wfps$date)
obs_soil_wfps$month <- month(obs_soil_wfps$date)

# separating data by plot

obs_soil_wfps_p1 <- filter(obs_soil_wfps, plot == "P1")
obs_soil_wfps_p2 <- filter(obs_soil_wfps, plot == "P2")
obs_soil_wfps_p3 <- filter(obs_soil_wfps, plot == "P3")
obs_soil_wfps_p4 <- filter(obs_soil_wfps, plot == "P4")

obs_soil_vwc <- read_excel("obs_data/measured_vwc/e26_vwc.xlsx")
obs_soil_vwc$date <- as.Date(obs_soil_vwc$date, format = "%m/%d/%Y")
obs_soil_vwc$year <- year(obs_soil_vwc$date)
obs_soil_vwc$month <- month(obs_soil_vwc$date)

# separating data by plot

obs_soil_vwc_p1 <- filter(obs_soil_vwc, plot == "P1")
obs_soil_vwc_p2 <- filter(obs_soil_vwc, plot == "P2")
obs_soil_vwc_p3 <- filter(obs_soil_vwc, plot == "P3")
obs_soil_vwc_p4 <- filter(obs_soil_vwc, plot == "P4")

# import observed data soil nitrogen

obs_soil_n <- read.csv("obs_data/measured_nh4_no3/E26_Nh4_NO3_dndc.csv", header = TRUE)
obs_soil_n$date <- parse_date_time(obs_soil_n$date, orders = c("dmy", "d/m/y", "d/m/Y"))
obs_soil_n$date <- as.Date(obs_soil_n$date)
obs_soil_n$doy <- obs_soil_n$date_jul

# separating data by plot

obs_soil_n_p1 <- filter(obs_soil_n, plot == "P1" & year > 2017)
obs_soil_n_p2 <- filter(obs_soil_n, plot == "P2" & year > 2017)
obs_soil_n_p3 <- filter(obs_soil_n, plot == "P3" & year > 2017)
obs_soil_n_p4 <- filter(obs_soil_n, plot == "P4" & year > 2017)

# separating data by plot

obs_soil_n_p1_long <- filter(obs_soil_n, plot == "P1")
obs_soil_n_p2_long <- filter(obs_soil_n, plot == "P2")
obs_soil_n_p3_long <- filter(obs_soil_n, plot == "P3")
obs_soil_n_p4_long <- filter(obs_soil_n, plot == "P4")

# graph to check observed data for soil N

# obs_soil_n %>% 
#   ggplot(aes(x = doy, y = nh4_kg_n_ha)) +
#   geom_col(color = "black") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_grid(cols = vars(year), rows = vars(plot)) +
#   ylab("Soil NH4 0-15 cm (kg N/ha)")

# import observed data co2
obs_co2 <- read.csv("obs_data/measured_co2/co2_jevans_folder.csv", header = FALSE)
colnames(obs_co2) <- obs_co2[2, ]
obs_co2 <- obs_co2[-c(1:3),]
obs_co2$obs_nee <- as.numeric(obs_co2$NEE)
obs_co2$obs_gpp <- as.numeric(obs_co2$GPP)
obs_co2$obs_resp <- as.numeric(obs_co2$Re)
obs_co2$year <- as.numeric(obs_co2$year)
obs_co2$date <- as.Date(paste(obs_co2$year, obs_co2$mm, obs_co2$dd, sep = "-"))

obs_co2_p1 <- filter(obs_co2, plot == "1")
obs_co2_p3 <- filter(obs_co2, plot == "3")
obs_co2_p3$doy <- as.numeric(obs_co2_p3$doy )

# graph observed co2 data

# head(obs_co2_p1)
# obs_co2_p1 %>% 
#   ggplot(aes(x = doy, y = obs_nee)) +
#   geom_point(color = "black") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   facet_wrap(year~., nrow = 4) +
#   ylab("NEE")

# import observed data n2o

obs_n2o_p1p2 <- read_excel("obs_data/measured_n2o/flux_grad_p1p2_2018_2024_n2o.xlsx")
obs_n2o_p3p4 <- read_excel("obs_data/measured_n2o/flux_grad_p3p4_2018_2024_n2o.xlsx")

# loading management dates current P1 and P3 to include in graphs

mgmt_dates_p1 <- as.data.frame( do.call( rbind, list(
  c(131,2012), # planting
  c(276,2012), # harvest
  c(129,2013), # planting
  c(295,2013), # harvest
  c(146,2014), # planting
  c(308,2014), # harvest
  c(133,2015), # planting
  c(300,2015), # harvest
  c(133,2016), # planting
  c(280,2016), # harvest
  c(138,2017), # planting
  c(313,2017), # harvest
  c(140,2018), # planting
  c(303,2018), # harvest
  c(150,2019), # planting
  c(273,2019), # harvest
  c(143,2020), # planting
  c(267,2020), # harvest
  c(134,2021), # planting
  c(307,2021), # harvest
  c(138,2022), # planting
  c(276,2022), # harvest
  c(136,2023), # planting
  c(277,2023) # harvest
)))
colnames(mgmt_dates_p1) <- c("doy", "year")

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
  c(131,2012), # planting
  c(276,2012), # harvest
  c(129,2013), # planting
  c(295,2013), # harvest
  c(146,2014), # planting
  c(308,2014), # harvest
  c(133,2015), # planting
  c(300,2015), # harvest
  c(133,2016), # planting
  c(280,2016), # harvest
  c(138,2017), # planting
  c(313,2017), # harvest
  c(140,2018), # planting
  c(303,2018), # harvest
  c(150,2019), # planting
  c(273,2019), # harvest
  c(213,2020), # harvest
  c(134,2021), # planting
  c(307,2021), # harvest
  c(138,2022), # planting
  c(276,2022), # harvest
  c(221,2023) # harvest
)))
colnames(mgmt_dates_p3) <- c("doy", "year")

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
col_names <- c("doy", "temp_min", "temp_max", "prec", "wind", "radi", "hum")

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

# climate %>% 
#   group_by(year) %>% 
#   summarise(
#     prec = sum(prec)
#   ) %>% 
#   ggplot(aes(x = year, y = prec))+
#   geom_col()+
#   theme_bw()

#################################

###################################
####### CONVENTIONAL ROTATION #####
###################################

# loading data from DNDC ouputs related to the conventional rotation in my project, running statistics, and creating graphs (all by variable). 

###### CROP YIELD, CROP C #######

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

combined_data <- merge(obs_yield_p1, mod_yield_p1, by = c("year", "crop_type"))

# yield 

combined_data %>% 
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha, crop_yield_mod_kg_ha), 
    names_to = "yield_type", 
    values_to = "yield_value"
  ) %>%
  mutate(yield_category = ifelse(grepl("obs", yield_type), "obs", "mod")) %>%
  select(-yield_type) %>% 
  ggplot(aes(x = yield_category, y = yield_value, fill = yield_category)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  facet_grid(cols = vars(year), rows = vars(crop_type)) +
  scale_fill_manual(values = c("obs" = "#009E73", "mod" = "#999999")) +
  #theme_minimal() +
  labs(x = "Data Type", y = "crop yield (kg/ha)")+
  geom_text(aes(label = round(yield_value, 0)), 
            position = position_dodge(width = 0.9), 
            vjust = 1)+
  theme_bw()

## statistics

# yield 

# metrics_yield_df <- data.frame( # data frame to store results from multiple runs
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

modeled <- combined_data$crop_yield_mod_kg_ha
observed <- combined_data$crop_yield_obs_kg_ha
metrics_yield <- calculate_metrics(observed, modeled)

metrics_yield_df <- rbind(metrics_yield_df, metrics_yield)
metrics_yield_df

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
observed <- combined_data$T_5_c
modeled <- combined_data$X5cm
calculate_metrics(observed, modeled)

# temperature 25 cm
observed <- combined_data$T_25_c
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
mod_soil_water_p1$wfps_25_mod <- (mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm) / 2

mod_soil_water_p1$vwc_5 <- mod_soil_water_p1$X5cm * (1.26/2.65) * 100
mod_soil_water_p1$vwc_25 <- c((mod_soil_water_p1$X20cm + mod_soil_water_p1$X30cm)/2 * (1.30/2.65) * 100)
mod_soil_water_p1$vwc_55 <- c((mod_soil_water_p1$X50cm + mod_soil_water_p1$X60cm)/2 * (1.35/2.65) * 100)
mod_soil_water_p1$vwc_85 <- c((mod_soil_water_p1$X80cm + mod_soil_water_p1$X90cm)/2 * (1.40/2.65) * 100)
mod_soil_water_p1$doy <- mod_soil_water_p1$date_jul

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

# graphs
combined_data_water <- merge(obs_soil_wfps_p1, mod_soil_water_p1, by = "date")
combined_data_water$year <- combined_data_water$year.x

# wfps_5

climate_filtered <- climate %>%
  filter(doy > 90 & doy < 335) %>%  # Filter for the range of days of the year (March 31st to Dec 1st)
  filter(year >= 2018 & year <= 2022)

combined_data_water %>% 
  filter(doy > 90 & doy < 335) %>%  # Filter for the desired doy range
  ggplot(aes(x = doy, y = X5cm * 100)) +
  geom_point(size = 1, color = "gray") +
  geom_point(aes(x = doy, y = wfps_5), 
             color = "#D55E00", size = 1, linetype = 3) +
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw() +
  ylab("WFPS at 5 cm (%)") +
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
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

combined_data_water %>% 
  filter(doy > 90 & doy < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = doy, y = wfps_25_mod*100))+
  geom_point(size = 1, color = "gray")+
  geom_point(aes(x = doy, y = wfps_25), 
            color = "#009E73", size = 1, linetype = 3)+
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw()+
  ylab("WFPS at 25 cm (%)")+
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
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

# combined_data2 %>% 
#   filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
#   ggplot(aes(x = Day, y = vwc_5))+
#   geom_point()+
#   geom_point(aes(x = Day, y = VWC_5_m3_m3 *100), 
#              color = "#D55E00")+
#   facet_wrap(year.x ~ ., nrow = 5) +
#   theme_bw()
# 
# combined_data2 %>% 
#   filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
#   ggplot(aes(x = Day, y = vwc_25))+
#   geom_point()+
#   geom_point(aes(x = Day, y = VWC_25_m3_m3*100), 
#              color = "#009E73")+
#   facet_wrap(year.x ~ ., nrow = 5) +
#   theme_bw()
# 
# combined_data2 %>% 
#   filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
#   ggplot(aes(x = Day, y = vwc_55))+
#   geom_point()+
#   geom_point(aes(x = Day, y = VWC_55_m3_m3*100), 
#              color = "#009E73")+
#   facet_wrap(year.x ~ ., nrow = 5) +
#   theme_bw()
# 
# combined_data2 %>% 
#   filter(Day > 90 & Day < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
#   ggplot(aes(x = Day, y = vwc_85))+
#   geom_point()+
#   geom_point(aes(x = Day, y = VWC_85_m3_m3*100), 
#              color = "#009E73")+
#   facet_wrap(year.x ~ ., nrow = 5) +
#   theme_bw()

### statistics

# wfps 5 cm

# observed <- combined_data$wfps_5
# modeled <- combined_data$X5cm*100
# calculate_metrics(observed, modeled)

# metrics_wfps5_df <- data.frame( 
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_water, Day > 90 & Day < 335)$wfps_5
modeled <- filter(combined_data_water, Day > 90 & Day < 335)$X5cm*100
metrics_wfps5 <- calculate_metrics(observed, modeled)

metrics_wfps5_df <- rbind(metrics_wfps5_df, metrics_wfps5)
metrics_wfps5_df

# wfps 25 cm

# observed <- combined_data$wfps_25
# modeled <- combined_data$wfps_25_mod*100
# calculate_metrics(observed, modeled)

# metrics_wfps25_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_water, Day > 90 & Day < 335)$wfps_25
modeled <- filter(combined_data_water, Day > 90 & Day < 335)$wfps_25_mod*100
metrics_wfps25 <- calculate_metrics(observed, modeled)

metrics_wfps25_df <- rbind(metrics_wfps25_df, metrics_wfps25)
metrics_wfps25_df

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


# plotting soil N for the entire period 2012-2023

mod_conv_soil_n %>%
  #filter(year > 2017) %>%
  ggplot(aes(x = doy, y = no3_0_15))+
  geom_line(colour = "#009E73", alphha = 0.75, size = 0.75)+
  geom_point(data = filter(obs_soil_n_p1_long, year %in% c(2008:2023)), aes(x=doy, y=no3_kg_n_ha), colour = "#009E73", alpha=0.5)+
  geom_point(data = filter(obs_soil_n_p1_long, year %in% c(2008:2023)), aes(x=doy, y=nh4_kg_n_ha), colour = "#D55E00", alpha=0.5)+
  geom_line(data = filter(mod_conv_soil_n, year > 2008), aes(x=doy, y=nh4_0_15), colour = "#D55E00", alpha=0.75, size = 0.75)+
  
  facet_wrap(year~.)+
  theme_bw()+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2008:2023)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2008:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
  ylab("Soil N (kg N/ha)")

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
  #filter(year>2017) %>% 
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
# 
# 
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

combined_data <- merge(mod_conv_soil_n, obs_soil_n_p1_long, by = c("year", "doy"))
combined_data <- combined_data %>% 
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

observed <- combined_data$nh4_kg_n_ha
modeled <- combined_data$nh4_0_15

metrics_nh4_0_15<- calculate_metrics(observed, modeled)

metrics_nh4_0_15_df <- rbind(metrics_nh4_0_15_df, metrics_nh4_0_15)
metrics_nh4_0_15_df


# no3

# metrics_no3_0_15_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- combined_data$no3_kg_n_ha
modeled <- combined_data$no3_0_15

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
  filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 7 ~ 2018,
    Year == 8 ~ 2019,
    Year == 9 ~ 2020,
    Year == 10 ~ 2021,
    Year == 11 ~ 2022,
    Year == 12 ~ 2023,
    Year == 1 ~ 2024
    
  ))

mod_co2_p1$date <- as.Date(paste(mod_co2_p1$year, mod_co2_p1$date_jul), format = "%Y %j")
mod_co2_p1$doy <- as.numeric(mod_co2_p1$Day)

### graphs 

# gpp
mod_co2_p1 %>% 
  filter(year<2022) %>% 
  ggplot(aes(x = as.numeric(doy), y = mod_gpp))+
  geom_line(size = 1, color = "darkgray")+
  geom_line(data = obs_co2_p1, aes(x = as.numeric(doy), y = obs_gpp), color = "#009E73", linetype = 1, size = 1, alpha = 0.5)+
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  facet_wrap(year~., nrow = 6, scales = "free_x")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("GPP (g C/m2/day)")+
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2021)), aes(xintercept = doy), color = "red", linetype = "dashed") 
#asdasd

# respiration

ggplot(data = mod_co2_p1, aes(x = as.numeric(doy), y = mod_resp)) +
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
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2023)), aes(xintercept = doy), color = "red", linetype = "dashed")

# nee

ggplot(mod_co2_p1, aes(x = as.numeric(doy), y = mod_nee))+
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
merged_data <- merge(mod_co2_p1, obs_co2_p1, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))

# gpp


predicted <- merged_data$mod_gpp
observed <- merged_data$obs_gpp

metrics_gpp <- calculate_metrics(observed, predicted)
# corn
# predicted <- filter(merged_filter, year_mod %in% c(2018,2021))$mod_gpp
# observed <- filter(merged_filter, year_mod %in% c(2018,2021))$obs_gpp
# metrics_gpp <- calculate_metrics(observed, predicted)

# soybeans
# predicted <- filter(merged_filter, year_mod %in% c(2019,2020))$mod_gpp
# observed <- filter(merged_filter, year_mod %in% c(2019,2020))$obs_gpp
# metrics_gpp <- calculate_metrics(observed, predicted)

# metrics_gpp_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

metrics_gpp_df <- rbind(metrics_gpp_df, metrics_gpp)
metrics_gpp_df


# respiration

predicted <- merged_data$mod_resp
observed <- merged_data$obs_resp
metrics_resp <- calculate_metrics(observed, predicted)
 
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

# metrics_resp_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

metrics_resp_df <- rbind(metrics_resp_df, metrics_resp)
metrics_resp_df


# nee

predicted <- merged_data$mod_nee
observed <- merged_data$obs_nee
metrics_nee <- calculate_metrics(observed, predicted)

# metrics_nee_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

metrics_nee_df <- rbind(metrics_nee_df, metrics_nee)
metrics_nee_df

### analysis of different time scales

# gpp by week

merged_data %>% 
  mutate(
    week = week(date)
  ) %>% 
  group_by(week, year_obs) %>% 
  summarise(
    weekly_obs_gpp = mean(obs_gpp),
    weekly_mod_gpp = mean(mod_gpp),
    weekly_obs_resp = mean(obs_resp),
    weekly_mod_resp = mean(mod_resp),
    weekly_obs_nee = mean(obs_nee),
    weekly_mod_nee = mean(mod_nee)
  ) %>% 
  ggplot(aes(x = week, y = weekly_mod_gpp ))+
  geom_line(size = 0.75, color = "gray")+
  geom_line(aes(x = week, y = weekly_obs_gpp ), color = "#009E73", size = 0.75)+
  facet_wrap(year_obs~., nrow = 5)+
  theme_bw()

# gpp by month

merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
  mutate(
    month = month(date)
  ) %>% 
  group_by(month, year.x) %>% 
  summarise(
    monthly_obs_gpp = mean(obs_gpp),
    monthly_mod_gpp = mean(mod_gpp),
  ) %>% 
  pivot_longer(
    cols = c(monthly_obs_gpp, monthly_mod_gpp), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "monthly_mod_gpp", "mod", "obs"),
    
  ) %>% 
  ggplot(aes(x = as.factor(month), y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#009E73")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
  #theme(panel.spacing = unit(1, "lines"))+
  ylab("Monthly av GPP (g C/m2/day)")+
  xlab("Month")

# gpp by year

merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year, year.x) %>% 
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

merged_data %>% 
  mutate(
    week = week(date)
  ) %>% 
  group_by(week, year_obs) %>% 
  summarise(
    weekly_obs_resp = mean(obs_resp),
    weekly_mod_resp = mean(mod_resp),
  ) %>% 
  ggplot(aes(x = week, y = weekly_mod_resp ))+
  geom_line(size = 0.75, color = "gray")+
  geom_line(aes(x = week, y = weekly_obs_resp ), color = "#D55E00", size = 0.75)+
  facet_wrap(year_obs~., nrow = 5)+
  theme_bw()

# resp by month

merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
  mutate(
    month = month(date)
  ) %>% 
  group_by(month, year.x) %>% 
  summarise(
    monthly_obs_resp = mean(obs_resp),
    monthly_mod_resp = mean(mod_resp),
  ) %>% 
  pivot_longer(
    cols = c(monthly_obs_resp, monthly_mod_resp), 
    names_to = "measurement", 
    values_to = "value"
  ) %>% 
  mutate(
    obs_mod = ifelse(measurement == "monthly_mod_resp", "mod", "obs"),
    
  ) %>% 
  ggplot(aes(x = as.factor(month), y = value, fill = obs_mod)) +
  geom_col(position = "dodge", color = "black") +  # Use position = "dodge" to place bars side by side
  scale_fill_manual(values = c("mod" = "#999999", "obs" = "#D55E00")) +  # Customize fill colors
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(year.x ~ ., ncol = 6) +  # Keep faceting if needed
  #theme(panel.spacing = unit(1, "lines"))+
  ylab("Monthly av resp (g C/m2/day)")+
  xlab("Month")

# resp by year

merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year, year.x) %>% 
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

# nee by year

merge(mod_co2_p1, obs_co2_p1, by = "date") %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(year, year.x) %>% 
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

# graphs 
mod_n2o_p1 %>%
  filter(year %in% c(2018:2024)) %>% 
  ggplot(aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_d)) +
  geom_line(size = 1, color = "darkgray") +
  geom_line(data = filter(obs_n2o_p1p2, year < 2025), aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_day), color = "#0072B2", size = 1, alpha = 0.6) +
  facet_wrap(year ~ ., ncol = 2) +
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("N2O emissions (g N/ha/day)")+
  geom_vline(data = filter(fert_dates_p1, year %in% c(2018:2024)), aes(xintercept = doy), color = "darkred", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p1, year %in% c(2018:2024)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)
  # geom_point(data = combined_data_water, aes(x = doy, y = X5cm * 100), size = 1, color = "blue", alpha = 0.5)+
  # geom_point(data = combined_data_water, aes(x = doy, y = wfps_5), size = 1, color = "#D55E00", alpha = 0.5)

combined_data_water$year = combined_data_water$year.x


# statistics 
merged_data <- merge(mod_n2o_p1, obs_n2o_p1p2, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs"))

predicted <- merged_data$n2o_flux_g_n_ha_d
observed <- merged_data$n2o_flux_g_n_ha_day
metrics_n2o <- calculate_metrics(observed, predicted)

metrics_n2o_df <- data.frame(
  PBIAS = numeric(0),
  NSE = numeric(0),
  RMSE = numeric(0),
  RRMSE_percent = numeric(0),
  Index_of_Agreement_d = numeric(0),
  R2 = numeric(0)
)

metrics_n2o_df <- rbind(metrics_n2o_df, metrics_n2o)
metrics_n2o_df

###################################

###################################
####### DIVERSE ROTATION ##########
###################################

# loading data from DNDC ouputs related to the diverse rotation in my project, running statistics, and creating graphs (all by variable). 

###### CROP YIELD, CROP C #######

### import observed data crop yield specifically for p3 as a few things are different

obs_yield <- read_excel("obs_data/measured_crop/obs_yield_e26_2025_02_06.xlsx", sheet = "obs_yield_cc")
obs_yield$type <- "obs"
obs_yield$grain_t_ha = as.numeric(obs_yield$grain_t_ha)
obs_yield$crop_yield_obs_kg_ha <- obs_yield$grain_t_ha*1000
obs_yield_p3 <- filter(obs_yield, plot == "3")

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
  mutate(across(everything(), ~ ifelse(. == "rye", "cover crop", .)))

# graph

combined_data <- merge(obs_yield_p3, mod_yield_p3, by = c("year", "crop_type", "crop_order"))

combined_data %>% 
  pivot_longer(
    cols = c(crop_yield_obs_kg_ha, crop_yield_mod_kg_ha), 
    names_to = "yield_type", 
    values_to = "yield_value"
  ) %>%
  mutate(yield_category = ifelse(grepl("obs", yield_type), "obs", "mod")) %>%
  select(-yield_type) %>% 
  #filter(!crop == 3) %>% 
  ggplot(aes(x = yield_category, y = yield_value, fill = yield_category)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  facet_grid(cols = vars(year), rows = vars(crop_order)) +
  #facet_nested(crop ~ year+ crop_type)
  scale_fill_manual(values = c("obs" = "#009E73", "mod" = "#999999")) +
  #theme_minimal() +
  labs(x = "Data Type", y = "crop yield (kg/ha)")+
  geom_text(aes(label = round(yield_value, 0)), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5)+
  theme_bw()

## statistics

# yield 

# metrics_yield_df <- data.frame( # data frame to store results from multiple runs
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

modeled <- combined_data$crop_yield_mod_kg_ha
observed <- combined_data$crop_yield_obs_kg_ha
metrics_yield <- calculate_metrics(observed, modeled)

metrics_yield_df <- rbind(metrics_yield_df, metrics_yield)
metrics_yield_df

###### SOIL TEMPERATURE ###########

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
combined_data <- merge(obs_soil_temp_p3, mod_conv_soil_temp, by = "date")

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
observed <- combined_data$T_5_c
modeled <- combined_data$X5cm
calculate_metrics(observed, modeled)

# temperature 25 cm
observed <- combined_data$T_25_c
modeled <- combined_data$temp_25
calculate_metrics(observed, modeled)


###### SOIL WATER (WFPS) ###########

mod_soil_water_p3 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilClimate_1.csv", header = FALSE)
mod_soil_water_p3 <- mod_soil_water_p3[, c(1:5, 28:49)]
colnames(mod_soil_water_p3) <- mod_soil_water_p3[3,]
mod_soil_water_p3 <- mod_soil_water_p3[-c(1:3),]
mod_soil_water_p3 <- data.frame(lapply(mod_soil_water_p3, as.numeric))
mod_soil_water_p3$date_jul <- mod_soil_water_p3$Day
mod_soil_water_p3$wfps_25_mod <- (mod_soil_water_p3$X20cm + mod_soil_water_p3$X30cm) / 2

mod_soil_water_p3$vwc_5 <- mod_soil_water_p3$X5cm * (1.26/2.65) * 100
mod_soil_water_p3$vwc_25 <- c((mod_soil_water_p3$X20cm + mod_soil_water_p3$X30cm)/2 * (1.30/2.65) * 100)
mod_soil_water_p3$vwc_55 <- c((mod_soil_water_p3$X50cm + mod_soil_water_p3$X60cm)/2 * (1.35/2.65) * 100)
mod_soil_water_p3$vwc_85 <- c((mod_soil_water_p3$X80cm + mod_soil_water_p3$X90cm)/2 * (1.40/2.65) * 100)
mod_soil_water_p3$doy <- mod_soil_water_p3$date_jul

mod_soil_water_p3 <- mod_soil_water_p3 %>%
  filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 7 ~ 2018,
    Year == 8 ~ 2019,
    Year == 9 ~ 2020,
    Year == 10 ~ 2021,
    Year == 11 ~ 2022,
    Year == 12 ~ 2023
  ))

mod_soil_water_p3$date <- as.Date(paste(mod_soil_water_p3$year, mod_soil_water_p3$date_jul), format = "%Y %j")

# graphs
combined_data_swc_p3 <- merge(obs_soil_wfps_p3, mod_soil_water_p3, by = "date")
combined_data_swc_p3$year <- combined_data_swc_p3$year.x

# wfps_5

climate_filtered <- climate %>%
  filter(doy > 90 & doy < 335) %>%  # Filter for the range of days of the year (March 31st to Dec 1st)
  filter(year >= 2018 & year <= 2022)

combined_data_swc_p3 %>% 
  filter(doy > 90 & doy < 335) %>%  # Filter for the desired doy range
  ggplot(aes(x = doy, y = X5cm * 100)) +
  geom_point(size = 1, color = "gray") +
  geom_point(aes(x = doy, y = wfps_5), 
             color = "#D55E00", size = 1, linetype = 3) +
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw() +
  ylab("WFPS at 5 cm (%)") +
  geom_vline(data = filter(mgmt_dates_p1, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
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

combined_data_swc_p3 %>% 
  filter(doy > 90 & doy < 335) %>% # day = 90 is March 31st, and day = 335 is dec 1st
  ggplot(aes(x = doy, y = wfps_25_mod*100))+
  geom_point(size = 1, color = "gray")+
  geom_point(aes(x = doy, y = wfps_25), 
             color = "#009E73", size = 1, linetype = 3)+
  facet_wrap(year ~ ., nrow = 5) +
  theme_bw()+
  ylab("WFPS at 25 cm (%)")+
  geom_vline(data = filter(mgmt_dates_p3, year %in% c(2018:2022)), aes(xintercept = doy), color = "red", linetype = "dashed") +
  geom_bar(data = climate_filtered, aes(x = doy, y = prec*17), # 15 is an scaling factor
           stat = "identity", fill = "blue", alpha = 0.4, position = "identity", width = 1) +
  scale_y_continuous(
    name = "WFPS at 25 cm (%)", 
    limits = c(0, 100),  # Adjust the limits of the primary axis
    sec.axis = sec_axis(
      trans = ~ ./17,  # No scaling needed, precipitation is already in cm
      name = "Precipitation (cm)",
      breaks = seq(0, 6, 1)  # Set the secondary axis from 0 to 6 cm
    )
  )


### statistics

# wfps 5 cm

# metrics_wfps5_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_swc_p3, Day > 90 & Day < 335)$wfps_5
modeled <- filter(combined_data_swc_p3, Day > 90 & Day < 335)$X5cm*100
metrics_wfps5 <- calculate_metrics(observed, modeled)

metrics_wfps5_df <- rbind(metrics_wfps5_df, metrics_wfps5)
metrics_wfps5_df

# wfps 25 cm

# metrics_wfps25_df <- data.frame(
#   PBIAS = numeric(0),
#   NSE = numeric(0),
#   RMSE = numeric(0),
#   RRMSE_percent = numeric(0),
#   Index_of_Agreement_d = numeric(0),
#   R2 = numeric(0)
# )

observed <- filter(combined_data_swc_p3, Day > 90 & Day < 335)$wfps_25
modeled <- filter(combined_data_swc_p3, Day > 90 & Day < 335)$wfps_25_mod*100
metrics_wfps25 <- calculate_metrics(observed, modeled)

metrics_wfps25_df <- rbind(metrics_wfps25_df, metrics_wfps25)
metrics_wfps25_df


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
    Year_ == 17 ~ 2024,
    
  ))

mod_conv_soil_n$date <- as.Date(paste(mod_conv_soil_n$year, mod_conv_soil_n$date_jul), format = "%Y %j")
mod_conv_soil_n$doy <- mod_conv_soil_n$date_jul

mod_conv_soil_n$nh4_0_15 <- mod_conv_soil_n$NH4._0.10cm + mod_conv_soil_n$NH4._10.20cm/2
mod_conv_soil_n$no3_0_15 <- mod_conv_soil_n$X.NO3._0.10cm + mod_conv_soil_n$X.NO3._10.20cm/2

# plotting soil N for the entire period 2012-2023

# mod_conv_soil_n %>%
#   #filter(Year_ < 7) %>%
#   ggplot(aes(x = doy, y = no3_0_15))+
#   geom_line()+
#   geom_point(data = filter(obs_soil_n_p3_long, year>2011), aes(x=doy, y=no3_kg_n_ha), colour = "blue", alpha=0.5)+
#   facet_wrap(year~.)+
#   theme_bw()+
#   geom_vline(data = filter(fert_dates_p3, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p3, year %in% c(2012:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)
# 
# mod_conv_soil_n %>%
#   #filter(Year_ < 7) %>%
#   ggplot(aes(x = doy, y = nh4_0_15))+
#   geom_line()+
#   geom_point(data = filter(obs_soil_n_p3_long, year>2011), aes(x=doy, y=nh4_kg_n_ha), colour = "blue", alpha=0.5)+
#   facet_wrap(year~.)+
#   theme_bw()+
#   geom_vline(data = filter(fert_dates_p3, year %in% c(2012:2023)), aes(xintercept = doy), color = "red", linetype = "dashed", size = 0.75)+
#   geom_point(data = filter(mgmt_dates_p3, year %in% c(2012:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)

mod_conv_soil_n %>%
  #filter(Year_ < 7) %>%
  ggplot(aes(x = doy, y = no3_0_15))+
  geom_line(colour = "#009E73", alphha = 0.75, size = 0.75)+
  geom_point(data = filter(obs_soil_n_p1_long, year %in% c(2012:2014)), aes(x=doy, y=no3_kg_n_ha), colour = "#009E73", alpha=0.5)+
  geom_point(data = filter(obs_soil_n_p3_long, year %in% c(2008:2023)), aes(x=doy, y=no3_kg_n_ha), colour = "#009E73", alpha=0.5)+
  geom_point(data = filter(obs_soil_n_p3_long, year %in% c(2008:2023)), aes(x=doy, y=nh4_kg_n_ha), colour = "#D55E00", alpha=0.5)+
  geom_line(data = mod_conv_soil_n, aes(x=doy, y=nh4_0_15), colour = "#D55E00", alpha=0.75, size = 0.75)+
  
  facet_wrap(year~.)+
  theme_bw()+
  geom_vline(data = filter(fert_dates_p3, year %in% c(2012:2023)), aes(xintercept = doy), color = "darkred", linetype = "dashed", size = 1)+
  #geom_vline(data = filter(fert_dates_p1, year %in% c(2008:2014)), aes(xintercept = doy), color = "#CC79A7", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p3, year %in% c(2008:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)+
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
  #filter(year >2017) %>% 
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

merged_data <- merge(mod_conv_soil_n, obs_soil_n_p4, by = "date") %>% 
  select(date, nh4_0_15, nh4_kg_n_ha, year.x) %>% 
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

merged_data <- merge(mod_conv_soil_n, obs_soil_n_p3, by = "date") %>% 
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
  geom_point(data = obs_soil_n_p3,
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
combined_data <- merge(obs_soil_n_p3, mod_conv_soil_n, by = "date")
observed <- combined_data$nh4_kg_n_ha
modeled <- combined_data$nh4_0_15
calculate_metrics(observed, modeled)

# no3
combined_data <- merge(obs_soil_n_p3, mod_conv_soil_n, by = "date")
observed <- combined_data$no3_kg_n_ha
modeled <- combined_data$no3_0_15
calculate_metrics(observed, modeled)

###### CO2 ############

# import modeled data for p3

mod_co2_p3 <- read.csv("C:/DNDC/Result/Record/Site/Day_SoilC_1.csv", header = FALSE)
colnames(mod_co2_p3) <- mod_co2_p3[2, ]
colnames(mod_co2_p3) <- make.names(colnames(mod_co2_p3), unique = TRUE)
mod_co2_p3 <- mod_co2_p3[-c(1,2),]
mod_co2_p3 <- data.frame(lapply(mod_co2_p3, as.numeric))

mod_co2_p3$date_jul <- mod_co2_p3$Day

mod_co2_p3$mod_resp <- mod_co2_p3$Eco.respiration*1000/10000
mod_co2_p3$mod_gpp <- mod_co2_p3$Photosynthesis*(-1000/10000)
mod_co2_p3$mod_nee <- mod_co2_p3$X.NEE*1000/10000
mod_co2_p3 <- mod_co2_p3 %>%
  filter(Year > 6) %>%
  mutate(year = case_when(
    Year == 7 ~ 2018,
    Year == 8 ~ 2019,
    Year == 9 ~ 2020,
    Year == 10 ~ 2021,
    Year == 11 ~ 2022,
    Year == 12 ~ 2023
  ))

mod_co2_p3$date <- as.Date(paste(mod_co2_p3$year, mod_co2_p3$date_jul), format = "%Y %j")
mod_co2_p3$doy <- mod_co2_p3$Day

### statistics
merged_data <- merge(mod_co2_p3, obs_co2_p3, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))

predicted <- merged_data$mod_gpp
observed <- merged_data$obs_gpp

metrics_gpp <- calculate_metrics(observed, predicted)

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


## graphs 

# gpp

mod_co2_p3 %>% 
  filter(year < 2022) %>% 
  ggplot(aes(x = doy, y = mod_gpp))+
  geom_line(size = 1, color = "gray")+
  geom_line(data = obs_co2_p3, aes(x = doy, y = obs_gpp), color = "#009E73", linetype = 1, size = 1, alpha = 0.4)+
  #geom_line(data = obs_co2_p1, aes(x = date, y = obs_gpp), color = "red", linetype = 1, size = 1, alpha = 0.5)+
  # scale_x_date(
  #   date_breaks = "10 days",          # Breaks every 10 days
  #   date_labels = "%d/%m"          # Format as DD/MM
  # ) +
  facet_wrap(year~., nrow = 6)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("GPP (kg C/ha/day)")+
  geom_vline(aes(xintercept = 171), color = "red", linetype = "dashed", size = 0.75)+
  geom_point(data = filter(mgmt_dates_p3, year %in% c(2018:2021)), aes(xintercept = doy, y = -5), color = "blue", size = 4, shape = 4)

  

mod_co2_p3 %>% 
  filter(year == 2020 & Day > 274) %>% 
  summarise(
    gpp = mean(mod_gpp)
  )

# respiration

ggplot(data = mod_co2_p3, aes(x = date, y = mod_resp)) +
  geom_line(size = 1, color = "gray") +
  geom_line(data = obs_co2_p3, aes(x = date, y = obs_resp), color = "#D55E00", linetype = 1, size = 1) +
  facet_wrap(year ~ ., nrow = 5, scales = "free_x") +
  scale_x_date(
    date_breaks = "10 days",          # Breaks every 10 days
    date_labels = "%d/%m"          # Format as DD/MM
  ) +
  facet_wrap(year ~ ., nrow = 5, scales = "free_x") +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("Respiration (kg C/ha/day)")

# nee

ggplot(mod_conv_co21, aes(x = date, y = mod_nee))+
  geom_line()+
  geom_line(data = obs_co2_p1, aes(x = date, y = obs_nee), color = "#0072B2")+
  scale_x_date(
    date_breaks = "10 days",          # Breaks every 10 days
    date_labels = "%d/%m"          # Format as DD/MM
  ) +
  facet_wrap(year~., nrow = 5, scales = "free_x")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))

# statistics
merged_data <- merge(mod_co2_p3, obs_co2_p3, by = "date", all = TRUE, suffixes = c("_mod", "_obs"))

# gpp

predicted <- merged_data$mod_gpp
observed <- merged_data$obs_gpp
calculate_metrics(observed, predicted)

predicted <- filter(merged_data, year_mod %in% c(2018,2021))$mod_gpp
observed <- filter(merged_data, year_mod %in% c(2018,2021))$obs_gpp
calculate_metrics(observed, predicted)

# respiration

predicted <- merged_data$mod_gpp
observed <- merged_data$obs_gpp
metrics <- calculate_metrics(observed, predicted)

predicted <- filter(merged_data, year_mod %in% c(2018,2021))$mod_resp
observed <- filter(merged_data, year_mod %in% c(2018,2021))$obs_resp
calculate_metrics(observed, predicted)

# nee

predicted <- merged_data$mod_nee
observed <- merged_data$obs_nee
calculate_metrics(observed, predicted)

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


# loading other years of n2o data to compare

n2o_new_plot1_2000_2014 <- read_excel("obs_data/measured_n2o/other_or_older/e-26_longtermN2O_March_2025_Nir.xlsx", sheet = "e26_2000-2014_N2O_plot1")
n2o_new_plot3_2015_2017 <- read_excel("obs_data/measured_n2o/other_or_older/e-26_longtermN2O_March_2025_Nir.xlsx", sheet = "e26_2015-2023_N2O_plot3")

n2o_new_plot1_2000_2014$doy = n2o_new_plot1_2000_2014$DOY
n2o_new_plot1_2000_2014$year = n2o_new_plot1_2000_2014$Year
n2o_new_plot3_2015_2017$doy = yday(n2o_new_plot3_2015_2017$Date) 
n2o_new_plot3_2015_2017$year = n2o_new_plot3_2015_2017$Year

# graphs 

mod_n2o_p3 %>% 
  filter(year %in%c(2018:2024)) %>% 
  ggplot(aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_d)) +
  geom_line(size = 1, color = "darkgray") +
  geom_line(data = filter(obs_n2o_p3p4, year < 2025), aes(x = as.numeric(doy), y = n2o_flux_g_n_ha_day), color = "#0072B2", size = 1, alpha = 0.6) +
  
  #geom_line(data = filter(n2o_new_plot1_2000_2014, year %in% c(2010:2014)), aes(x = as.numeric(doy), y = as.numeric(`N2O_non gapfilled (g N2O_N ha-1 d-1)`)), color = "#0072B2", size = 1, alpha = 0.6) +
  #geom_line(data = filter(n2o_new_plot3_2015_2017, year %in% c(2015:2017)), aes(x = as.numeric(doy), y = as.numeric(`N2O_non gapfilled (g N2O_N ha-1 d-1)`)), color = "#0072B2", size = 1, alpha = 0.6) +
  facet_wrap(year ~ ., ncol = 2) +
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90))+
  ylab("N2O emissions (g N/ha/day)")+
  geom_vline(data = filter(fert_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy), color = "darkred", linetype = "dashed", size = 1)+
  
  geom_point(data = filter(mgmt_dates_p3, year %in% c(2018:2023)), aes(xintercept = doy, y = 100), color = "blue", size = 4, shape = 4)
  # geom_point(data = combined_data_water, aes(x = doy, y = X5cm * 100), size = 1, color = "blue", alpha = 0.5)+
  # geom_point(data = combined_data_water, aes(x = doy, y = wfps_5), size = 1, color = "#D55E00", alpha = 0.5)


# statistics 
merged_data <- merge(mod_n2o_p3, obs_n2o_p3p4, by = c("year", "doy"), all = TRUE, suffixes = c("_mod", "_obs"))

predicted <- merged_data$n2o_flux_g_n_ha_d
observed <- merged_data$n2o_flux_g_n_ha_day
metrics <- calculate_metrics(observed, predicted)
metrics


#########################

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

