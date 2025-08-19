# importing and merging flux gradient n2o data
# 03/03/2024
# agustin olivo
# aolivo@uoguelph.ca


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

###################################

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

#2019
flux_grad_p1_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2019_n2o) <- column_names
flux_grad_p2_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2019_n2o) <- column_names

#2020
flux_grad_p1_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2020_n2o) <- column_names
flux_grad_p2_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2020_n2o) <- column_names

#2021
flux_grad_p1_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2021_n2o) <- column_names
flux_grad_p2_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2021_n2o) <- column_names

#2022
flux_grad_p1_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P1.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p1_2022_n2o) <- column_names
flux_grad_p2_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P2.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p2_2022_n2o) <- column_names

#2023
flux_grad_p1_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2023_n2o) <- column_names
flux_grad_p2_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2023_n2o) <- column_names

#2024
flux_grad_p1_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P1.csv", skip = 3, header = FALSE)
colnames(flux_grad_p1_2024_n2o) <- column_names
flux_grad_p1_2024_n2o <- flux_grad_p1_2024_n2o %>% 
  select(-which(is.na(names(.))))
flux_grad_p1_2024_n2o <- flux_grad_p1_2024_n2o %>% 
  mutate(N2O_flux = ifelse(DOY < 110 | DOY >180, N2O_flux, N2O_flux*-1)) # claudia indicated about the 2024 fluxes: It also looks like plot 1 (red symbols) is inverted (that happens with our inatkes some times) so that the flux needs to be m,ultiplied by -1. Looking a little more in detail, seems like the flipped values are mostly from doy 110 onwards (likely up to 180, although after that there seems to be other issues with the data). For the purpose of this project, I will flip both N2O and CO2 from day 110 to 180, as that should include all dates I need for this project (that goes up to doy 121 of 2024)

flux_grad_p2_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P2.csv", skip = 3, header = FALSE)
colnames(flux_grad_p2_2024_n2o) <- column_names
flux_grad_p2_2024_n2o$N2O_flux <- flux_grad_p2_2024_n2o$N2O_flux 

# processing plots 1 and 2

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
flux_grad_p1p2_2018_2024_n2o$Plot <- as.character(flux_grad_p1p2_2018_2024_n2o$Plot)

flux_grad_p1p2_2018_2024_n2o %>% 
  filter(Year %in% 2018:2023) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% 
  write_xlsx("flux_grad_p1p2_2018_2023_n2o.xlsx") 

# importing other information needed

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
  c(277,2023), # harvest
  c(143,2024)
  
)))
colnames(mgmt_dates_p1) <- c("DoY", "Year")

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
  c(168,2021), # UAN injected
  c(143,2024),
  c(176,2024)
  
)))
colnames(fert_dates_p1) <- c("DoY", "Year")

# read azeem's data

azeem <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/x_other/simulation_azeem/E26_Gap filling_Azeem_2024paper.xlsx", sheet = "DATA")

azeem12_n2o <- azeem %>% 
  filter(Plot %in% c("P1", "P2")) %>%
  group_by(DoY, Year) %>% 
  summarise(
    avg12_n2o = mean(original_data, na.rm = TRUE)
  )

# plots

flux_grad_p1p2_2018_2024_n2o %>%
  filter(Year==2024) %>% 
  #filter(Year == 2024 & Plot == 2) %>% 
  #filter(Year %in% 2022:2024) %>% 
  #filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  #filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  filter(!abs(N2O_flux) > 150) %>% # I am looking at the data and there seem to be some very large values which I will remove.
  # group_by(Year, DoY, Plot) %>%
  # summarise(
  #   n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
  #   N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  # ) %>%
  #ggplot(aes(x = DoY, y = N2O_flux))+
  ggplot(aes(x = DoY, y = N2O_flux, color = Plot))+
  geom_hline(yintercept = 0, color = "black")+
  # geom_rect(
  #   data = data.frame(Year = 2023),  # Filter to apply only to 2023 facet
  #   aes(xmin = 171, xmax = 365, ymin = -Inf, ymax = Inf),
  #   fill = "red", alpha = 0.2, inherit.aes = FALSE
  # )+
  # geom_rect(
  #   data = data.frame(Year = 2024),  # Filter to apply only to 2023 facet
  #   aes(xmin = 121, xmax = 365, ymin = -Inf, ymax = Inf),
  #   fill = "red", alpha = 0.2, inherit.aes = FALSE
  # )+
  # geom_rect(
  #   data = data.frame(Year = 2024),  # Filter to apply only to 2023 facet
  #   aes(xmin = 0, xmax = 120, ymin = -Inf, ymax = Inf),
  #   fill = "orange", alpha = 0.2, inherit.aes = FALSE
  # )+
  geom_point(alpha = 0.3)+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  #facet_wrap(Year~., ncol = 2, scales = "free_y")+
  facet_wrap(Year~., ncol = 7)+
  #facet_grid(vars(Year), vars(Plot))+
  theme_bw()+
  ylab("N2O Flux (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")
  #geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2023:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") +
  #geom_vline(data = filter(fert_dates_p1, Year %in% c(2024:2024)), aes(xintercept = DoY), color = "blue", linetype = "dashed")

ggsave("p12_n2o.png",  width = 10, height = 6)
ggsave("p12_n2o_daily.png", plot = last_plot(), width = 10, height = 6)

flux_grad_p1p2_2018_2024_n2o %>%
  #filter(Year == 2024 & Plot == 2) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  group_by(Year, DoY, Plot) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  ) %>% 
  #ggplot(aes(x = DoY, y = N2O_flux, color = Plot))+
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.75, size=1, color = "#0072B2")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., nrow = 1 )+
  scale_y_continuous(limits = c(-100, 270), breaks = seq(-100, 270, by = 50))+
  theme_bw()+
  ylab("N2O flux (g N2O-N ha-1 day-1)")+
  #geom_vline(xintercept = 121, color = "red", linetype = "dashed")
  geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "red", linetype = "dashed")

ggsave("p12_n2o_daily.png", plot = last_plot(), width = 10, height = 2.8)
                           
# exporting the data combining plots 1 and 2

flux_grad_p1p2_2018_2024_n2o %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  group_by(Year, DoY) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
  mutate(
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  #   mutate(
  #   plot = Plot,
  #   year = Year,
  #   doy = DOY,
  #   n2o_flux_g_n_ha_day = N2O_flux
  # ) %>%
  select(year, doy, n2o_flux_g_n_ha_day) %>% 
  #select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  write_xlsx("flux_grad_p1p2_2018_2024_n2o_daily.xlsx")  # Export as Excel file

# exporting the data separately for plots 1 and 2

flux_grad_p1p2_2018_2024_n2o %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  group_by(Year, DoY, Plot) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
    mutate(
    plot = Plot,
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  write_xlsx("flux_grad_p1p2_2018_2024_n2o_daily_plot.xlsx")  # Export as Excel file

flux_grad_p1p2_2018_2024_n2o %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  group_by(Year, DoY, Plot) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
  mutate(
    plot = Plot,
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  filter(year == 2024) %>% 
  ggplot()+
  geom_point(aes(x = doy, y = n2o_flux_g_n_ha_day))

flux_grad_p3p4_2018_2024_n2o %>%
  filter(N2O_flux <1000) %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  group_by(Year, DoY) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
  mutate(
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  # mutate(
  #   plot = Plot,
  #   year = Year,
  #   doy = DoY,
  #   n2o_flux_g_n_ha_day = N2O_flux
  # ) %>% 
  select(year, doy, n2o_flux_g_n_ha_day) %>% 
  #select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  write_xlsx("flux_grad_p3p4_2018_2024_n2o_daily.xlsx")  # Export as Excel file



  
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

#2019
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names


#2020
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names

#2021
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names

#2022
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p4.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names

#2023
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names

#2024
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p4.csv", skip = 3, header = FALSE)
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
flux_grad_p3p4_2018_2024_n2o$Plot <- as.character(flux_grad_p3p4_2018_2024_n2o$Plot)

# reading in other information

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
colnames(fert_dates_p3) <- c("DoY", "Year")

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
  c(221,2023), # harvest
  c(143,2024)
  
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
azeem$DoY = azeem$DOY
azeem$original_data = as.numeric(azeem$original_data )

# plots

flux_grad_p3p4_2018_2024_n2o %>%
  filter(Year %in% 2022:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  ggplot(aes(x = DoY, y = N2O_flux, color = Plot))+
  geom_point(alpha = 0.5)+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2)+
  #facet_wrap(Year~., ncol = 2, scales = "free_y")+
  
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 

  flux_grad_p1p2_2018_2024_n2o %>% 
  filter(Year %in% 2022:2024) %>%
  summarise(
    n = sum(!is.na(N2O_flux))
  )
  

flux_grad_p3p4_2018_2024_n2o %>%
  filter(N2O_flux <300) %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  #filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  group_by(Year, DoY, Plot) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_)
  ) %>%
  #ggplot(aes(x = DoY, y = N2O_flux, color = Plot))+
  ggplot(aes(x = DoY, y = N2O_flux))+
  # geom_rect(
  #   data = data.frame(Year = 2023),  # Filter to apply only to 2023 facet
  #   aes(xmin = 171, xmax = 365, ymin = -Inf, ymax = Inf),
  #   fill = "red", alpha = 0.1, inherit.aes = FALSE
  # )+
  # geom_rect(
  #   data = data.frame(Year = 2024),  # Filter to apply only to 2023 facet
  #   aes(xmin = 121, xmax = 365, ymin = -Inf, ymax = Inf),
  #   fill = "red", alpha = 0.1, inherit.aes = FALSE
  # )+
  # geom_rect(
  #   data = data.frame(Year = 2024),  # Filter to apply only to 2023 facet
  #   aes(xmin = 0, xmax = 121, ymin = -Inf, ymax = Inf),
  #   fill = "orange", alpha = 0.2, inherit.aes = FALSE
  # )+
  #geom_hline(yintercept = 0, color = "black")+
  geom_point(alpha = 0.3,color = "#0072B2")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  #facet_grid(vars(Year), vars(Plot))+
  scale_y_continuous(limits = c(-100, 270), breaks = seq(-100, 270, by = 50))+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "red", linetype = "dashed")+
  facet_wrap(Year~., nrow = 1 )+
  theme_bw()+
  ylab("N2O Flux (g N2O-N ha-1 day-1)")

ggsave("p34_n2o_daily.png", plot = last_plot(), width = 10, height = 2.8)


  #geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") +
  geom_vline(data = filter(fert_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "blue", linetype = "dashed")

ggsave("p34_n2o.png",  width = 10, height = 6)
ggsave("p34_n2o_daily.png",  width = 10, height = 6)

# 
# flux_grad_p3p4_2018_2024_n2o %>%
#   group_by(Year, DoY) %>% 
#   summarise(
#     n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
#     N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
#   ) %>% 
#   ggplot(aes(x = DoY, y = N2O_flux))+
#   geom_point(alpha = 0.5, color = "blue")+
#   geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
#   facet_wrap(Year~., ncol = 2, )+
#   theme_bw()+
#   ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
#   geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
#   geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 


# exporting the data combining plots 1 and 2

flux_grad_p3p4_2018_2024_n2o %>%
  filter(N2O_flux <1000) %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  group_by(Year, DoY) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
  mutate(
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  # mutate(
  #   plot = Plot,
  #   year = Year,
  #   doy = DoY,
  #   n2o_flux_g_n_ha_day = N2O_flux
  # ) %>% 
  select(year, doy, n2o_flux_g_n_ha_day) %>% 
  #select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  write_xlsx("flux_grad_p3p4_2018_2024_n2o_daily.xlsx")  # Export as Excel file

# exporting the data separately for plots 3 and 4

flux_grad_p3p4_2018_2024_n2o %>%
  filter(N2O_flux <1000) %>%
  filter(Year %in% 2018:2024) %>% 
  filter(!(Year == 2023 & DoY > 171)) %>% # filtering out these dates as per Claudia's and Eli's recommendation (from Claudia in email 2025/03/12: "I think the data in 2023 is ok until about the break in the data (mid-June) when we had issues with the pump.")
  #filter(!(Year == 2024 & DoY > 121)) %>% # filtering out these dates as per Claudia's and Eli's recommendation
  filter(!(Year == 2024 & DoY < 5)) %>% # I am looking at the data and there seem to be some very large negative and isolated values that might be related to equipment disfunction in late 2023; will discard them.
  group_by(Year, DoY, Plot) %>%
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),
  ) %>%
  ungroup() %>%
  mutate(
    plot = Plot,
    year = Year,
    doy = DoY,
    n2o_flux_g_n_ha_day = N2O_flux
  ) %>%
  select(plot, year, doy, n2o_flux_g_n_ha_day) %>% 
  write_xlsx("flux_grad_p3p4_2018_2024_n2o_daily_plot.xlsx")  # Export as Excel file


########################################################################

#######################################################################
###### importing flux gradient data co2 ###############################

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
flux_grad_p1_2024 <- flux_grad_p1_2024 %>%
  mutate(
    ` Date` = as.numeric(` Date`),
    `CO2 flux` = as.numeric(`CO2 flux`),
    `CO2 flux` = ifelse(` Date` >= 110 & ` Date` <= 180, `CO2 flux` * -1, `CO2 flux`)
  ) %>% 
  mutate(` Date` = as.character(` Date`),
         `CO2 flux` = as.character(`CO2 flux`))
 # claudia indicated about the 2024 n2o fluxes: It also looks like plot 1 (red symbols) is inverted (that happens with our inatkes some times) so that the flux needs to be m,ultiplied by -1. Looking a little more in detail at the N2O fluxes, seems like the flipped values are mostly from doy 110 onwards (likely up to 180, although after that there seems to be other issues with the N2O data). This looks similar to some extent to what is happening in the CO2 data. For the purpose of this project, I will flip both N2O and CO2 from day 110 to 180, as that should include all dates I need for this project (that goes up to doy 121 of 2024)

flux_grad_p2_2024 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2024/CO2flux_P2.csv", 
                              header = FALSE)
colnames(flux_grad_p2_2024) <- flux_grad_p2_2024[2, ]
flux_grad_p2_2024 <- flux_grad_p2_2024[-c(1:3), ]

# processing

flux_grad_p1p2_2018_2024 <- bind_rows(flux_grad_p1_2018, flux_grad_p2_2018, flux_grad_p1_2019, flux_grad_p2_2019, flux_grad_p1_2020, flux_grad_p2_2020, flux_grad_p1_2021, flux_grad_p2_2021, flux_grad_p1_2022, flux_grad_p2_2022, flux_grad_p1_2023, flux_grad_p2_2023, flux_grad_p1_2024, flux_grad_p2_2024)

flux_grad_p1p2_2018_2024$CO2_flux_umol <- (as.numeric(flux_grad_p1p2_2018_2024$`CO2 flux`) / 44.01) 

flux_grad_p1p2_2018_2024 <- mutate(flux_grad_p1p2_2018_2024, 
                                   CO2_flux_umol = ifelse(CO2_flux_umol < -100, NA, CO2_flux_umol), 
                                   CO2_flux_umol = ifelse(CO2_flux_umol > 60, NA, CO2_flux_umol)
)

flux_grad_p1p2_2018_2024$Date <- as.numeric(flux_grad_p1p2_2018_2024$` Date`)
flux_grad_p1p2_2018_2024$Year <- as.numeric(flux_grad_p1p2_2018_2024$`% Year`)

# loading eddy data to compare

CombinedData_p12$Date <- CombinedData_p12$DoY + CombinedData_p12$Hour / 24

# merging

flux_ec_2018_2024 <- CombinedData_p12 %>% 
  mutate(Date = round(Date, 3)) %>%
  left_join(
    flux_grad_p1p2_2018_2024 %>% mutate(Date = round(Date, 3)), 
    by = c("Date", "Year")
  )
flux_ec_2018_2024 %>%
  # filter(!c(Year==2018 & DoY<121)) %>% 
  # filter(!c(Year==2024 & DoY>121)) %>% 
  filter(c(Year==2024 & DoY %in% 305:365)) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()+
  ylab("CO2 FG (umolm-2s-1)")+
  xlab("CO2 EC (umolm-2s-1)")+
  labs(
    title = "CO2 FG vs CO2 EC (Nov-Dec 2024, plot 1&2)"
  )


flux_ec_2018_2024 %>%
  # filter(!c(Year==2018 & DoY<121)) %>% 
  # filter(!c(Year==2024 & DoY>121)) %>% 
  filter(c(Year==2023 & DoY %in% 325:335)) %>% 
  ggplot(aes(x = Date, y = NEE_U50_orig))+
  geom_point()+
  geom_point(data = filter(flux_ec_2018_2024, Year==2023 & DoY %in% 325:335), aes(x = Date, y = CO2_flux_umol), color = "blue")+
  #geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()+
  geom_hline(yintercept = 0)+
  xlab("doy")+
  ylab("CO2 EC (umolm-2s-1)")+
  labs(
    title = "CO2 FG (blue) vs CO2 EC (black) (2023, plot 1&2)"
  )

flux_ec_2018_2024 %>%
  # filter(!c(Year==2018 & DoY<121)) %>% 
  # filter(!c(Year==2024 & DoY>121)) %>% 
  filter(c(Year==2023 & DoY %in% 315:325 & abs(NEE_U50_orig) < 10)) %>% 
  ggplot(aes(x = Date, y = NEE_U50_orig))+
  geom_point()+
  geom_point(data = filter(flux_ec_2018_2024, Year==2022 & DoY %in% 315:325), aes(x = Date, y = NEE_U50_orig), color = "red", alpha  = 0.5)+
  geom_point(data = filter(flux_ec_2018_2024, Year==2023 & DoY %in% 315:325), aes(x = Date, y = CO2_flux_umol), color = "blue", alpha  = 0.5)+
  #geom_smooth(method = "lm", se = FALSE, color = "blue")+
  scale_x_continuous(limits = c(315, 325), breaks = seq(315, 325, by = 0.5))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  xlab("doy")+
  ylab("CO2 EC (umolm-2s-1)")+
  labs(
    title = "CO2 EC 2023 (black), CO2 EC 2022 (red), CO2 FG 2023 (blue) (plot 1&2)"
  )


flux_ec_2018_2024_model <- flux_ec_2018_2024 %>%
  filter(c(Year==2024 & DoY %in% 305:365)) 
  
  filter(!c(Year==2018 & DoY<121)) %>% 
  filter(!c(Year==2024 & DoY>121))         

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_2018_2024_model))
summary(model)

flux_grad_p1p2_2018_2024 %>%
  #filter(Year == 2023 & Date > 250) %>% 
  filter(Year == 2024 & Date <200) %>% 
  ggplot(aes(x = Date, y = CO2_flux_umol, color =  as.factor(` Plot`)))+
  geom_point(alpha = 0.3, size = 0.8)+
  scale_color_manual(values = c("#0072B2", "#D55E00"))+
  #geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., ncol = 3)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  xlab("DOY")
theme(
  legend.position = "bottom"
)

ggsave("fg_p12_2018_2024.png", plot = last_plot(), width = 11, height = 3.5)


# graph

flux_ec_2018_2024 %>%
  filter(!c(Year == 2018 & doy < 121)) %>% 
  
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





# Current gaps in the data are for DOY 268-340 in 2019, and DOY 230 to 279 in 2023; this is the relationship between EC and flux gradient points in 2020 and 2022 (two other soybean years), for the period doy 268-305. Based on what was discussed with Claudia, this could be used as a justification to gap-fill with flux gradient data.

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

#2018

flux_grad_p3_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2018) <- flux_grad_p3_2018[2, ]
flux_grad_p3_2018 <- flux_grad_p3_2018[-c(1:3), ]

flux_grad_p4_2018 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2018/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2018) <- flux_grad_p4_2018[2, ]
flux_grad_p4_2018 <- flux_grad_p4_2018[-c(1:3), ]

#2019

flux_grad_p3_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2019) <- flux_grad_p3_2019[2, ]
flux_grad_p3_2019 <- flux_grad_p3_2019[-c(1:3), ]

flux_grad_p4_2019 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2019/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2019) <- flux_grad_p4_2019[2, ]
flux_grad_p4_2019 <- flux_grad_p4_2019[-c(1:3), ]

#2020

flux_grad_p3_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2020) <- flux_grad_p3_2020[2, ]
flux_grad_p3_2020 <- flux_grad_p3_2020[-c(1:3), ]

flux_grad_p4_2020 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2020/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2020) <- flux_grad_p4_2020[2, ]
flux_grad_p4_2020 <- flux_grad_p4_2020[-c(1:3), ]


#2021

flux_grad_p3_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2021) <- flux_grad_p3_2021[2, ]
flux_grad_p3_2021 <- flux_grad_p3_2021[-c(1:3), ]

flux_grad_p4_2021 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2021/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2021) <- flux_grad_p4_2021[2, ]
flux_grad_p4_2021 <- flux_grad_p4_2021[-c(1:3), ]

#2022

flux_grad_p3_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P3.csv",
                              header = FALSE)
colnames(flux_grad_p3_2022) <- flux_grad_p3_2022[2, ]
flux_grad_p3_2022 <- flux_grad_p3_2022[-c(1:3), ]

flux_grad_p4_2022 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2022/CO2flux_P4.csv", 
                              header = FALSE)
colnames(flux_grad_p4_2022) <- flux_grad_p4_2022[2, ]
flux_grad_p4_2022 <- flux_grad_p4_2022[-c(1:3), ]


#2023

# for data from p3 and p4 for 2023, after showing the results in one of our team meetings, we agreed the data from day 240 onwards looked weird, so I decided to remove them. 

flux_grad_p3_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_p3",
                              header = FALSE)
colnames(flux_grad_p3_2023) <- flux_grad_p3_2023[2, ]
flux_grad_p3_2023 <- flux_grad_p3_2023[-c(1:3), ]
flux_grad_p3_2023 <- flux_grad_p3_2023 %>%
  filter(as.numeric(` Date`) < 240)

flux_grad_p4_2023 <- read.csv("obs_data/measured_co2/data_extraction/flux_gradient/2023/CO2flux_P4", 
                              header = FALSE)
colnames(flux_grad_p4_2023) <- flux_grad_p4_2023[2, ]
flux_grad_p4_2023 <- flux_grad_p4_2023[-c(1:3), ]
flux_grad_p4_2023 <- flux_grad_p4_2023 %>%
  filter(as.numeric(` Date`) < 240)


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

flux_grad_p3p4_2018_2024 <- bind_rows(flux_grad_p3_2018, flux_grad_p4_2018,flux_grad_p3_2019, flux_grad_p4_2019,flux_grad_p3_2020, flux_grad_p4_2020, flux_grad_p3_2021, flux_grad_p4_2021, flux_grad_p3_2022, flux_grad_p4_2022, flux_grad_p3_2023, flux_grad_p4_2023, flux_grad_p3_2024, flux_grad_p4_2024)

flux_grad_p3p4_2018_2024$CO2_flux_umol <- (as.numeric(flux_grad_p3p4_2018_2024$`CO2 flux`) / 44.01) 

flux_grad_p3p4_2018_2024 <- mutate(flux_grad_p3p4_2018_2024, 
                                   CO2_flux_umol = ifelse(CO2_flux_umol < -100, NA, CO2_flux_umol), 
                                   CO2_flux_umol = ifelse(CO2_flux_umol > 60, NA, CO2_flux_umol)
)

flux_grad_p3p4_2018_2024$Date <- as.numeric(flux_grad_p3p4_2018_2024$` Date`)
flux_grad_p3p4_2018_2024$Year <- as.numeric(flux_grad_p3p4_2018_2024$`% Year`)

flux_grad_p3p4_2018_2024 %>%
  filter(Year == 2024) %>% 
  ggplot(aes(x = Date, y = CO2_flux_umol, color =  as.factor(` Plot`)))+
  geom_point(alpha = 0.4, size = 0.8)+
  scale_color_manual(values = c("#0072B2", "#D55E00"))+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  #facet_grid(vars(variables), vars(Year))+
  facet_wrap(Year~., nrow = 1)+
  theme_bw()+
  ylab("NEE (umolm-2s-1)")+
  xlab("DOY")+
  theme(
    legend.position = "right"
  )

ggsave("fg_p34_2018_2024.png", plot = last_plot(), width = 11, height = 3.5)

# loading eddy data to compare

CombinedData$Date <- CombinedData$DoY + CombinedData$Hour / 24

# merging

flux_ec_p34_2018_2024 <- CombinedData %>% 
  mutate(Date = round(Date, 3)) %>%
  left_join(
    flux_grad_p3p4_2018_2024 %>% mutate(Date = round(Date, 3)), 
    by = c("Date", "Year")
  )
flux_ec_p34_2018_2024 %>%
  #filter(Year == 2023) %>% 
  ggplot(aes(x = NEE_U50_orig, y = CO2_flux_umol))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  theme_bw()

model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = filter(flux_ec_p34_2018_2024, Year == 2023))
summary(model)
model <- lm(NEE_U50_orig ~ CO2_flux_umol, data = flux_ec_p34_2018_2024)
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





#######################################################################