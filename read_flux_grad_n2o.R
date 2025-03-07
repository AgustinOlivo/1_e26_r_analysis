# importing and merging flux gradient n2o data
# 03/03/2024
# agustin olivo
# aolivo@uoguelph.ca


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
flux_grad_p3_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2018_n2o) <- column_names
flux_grad_p4_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2018_n2o) <- column_names

#2019
flux_grad_p1_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2019_n2o) <- column_names
flux_grad_p2_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2019_n2o) <- column_names
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names

#2020
flux_grad_p1_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2020_n2o) <- column_names
flux_grad_p2_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2020_n2o) <- column_names
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names

#2021
flux_grad_p1_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2021_n2o) <- column_names
flux_grad_p2_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2021_n2o) <- column_names
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names

#2022
flux_grad_p1_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P1.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p1_2022_n2o) <- column_names
flux_grad_p2_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P2.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p2_2022_n2o) <- column_names
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names

#2023
flux_grad_p1_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P1", skip = 3, header = FALSE)
colnames(flux_grad_p1_2023_n2o) <- column_names
flux_grad_p2_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P2", skip = 3, header = FALSE)
colnames(flux_grad_p2_2023_n2o) <- column_names
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names

#2024
flux_grad_p1_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P1.csv", skip = 3, header = FALSE)
colnames(flux_grad_p1_2024_n2o) <- column_names
flux_grad_p2_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P2.csv", skip = 3, header = FALSE)
colnames(flux_grad_p2_2024_n2o) <- column_names
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p4_2024_n2o) <- column_names

# processing

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

# merging
str(azeem)
flux_grad_p1p2_2018_2024_n2o %>%
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, scales = "free_y")+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 


str(flux_grad_p1p2_2018_2024_n2o)

flux_grad_p1p2_2018_2024_n2o %>%
  group_by(Year, DoY) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  ) %>% 
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, )+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p1, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 

azeem$DoY = azeem$DOY
azeem$original_data = as.numeric(azeem$original_data )

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
colnames(mgmt_dates_p3) <- c("DoY", "Year")

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
colnames(mgmt_dates_p1) <- c("DoY", "Year")

# read azeem's data

azeem <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/x_other/simulation_azeem/E26_Gap filling_Azeem_2024paper.xlsx", sheet = "DATA")

azeem12_n2o <- azeem %>% 
  filter(Plot %in% c("P1", "P2")) %>%
  group_by(DoY, Year) %>% 
  summarise(
    avg12_n2o = mean(original_data, na.rm = TRUE)
  )


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
flux_grad_p3_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2018_n2o) <- column_names
flux_grad_p4_2018_n2o <- read.csv("obs_data/measured_n2o/raw_data/2018/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2018_n2o) <- column_names

#2019
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names
flux_grad_p3_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2019_n2o) <- column_names
flux_grad_p4_2019_n2o <- read.csv("obs_data/measured_n2o/raw_data/2019/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2019_n2o) <- column_names

#2020
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names
flux_grad_p3_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2020_n2o) <- column_names
flux_grad_p4_2020_n2o <- read.csv("obs_data/measured_n2o/raw_data/2020/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2020_n2o) <- column_names

#2021
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names
flux_grad_p3_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2021_n2o) <- column_names
flux_grad_p4_2021_n2o <- read.csv("obs_data/measured_n2o/raw_data/2021/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2021_n2o) <- column_names

#2022
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_p4.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names
flux_grad_p3_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p3_2022_n2o) <- column_names
flux_grad_p4_2022_n2o <- read.csv("obs_data/measured_n2o/raw_data/2022/N2Oflux_P3.CSV", skip = 3, header = FALSE)
colnames(flux_grad_p4_2022_n2o) <- column_names

#2023
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_p4", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names
flux_grad_p3_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p3_2023_n2o) <- column_names
flux_grad_p4_2023_n2o <- read.csv("obs_data/measured_n2o/raw_data/2023/N2Oflux_P3", skip = 3, header = FALSE)
colnames(flux_grad_p4_2023_n2o) <- column_names

#2024
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_p4.csv", skip = 3, header = FALSE)
colnames(flux_grad_p4_2024_n2o) <- column_names
flux_grad_p3_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
colnames(flux_grad_p3_2024_n2o) <- column_names
flux_grad_p4_2024_n2o <- read.csv("obs_data/measured_n2o/raw_data/2024/N2Oflux_P3.csv", skip = 3, header = FALSE)
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

# merging
flux_grad_p3p4_2018_2024_n2o %>%
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  #geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, scales = "free_y")+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 


flux_grad_p3p4_2018_2024_n2o %>%
  group_by(Year, DoY) %>% 
  summarise(
    n_obs_n2o = sum(!is.na(N2O_flux)),  # Count non-NA half-hourly values
    N2O_flux = ifelse(n_obs_n2o > 2, mean(N2O_flux, na.rm = TRUE), NA_real_),   
  ) %>% 
  ggplot(aes(x = DoY, y = N2O_flux))+
  geom_point(alpha = 0.5, color = "blue")+
  geom_point(data = filter(azeem12_n2o, Year %in% c(2018:2024)), aes(x = DoY, y = avg12_n2o), alpha = 0.5, color = "orange")+
  facet_wrap(Year~., ncol = 2, )+
  theme_bw()+
  ylab("N2o fLUX (g N2O-N ha-1 day-1)")+
  geom_vline(xintercept = 121, color = "red", linetype = "dashed")+
  geom_vline(data = filter(mgmt_dates_p3, Year %in% c(2018:2024)), aes(xintercept = DoY), color = "darkgreen", linetype = "dashed") 

azeem$DoY = azeem$DOY
azeem$original_data = as.numeric(azeem$original_data )

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
colnames(mgmt_dates_p3) <- c("DoY", "Year")

mgmt_dates_p3 <- as.data.frame( do.call( rbind, list(
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
colnames(mgmt_dates_p3) <- c("DoY", "Year")

# read azeem's data

azeem <- read_excel("C:/Users/aolivo/OneDrive - University of Guelph/0_all_files_postdoc/1_projects/1_modeling_div_conv/x_other/simulation_azeem/E26_Gap filling_Azeem_2024paper.xlsx", sheet = "DATA")

azeem12_n2o <- azeem %>% 
  filter(Plot %in% c("P3", "P4")) %>%
  group_by(DoY, Year) %>% 
  summarise(
    avg12_n2o = mean(original_data, na.rm = TRUE)
  )
########################################################################