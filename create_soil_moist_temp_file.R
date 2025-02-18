# file to extract soil temperature and water data for 2022

temp_1 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P1_Temp.csv", header = TRUE)
temp_2 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P2_Temp.csv", header = TRUE)
temp_3 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P3_Temp.csv", header = TRUE)
temp_4 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P4_Temp.csv", header = TRUE)

vwc_1 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P1_VWC.csv", header = TRUE)
vwc_2 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P2_VWC.csv", header = TRUE)
vwc_3 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P3_VWC.csv", header = TRUE)
vwc_4 <- read.csv("Z:/E26/database/ers4/2022/Soil Temp Moisture/Final/P4_VWC.csv", header = TRUE)

str(temp_1)

temp_1_av <- temp_1 %>% 
  mutate(
    doy = trunc(DOY),
    ) %>% 
  group_by(doy) %>% 
  summarise(
    t_5 = mean(P1_T_5),
    t_25 = mean(P1_T_25),
    t_55 = mean(P1_T_55),
    t_85 = mean(P1_T_85),
    plot = 1
  )

temp_2_av <- temp_2 %>% 
  mutate(
    doy = trunc(DOY)
  ) %>% 
  group_by(doy) %>% 
  summarise(
    t_5 = mean(P2_T_5),
    t_25 = mean(P2_T_25),
    t_55 = mean(P2_T_55),
    t_85 = mean(P2_T_85),
    plot = 2
  )

temp_3_av <- temp_3 %>% 
  mutate(
    doy = trunc(DOY)
  ) %>% 
  group_by(doy) %>% 
  summarise(
    t_5 = mean(P3_T_5),
    t_25 = mean(P3_T_25),
    t_55 = mean(P3_T_55),
    t_85 = mean(P3_T_85),
    plot = 3
  )


temp_4_av <- temp_4 %>% 
  mutate(
    doy = trunc(DOY)
  ) %>% 
  group_by(doy) %>% 
  summarise(
    t_5 = mean(P4_T_5),
    t_25 = mean(P4_T_25),
    t_55 = mean(P4_T_55),
    t_85 = mean(P4_T_85),
    plot = 4
  )

merged_temp <- bind_rows(temp_1_av, temp_2_av, temp_3_av, temp_4_av)

write_xlsx(merged_temp, path = "soil_temp_2022.xlsx")

merged_temp %>% 
  ggplot(aes(x = doy, y = t_5 ))+
  geom_line()+
  facet_wrap(plot~.)+
  theme_bw()

















