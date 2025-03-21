#Import the necessary libraries

library(tidyverse)
library(readxl)

#Import the first year data set
data_y1 <- read_csv("y1_qc.csv")
colnames(data_y1)
tibble(data_y1)
#Import the second year data set
data_y2 <- read_csv("y2_qc.csv")
tibble(data_y2)

#Modify time column to date time
data_y1$time <- ymd_hm(data_y1$time)
data_y2$time <- ymd_hm(data_y2$time)

# Look at the mean air temperature in each month (NaN values filtered out)
air_2020 <- data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(air_temperature)) %>%
  summarise(mean_airtemp = mean(air_temperature))

air_2021 <- data_y2 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(air_temperature)) %>%
  summarise(mean_airtemp = mean(air_temperature))



# Look at the max precipitation in each month (NaN values filtered out)
rain_2020 <- data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(precipitation)) %>%
  summarise(max_rain=max(precipitation))
            
rain_2021 <- data_y2 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(precipitation)) %>%
  summarise(max_rain=max(precipitation))


# Look at the mean gti cleaned pyranometer in each month (NaN values filtered out)
gti_2020 <- data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(gti_clean)) %>%
  summarise(mean_gti_clean = mean(gti_clean))

gti_2021 <- data_y2 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(gti_clean)) %>%
  summarise(mean_gti_clean = mean(gti_clean))

# Look at the mean DHI pyranometer in each month (NaN values filtered out)
dhi_2020 <- data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(dhi_pyr)) %>%
  summarise(mean_dhi_pyr = mean(dhi_pyr))

dhi_2021 <- data_y2 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(dhi_pyr)) %>%
  summarise(mean_dhi_pyr = mean(dhi_pyr))


#Individual plots 
library(patchwork)

p9 <- ggplot() +
  geom_line(aes(x = air_2020$month, y = air_2020$mean_airtemp), color="deeppink", size=1) +
  geom_line(aes(x = air_2021$month, y = air_2021$mean_airtemp), color="pink", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 and 2021 ",
       y = "Mean air temperature (C)",
       x = "Month") + theme_bw(base_size = 10)

p10 <- ggplot() +
  geom_line(aes(x = rain_2020$month, y = rain_2020$max_rain), color="blue", size=1) +
  geom_line(aes(x = rain_2021$month, y = rain_2021$max_rain), color="steelblue1", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 and 2021  ",
       y = "Max monthly rain (mm)",
       x = "Month") + theme_bw(base_size = 10)

p11 <- ggplot() +
  geom_line(aes(x = gti_2020$month, y = gti_2020$mean_gti_clean), color="coral1", size=1) +
  geom_line(aes(x = gti_2021$month, y = gti_2021$mean_gti_clean), color="lightsalmon", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 and 2021 ",
       y = "Mean cleaned GTI (V/m-2)",
       x = "Month") + theme_bw(base_size = 10)

p12 <- ggplot() +
  geom_line(aes(x = dhi_2020$month, y = dhi_2020$mean_dhi_pyr), color="darkorchid", size=1) +
  geom_line(aes(x = dhi_2021$month, y = dhi_2021$mean_dhi_pyr), color="plum4", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 and 2021 ",
       y = "Mean DHI (V/m-2)",
       x = "Month") + theme_bw(base_size = 10)


p9 + p10 + p11 + p12

