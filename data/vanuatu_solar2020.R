#Import the necessary libraries

library(tidyverse)
library(readxl)

#Import the first year data set
data_y1 <- read_csv("y1_qc.csv")
colnames(data_y1)
tibble(data_y1)

#Modify time column to date time
data_y1$time <- ymd_hm(data_y1$time)
data_y1$time[[31]]

# Look at the min and max air temperature in each month (NaN values filtered out)
data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(air_temperature)) %>%
  summarise(min_airtemp=min(air_temperature), max_airtemp=max(air_temperature), mean_airtemp = mean(air_temperature))


# Look at the min and max precipitation in each month (NaN values filtered out)
data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(precipitation)) %>%
  summarise(min_rain=min(precipitation), max_rain=max(precipitation), mean_rain = mean(precipitation))
            

# Look at the min and max gti cleaned pyranometer in each month (NaN values filtered out)
data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(gti_clean)) %>%
  summarise(min_gti_clean=min(gti_clean), max_gti_clean=max(gti_clean), mean_gti_clean = mean(gti_clean))


# Look at the min and max DHI pyranometer in each month (NaN values filtered out)
data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(dhi_pyr)) %>%
  summarise(min_dhi_pyr=min(dhi_pyr), max_dhi_pyr=max(dhi_pyr), mean_dhi_pyr = mean(dhi_pyr))


# Visualize only the useful columns
y1_plot_raw <- data_y1 %>%
  mutate(month = month(time)) %>%
  group_by(month) %>%
  filter(!is.nan(air_temperature) & !is.nan(precipitation) & !is.nan(gti_clean) & !is.nan(dhi_pyr)) %>%
  summarise(mean_airtemp = mean(air_temperature), max_rain=max(precipitation), mean_gti_clean = mean(gti_clean), mean_dhi_pyr = mean(dhi_pyr))


y1_plot_raw %>%
  ggplot() +
  geom_line(aes(x = month, y = mean_airtemp), color="red", size=3) +
  geom_line(aes(x = month, y = max_rain), color="blue", size=3) +
  geom_line(aes(x = month, y = mean_gti_clean), color="coral1", size=3) +
  geom_line(aes(x = month, y = mean_dhi_pyr), color="darkorchid4", size=3) +
  labs(title = "Solar measurements in Vanuatu, 2020 ",
       subtitle = "Mean air temperature (C), max monthly rain (mm), mean cleaned GTI (V/m-2), mean DHI (V/m-2)",
       x = "Month") + theme_bw(base_size = 15)


#Individual plots 
library(patchwork)
p1 <- y1_plot_raw %>%
  ggplot() +
  geom_line(aes(x = month, y = mean_airtemp), color="red", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 ",
       y = "Mean air temperature (C)",
       x = "Month") + theme_bw(base_size = 10)

p2 <- y1_plot_raw %>%
  ggplot() +
  geom_line(aes(x = month, y = max_rain), color="blue", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 ",
       y = "Max monthly rain (mm)",
       x = "Month") + theme_bw(base_size = 10)

p3 <- y1_plot_raw %>%
  ggplot() +
  geom_line(aes(x = month, y = mean_gti_clean), color="coral1", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 ",
       y = "Mean cleaned GTI (V/m-2)",
       x = "Month") + theme_bw(base_size = 10)

p4 <- y1_plot_raw %>%
  ggplot() +
  geom_line(aes(x = month, y = mean_dhi_pyr), color="darkorchid4", size=1) +
  labs(title = "Solar measurements in Vanuatu, 2020 ",
       y = "Mean DHI (V/m-2)",
       x = "Month") + theme_bw(base_size = 10)


p1 + p2 + p3 + p4