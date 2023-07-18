library(tidyverse)
library(lubridate)
library(MetBrewer)

setwd("C:/Documents and Settings/ladwi/Documents/Projects/R/snowice_light/")

raw <- read_csv("field/SSB_HoboClean.csv")

df <- raw %>%
  arrange(dateTime, Depth_m) %>%
  mutate(year = year(dateTime),
         month = month(dateTime),
         hour = hour(dateTime)) %>%
  dplyr::select(dateTime, Depth_m, Temp_C, year, month, hour)

df$winter = NA
df$winter[df$month >= 12 & df$year == 2018] = 'winter18-19'
df$winter[df$month <= 3 & df$year == 2019] = 'winter18-19'
df$winter[df$month >= 12 & df$year == 2019] = 'winter19-20'
df$winter[df$month <= 3 & df$year == 2020] = 'winter19-20'
df$winter[df$month >= 12 & df$year == 2020] = 'winter20-21'
df$winter[df$month <= 3 & df$year == 2021] = 'winter20-21'


ggplot(df ) +
  geom_line(aes(dateTime, Temp_C, group = as.factor(Depth_m), col = as.factor(Depth_m)))

ggplot(df %>% filter(Depth_m == 0.7)) +
  geom_line(aes(dateTime, Temp_C))

ggplot(df %>% dplyr::filter(!is.na(winter)) %>% mutate(date = yday(dateTime), weekday = wday(dateTime)) %>% 
         dplyr::filter(weekday %in% c(1,4,7)) %>%
         group_by(winter, date, Depth_m) %>%
         summarise(Temp = mean(Temp_C))) +
  geom_path(aes(Temp, Depth_m, group = as.factor(date), col = (date))) +
  scale_y_reverse() +
  # theme(legend.position = "none") +
  scale_color_gradientn(colors = met.brewer("Hokusai1", n=100)) +
  facet_wrap(~ winter, ncol =  1)
