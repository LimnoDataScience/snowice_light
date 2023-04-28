library(tidyverse)
library(lubridate)

setwd("C:/Documents and Settings/ladwi/Documents/Projects/R/snowice_light/")

raw <- read_csv("field/SSB_HoboClean.csv")

df <- raw %>%
  arrange(dateTime, Depth_m) %>%
  mutate(year = year(dateTime)) %>%
  select(dateTime, Depth_m, Temp_C, year)

ggplot(df ) +
  geom_line(aes(dateTime, Temp_C, group = as.factor(Depth_m), col = as.factor(Depth_m)))

ggplot(df %>% filter(Depth_m == 0.7)) +
  geom_line(aes(dateTime, Temp_C))
