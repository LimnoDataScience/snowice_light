library(scales)
library(tidyverse)
library(lubridate)
library(patchwork)

conv.layer = read.csv("output/interpolated_hourly_convective.csv") %>% 
  mutate(Time = as.POSIXct(Time))

all.dates <- seq(as.POSIXct('2018-11-01 00:00:00'), as.POSIXct('2021-05-31 00:00:00'), by = 3600)
idx.dates <- which(all.dates %in% conv.layer$Time)

add.dates <- data.frame('Time' = all.dates[-idx.dates],
                        'Buoydep' = NA,
                        'Convdep' = NA,
                        'energy' = NA,
                        'minT' = NA)

conv.layer <- rbind(conv.layer, add.dates) %>% arrange(Time)


conv.layer = conv.layer %>% 
  mutate(doy = yday(Time), year = year(Time), month = month(Time)) |> 
  mutate(winter = case_when(Time >= as.Date('2018-11-01') & Time < as.Date('2019-06-01') ~ 'winter18-19',
                            Time >= as.Date('2019-11-01') & Time < as.Date('2020-06-01') ~ 'winter19-20',
                            Time >= as.Date('2020-11-01') & Time < as.Date('2021-06-01') ~ 'winter20-21',
                            TRUE ~ NA))

winter.layer = conv.layer %>% filter(!is.na(winter)) %>%
  mutate(col =ifelse(winter == 'winter18-19', '#34cceb', ifelse(winter == 'winter19-20','#1b535e' ,'#dead1b'))) %>%
  mutate(fakeyear = if_else(month(as.Date(Time)) >= 08, `year<-`(as.Date(Time), 2019), `year<-`(as.Date(Time), 2020))) 

p1 <- ggplot(winter.layer) +
  geom_line(aes(fakeyear, energy,  col = winter), linewidth = 1.0) +
  scale_color_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_fill_manual(values = c('#34cceb','#1b535e','#dead1b'), name = 'Winter') +
  scale_x_date(date_breaks = 'month', date_minor_breaks = 'week',date_labels = '%b-%d') +
  ylim(5e7, 1.25e8) +
  labs(y = expression(paste("Internal energy (J ",m^-2,")")), x = "") +
  geom_vline(xintercept = winter.layer$fakeyear[ which(abs(winter.layer$minT - 4) < 0.003)], col = winter.layer$col[ which(abs(winter.layer$minT - 4) < 0.003)]) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 15, vjust = 0.5, hjust = 1), 
        axis.title.x = element_blank()); p1

ggsave(filename = 'figs/energy_hourly2.png', plot = p1, width = 15, height = 8, units = 'cm')
